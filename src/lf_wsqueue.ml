module type S = sig
  type 'a t
  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val steal : 'a t -> 'a option
end

module CArray = struct

  type 'a t = 'a Atomic.t array

  let create s v = Array.init s (fun _ -> Atomic.make v)

  let size t = Array.length t

  let get t i = Atomic.get t.(i mod (size t))

  let put t i v = Atomic.set t.(i mod (size t)) v

  let grow t top bottom = 
    let s = size t in
    let ns = 2 * s in
    let v = get t 0 in
    let out = create ns v in
    for i = top to bottom do
      out.(i mod (size out)) <- t.(i mod (size t))
    done;
    out

  let shrink t top bottom =
    let s = size t in
    let ns = s / 2 in
    let v = get t 0 in
    let out = create ns v in
    for i = top to bottom do
      out.(i mod (size out)) <- t.(i mod (size t))
    done;
    out 

end

module M : S = struct
  let min_size = 16
  let shrink_const = 3

  type 'a t = {
    top : int Atomic.t;
    bottom : int Atomic.t;
    tab : 'a CArray.t Atomic.t
  }

  let create () = {
    top = Atomic.make 0;
    bottom = Atomic.make 0;
    tab = Atomic.make (CArray.create min_size (Obj.magic ()))
  }

  let is_empty q = 
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    b - t <= 0

  let size q =
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    assert (b - t >= 0);
    b - t

  let push q v =
    let b = Atomic.get q.bottom in
    let t = Atomic.get q.top in
    let a = Atomic.get q.tab in
    let size = b - t in
    if size >= CArray.size a - 1 then
      Atomic.set q.tab (CArray.grow a t b);
    CArray.put a b v;
    Atomic.set q.bottom (b + 1)

  let pop q =
    let b = Atomic.get q.bottom - 1 in
    Atomic.decr q.bottom;
    let t = Atomic.get q.top in
    let a = Atomic.get q.tab in
    let size = b - t in
    if size < 0 then begin
      Atomic.set q.bottom t;
      None
    end else
      let out = Some (CArray.get a b) in
      if size > 0 then begin
        if (CArray.size a) / shrink_const > size then
          Atomic.set q.tab (CArray.shrink a t b);
          out
        end else begin
          Atomic.set q.bottom (t + 1);
          if (Atomic.compare_and_set q.top t (t + 1)) then
            out
          else 
            None
        end

    let steal q =
      let wait = Kcas.Backoff.create () in
      let rec loop () =
        let t = Atomic.get q.top in
        let b = Atomic.get q.bottom in
        let a = Atomic.get q.tab in
        let size = b - t in
        if size <= 0 then 
          None
        else
          let out = Some (CArray.get a t) in
          if Atomic.compare_and_set q.top t (t + 1) then
            out
          else begin
            Kcas.Backoff.once wait;
            loop ()
          end
      in loop ()

end