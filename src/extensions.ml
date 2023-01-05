module Array = struct
  include Array

  (** Inserts an element at position index within the array, shifting all elements after it to the right.
      @raise Invalid_argument if index is outside the range 0 to length a. *)
  let insert a index elem =
    let len = length a in
    if index < 0 || index > len then invalid_arg "Array.insert";
    let new_a = make (len + 1) elem in
    blit a 0 new_a 0 index;
    blit a index new_a (index + 1) (len - index);
    new_a

  (** Remove an element at position index within the array, shifting all elements after it to the left.
      @raise Invalid_argument if index is outside the range 0 to length a. *)
  let remove a index =
    let len = length a in
    match index with
    | 0 -> sub a 1 (len - 1)
    | n when n = len - 1 -> sub a 0 (len - 1)
    | n when n < 0 || n >= len -> invalid_arg "Array.remove"
    | n ->
        let new_a = make (len - 1) a.(0) in
        blit a 0 new_a 0 n;
        blit a (n + 1) new_a n (len - n - 1);
        new_a

  (** [filter_map f a] applies [f] to all elements in [a], filtering [None] results,
      and returning the array of [Some] results, along with a sorted list of the indices of deleted elements. *)
  let filter_map f a =
    let rec aux i l_mapped l_filtered =
      if i < 0 then (of_list l_mapped, l_filtered)
      else
        match f a.(i) with
        | Some x -> aux (i - 1) (x :: l_mapped) l_filtered
        | None -> aux (i - 1) l_mapped (i :: l_filtered)
    in
    aux (length a - 1) [] []
end

let popcount n =
  let rec aux n count =
    if n = 0 then count
    else
      let shifted = n lsr 1 in
      if 1 land n <> 0 then aux shifted (count + 1) else aux shifted count
  in
  aux n 0
