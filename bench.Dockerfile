FROM ocaml/opam:debian-ocaml-5.2
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
WORKDIR bench-dir
RUN sudo chown opam .
COPY *.opam ./
RUN opam remote add origin https://github.com/ocaml/opam-repository.git && \
    opam update
RUN opam pin -yn --with-version=dev .
RUN opam install -y --deps-only --with-test .
COPY . ./
RUN opam exec -- dune build --release bench/main.exe
