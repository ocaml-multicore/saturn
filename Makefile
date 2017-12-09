all:
	ocaml pkg/pkg.ml build --pinned false --debug true

clean:
	rm -rf _build
