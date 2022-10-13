.PHONY: all

all:
	dune build @install @runtest ./stress/stress.exe

coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	bisect-ppx-report summary	