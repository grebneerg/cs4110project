# lang
a programming language

probably to be later renamed

## Building
This project is developed with ocaml `4.09.0` and opam `2.0.7`. You will need to have `ocamllex` and `ocamlyacc` installed.

To build, simply run `make`. This will generate a binary in the root directory called `lang`, which can be run with the path to a program file as the first argument. As currently configured, it will typecheck the program, and print the resulting type.

Two example programs are provided in the `programs/` directory.