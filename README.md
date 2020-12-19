# lang
a programming language

yes, our language is called is lang

## Building
This project is developed with ocaml `4.09.0` and opam `2.0.7`. You will need to have `ocamllex` and `ocamlyacc` installed.

To build, simply run `make`. This will generate a binary in the root directory called `lang`, which can be run with the path to a program file as the first argument. As currently configured, it can either typecheck or run the program, and print the resulting type or result.

## Demo 
The two demo files we have provided are in the `programs/` directory. 

The first demo file we have is showcase1. This file showcases our small standard library that supports lists and math functions. In addition, it shows our system's ability to import other files and support for recursive functions.

The second demo file we have is showcase2. This file showcases our interpreter's support for pattern matching, records, and our math library's gcd function. 

Both of these files can be typechecked with `./lang check programs/showcase<x>` and run with `./lang run programs/showcase<x>`. Be sure to run these from the root directory of the project, since imports are currently relative to the user's working directory, not the directory of the file.