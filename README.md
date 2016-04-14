The Compy
============

### Language Information

The grammar of the language can be found [here](http://labouseur.com/courses/compilers/grammar.pdf).

### Build Instructions (Linux)

[Download and install](http://www.scala-lang.org/download/install.html) Scala.

Run `git clone https://github.com/brendon-boldt/the-compy.git`

To build, run `make` (or `make clean` to clean class files)

### Run Instructions

To run, run `scala compy.Main OPTIONS FILENAME`

To see a visual representation of the bracketed strings, copy and paste the text [here](http://mshang.ca/syntree/).

Output the concrete syntax tree with `-c` or `--cst`

Output the abstract syntax tree with `-a` or `--ast`

Output the symbol table with `-s` or `--st`

Verbose output can be specified with `-v` or `--verbose`
