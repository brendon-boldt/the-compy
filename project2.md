## Project 2

- When looking at the AST, it will appear that the node 'type will have a child int. All this means is that the node of type 'type has the value int, that is, "int" is _not_ a separate node
- intop (+) nodes are displayed Lisp-style (+ 1 2) since the intop's value is + while its children are the operands

- ~~Scope-check with the tokens or the CST. (I suggest using the CST.)~~
- ~~Build an Abstract Syntax Tree from the tokens or the CST. Display it after successful lex, parse, and semantic analysis.~~
- While you are scope-checking (or afterwards) build a symbol table (perhaps using the 
CST or AST) of IDs that includes their name, data type, scope, position in the source 
code, and anything else you think might be important. 
- Type-check the source code using the AST. Catch . . . 
  - ~~undeclared identifiers,~~ 
  - ~~redeclared identifiers in the same scope,~~
  - type mismatches, 
  - and anything else that might go wrong.  
- ~~Issue warnings about declared but unused identifiers.~~
- ~~Issue warnings about use of uninitialized variables but do not treat them as errors.~~ 
- Include verbose output functionality that traces the semantic analysis stages, including scope checking, the construction of the symbol table, and type checking actions.  
- When you detect an error, report it in helpful detail including where it was found
