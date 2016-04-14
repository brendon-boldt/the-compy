## Project 2

Project two implelements abstract syntax tree creation and semantic analysis. This includes scope checking, type checking, variable use, and symbol table construction.

Notes:

- When looking at the AST, it will appear that the node 'type will have a child int. All this means is that the node of type 'type has the value int, that is, "int" is _not_ a separate node
- intop (+) nodes (among others) are displayed Lisp-style (+ 1 2) since the intop's value is + while its children are the operands

#### Test 0
 
A test case with valid assignemnt which generates a warning for an uninitialized variable (ran with `-s`); outputs the symbol table

Source Program
```
{
  int x
  int y
	x = 4
	{
		string x
		x = "x"
	}
  x = y
}$
```

Output
```
Lexing completed successfully
Parsing completed successfully
Semantic Warning: variable y was not initialized; used at line 9

SymbolTable.0
y -> 'int
x -> 'int

SymbolTable.0.0
x -> 'string
```

#### Test 1
 
A test case showing the AST (`-a`); this case will generate a warning for an unused variable

Source Program
```
{
  boolean b
  b = true
  if ( b == ( b != true )) {
    string x
  }
}$
```

Output
```
{
  boolean b
  b = true
  if ( b == ( b != true )) {
    string x
  }
}$
```

#### Test 2
 
A test case showing invalid assignment, copmarison, and intop use

Source Program
```
{
  int x
  boolean b
  string x
  b = 2
  while ( s == x ) {
    x = 2 + b
  }
  print(s)
}$
```

Output
```
Lexing completed successfully
Parsing completed successfully
Semantic Error: redeclaration of x at line 4
Semantic Error: int cannot be assigned to b of type boolean; found on line 5
Semantic Error: variable s was not declared; found at line 6
Semantic Error: cannot add type int to type boolean; found on line 7
Semantic Error: variable s was not declared; found at line 9
Semantic Warning: variable x was not initialized; used at line 6
```
