## Project 1

As of this commit, The Compy contatins a lexer and a parser for the language. The parser is able to correctly identify a valid grammatical structure and create a corresponding concrete syntax tree. The only known bug as of now is that the parser will sometimes produce errors which are unhelpful (but still technically correct).


#### Test 0
 
A valid test case (ran with `-b`); outputs a bracketed from of the concrete syntax tree

Source Program
```
{
  i = 1 + 2
}$
```

Output
```
['Program['Block['lbracket {]['StatementList['Statement['AssignStatement['id i]['assign =]['Expr['IntExpr['digit 1]['intop +]['Expr['IntExpr['digit 2]]]]]]]['StatementList['epsilon]]]['rbracket }]]['eop $]]
```

#### Test 1 

A valid program with forgotton `$` at the end

Source Program
```
{
  int i
  i = 0
  while ( i != 9 ) {
    if (i == 4) {
      print("halfway there")
    }
    i = 1 + i
  }
}
```

Output
```
Lexing completed successfully
Parser warning: forgotten '$' at end of program
Parsing completed successfully
```

#### Test 2

A basic parse error where the closing paren is missing from the if statement

Source Program
```
{
  if ( i == 0 {
    i = 1
  }
}$
```

Output
```
Lexing completed successfully
Parse Error: expecting ) got { on line 2
Parsing failed due to one or more errors
```

#### Test 3

A test case which fails lex on account of illegal characters `@`, `N` and `#`

Source Program
```
{
  i = @a
  # Not a comment
  print("hello cruel world")
}$
```

Output
```
Unrecognized token @ at line 2
Unrecognized token # at line 3
Unrecognized token N at line 3
Lexing failed due to one or more errors
```
