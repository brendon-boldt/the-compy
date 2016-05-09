## Project 3

The part everybody has been waiting for: code generation! Try using `-o` or `--oast` to view the optimized AST.

#### Test 0
 
This represents a nice case of one program failing semantic analysis and one passing with the passing one moving on to codegen.

#### Test 1
 
Now that we have the semantic error removed we can see that everything works properly. Unless you run out of memory that is :/ Those darn expressions just take up too many opcodes.


#### Test 2
 
Finally, a proper test of the expressions code generation.


#### Test 3
 
A simple test of nested if statements


#### Test 4
 
A bit more complex with an if statement combined with a while statement.


#### Test 5
 
This tests my compiler's ability to compare strings.
