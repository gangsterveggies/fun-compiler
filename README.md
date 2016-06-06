# FUN compiler to SECD

This compiler implements the following features (the bold represent
the ones implemented by me):

 - The only primitive data are integers with primitives +, - and * 
 - If-zero-then-else conditional;
 - Single argument lambdas; currying can be used for 
   multiple arguments;
 - Single let-definitions; multiple definitions must be 
   translated to nested lets;
 - Single recursive function definitions (for simplicity);
 - **Pairs and generic tuples**;
 - **Lists, variants and records**;
 - **Data notation to represent obejcts (similar to haskell)**;
 - **Outputs records by printing each field**;
 - **Case guards (with pattern matching)**;

# Running the code

There is a Makefile that compiles the compiler code and the SECD
interpreter. To compile a FUN code into the SECD bytecode do the
following:

    ./Main <inputfile>.fun <outputfile>.secd

To interpret the resulting bytecode do:

    ./secd < <outputfile>.secd
    
# Examples

In the `examples/` directory there are 9 examples, all documented with
commentaries. Each one tests a different property of the compiler to
showcase its behavior.
