# Haskell While Interpreter
A While Programming Language Interpreter Implemented in Haskell.

## Running the Interpreter
The command below will run the interpreter with the given program file and variable values.

```
cabal run while-interpreter -- <program_file> --vars <variable_values>
```

 - `<program_file>` : Path to the while program to be interpreted.
 - `--vars <variable_values>` : Specifies the initial values of the provided variables. `<variable_values>` must of the form `x=5,y=10,longname=30`.

## Samples
Inside the `example_programs` directory, there are some sample while programs that can be run with the interpreter.
A comment at the top of each file describes the program and its expected output.
Below are some examples of how to run the interpreter with the sample programs.

### Collatz
```
cabal run while-interpreter -- example_programs/collatz.while -v input=12
```

### Factorial
```
cabal run while-interpreter -- example_programs/factorial.while -v x=5
```

### GCD
```
cabal run while-interpreter -- example_programs/gcd.while --vars x=14,y=35
```

### Fibonacci
```
cabal run while-interpreter -- example_programs/fibonacci.while --vars n=7
```

### Primes
```
cabal run while-interpreter -- example_programs/primes.while --vars n=10
```

## Testing
The while-interpreter-lib can be tested with
```
cabal test
```

----
Created as final project for UNM CS Advanced Declarative Programming Fall 2024
