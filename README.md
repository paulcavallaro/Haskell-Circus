# Haskell Circus

A Python 3 Compiler written in Haskell

## Usage

Need to have ghc installed

    λ ghc pylex.hs
    λ ghc pyparse.hs

Then you can run the lexer from the command line with:

    λ cat hello_world.py | ./pylex

..and run the parser from the command line with:

    λ cat hello_world.py | ./pylex | ./pyparse

..and see if your local installation of python3 can intepret it with:

    λ cat hello_world.py | ./pylex | ./pyparse | python3
    Hello, World!