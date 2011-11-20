# Haskell Circus

A Python 3 Compiler written in Haskell

## Usage

Need to have Alex installed and run the following to set up the lexer

    $ alex Tokens.x
    $ ghc Tokens.hs

Then you can run the lexer from the command line with:

    $ cat sample.py | ./Tokens