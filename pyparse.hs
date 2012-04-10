module Main (main) where
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data PyVal = Id String
           | String String
           | Integer Integer
           | Float Double
           | 

parseToken :: Parser PyVal
parseToken