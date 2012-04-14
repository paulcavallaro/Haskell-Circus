module Main (main) where
import System.Environment
import Control.Monad
import Prelude hiding (getContents, lines, null)
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Text (split, unpack, lines, strip, null)
import Data.Text.IO (getContents)

data PyVal = Id String
           | Keyword String
           | Punct String
           | String String
           | Integer Integer
           | Float Double
           | Unicode String

showVal :: PyVal -> String
showVal (Id contents) = contents
showVal (Keyword contents) = contents
showVal (Punct contents) = contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Unicode contents) = "u\"" ++ contents ++ "\""
showVal (Float num) = show num
showVal (Integer num) = show num

instance Show PyVal where show = showVal

parseLit :: Parser PyVal
parseLit = do
  char '('
  string "LIT "
  val <- try parseString <|> try parseInt
  char ')'
  return val
  
parsePunct :: Parser PyVal
parsePunct = do
  char '('
  string "PUNCT "
  char '"'
  x <- many1 $ noneOf ['"']
  char '"'
  char ')'
  return $ Punct x

parseString :: Parser PyVal
parseString = do
  char '"'
  x <- many (parseEscaped <|> noneOf ['"'])
  char '"'
  return $ String x

parseInt :: Parser PyVal
parseInt = liftM (Integer . read) $ many1 digit

parseEscaped = do
  char '\\'
  c <- anyChar
  case c of
    '\\' -> return '\\'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    '"' -> return '"'

parseToken :: Parser PyVal
parseToken = try parseLit
         <|> try parsePunct

readToken :: String -> PyVal
readToken input = case parse parseToken "python" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = do
  x <- getContents 
  mapM_ print $ map (readToken . unpack) $ filter (not . null . strip) $ lines x