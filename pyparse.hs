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
           | Newline
           | EOF
           | Indent
           | Dedent

showVal :: PyVal -> String
showVal (Id contents) = contents
showVal (Keyword contents) = contents
showVal (Punct contents) = contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Unicode contents) = "u\"" ++ contents ++ "\""
showVal (Float num) = show num
showVal (Integer num) = show num
showVal Indent = "(INDENT)"
showVal Newline = "(NEWLINE)"
showVal Dedent = "(DEDENT)"
showVal EOF = "(ENDMARKER)"

instance Show PyVal where show = showVal

parseMarker :: Parser PyVal
parseMarker = do
  marker <- try parseIndent <|> try parseEOF <|> try parseDedent <|> try parseNewline
  return marker

parseIndent = do string "INDENT"; return Indent
parseDedent = do string "DEDENT"; return Dedent
parseNewline = do string "NEWLINE"; return Newline
parseEOF = do string "ENDMARKER"; return EOF

parseLit :: Parser PyVal
parseLit = do
  string "LIT "
  val <- try parseString <|> try parseInt
  return val

parsePunct :: Parser PyVal
parsePunct = do
  string "PUNCT "
  char '"'
  x <- many1 $ noneOf ['"']
  char '"'
  return $ Punct x

parseId :: Parser PyVal
parseId = do
  string "ID "
  char '"'
  x <- many1 $ noneOf ['"']
  char '"'
  return $ Id x

parseKeyword :: Parser PyVal
parseKeyword = do
  string "KEYWORD "
  x <- many1 $ noneOf ")"
  return $ Keyword x

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
parseToken = do
  char '('
  tok <- try parseLit
     <|> try parsePunct
     <|> try parseId
     <|> try parseKeyword
     <|> try parseMarker
  char ')'
  return tok

readToken :: String -> PyVal
readToken input = case parse parseToken "python" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

prettyPrintVals :: [PyVal] -> String -> Int -> String
prettyPrintVals vals accum iLevel =
  case (head vals) of
    EOF -> accum
    Indent -> prettyPrintVals (tail vals) (accum ++ "\t") (iLevel + 1)
    Dedent -> prettyPrintVals (tail vals) (init accum) (iLevel - 1)
    Newline -> prettyPrintVals (tail vals) (accum ++ "\n" ++ (take iLevel (repeat '\t'))) iLevel
    kw@(Keyword _) -> prettyPrintVals (tail vals) (accum ++ (show kw) ++ " ") iLevel
    _ -> prettyPrintVals (tail vals) (accum ++ (show (head vals))) iLevel

main :: IO ()
main = do
  x <- getContents
  let vals = map (readToken . unpack) $ filter (not . null . strip) $ lines x in
    putStrLn $ prettyPrintVals vals "" 0