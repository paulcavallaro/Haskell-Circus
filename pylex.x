{
module Main(main) where
import Text.Regex (subRegex, mkRegex)
}
%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

$longstringchar = [.\n]
@stringescapeseq = \\.
$shortstringcharsingle = [^ \\ \n ']
$shortstringchardouble = [^ \\ \n \"]
@longstringitem = ($longstringchar | @stringescapeseq)
@shortstringitemsingle = ($shortstringcharsingle | @stringescapeseq)
@shortstringitemdouble = ($shortstringchardouble | @stringescapeseq)
@shortstring = (' @shortstringitemsingle* ' | \" @shortstringitemdouble* \")
@longstring = (\'\'\' @longstringitem* \'\'\' | \"\"\" @longstringitem* \"\"\")
$stringprefix = [rR]
@rawlongstringliteral = $stringprefix @longstring
@longstringliteral = @longstring
@rawshortstringliteral = $stringprefix @shortstring
@shortstringliteral = @shortstring

$shortbyteschardouble = [^ \\ \n \" ]
$shortbytescharsingle = [^ \\ \n \' ]
@longbyteschar = [^ \\]
@bytesescapeseq = \\ .
@shortbytesitemsingle = $shortbytescharsingle | bytesescapeseq
@shortbytesitemdouble = $shortbyteschardouble | bytesescapeseq
@longbytesitem = longbyteschar | bytesescapeseq
@shortbytes = \'@shortbytesitemsingle*\' | \"shortbytesitemdouble*\"
@longbytes = \'\'\' @longbytesitem* \'\'\' | \"\"\" @longbytesitem* \"\"\"
@bytesprefix = (b | B | br | Br | bR | BR)
@bytesliteralshort = @bytesprefix @shortbytes
@bytesliterallong = @bytesprefix @longbytes

$nonzerodigit = [1-9]
$octdigit = [0-7]
@hexdigit = digit | [a-f] | [A-F]
$bindigit = [0 1]
@octinteger = 0 (o | O) $octdigit+
@hexinteger = 0 (x | X) @hexdigit+
@bininteger = 0 (b | B) $bindigit+
@decimalinteger = $nonzerodigit $digit* | 0+
@integerliteral = @decimalinteger | @octinteger | @hexinteger | @bininteger

@intpart = $digit+
@fraction =  \. $digit+
@exponent =  (e | E) (\+ | \-)? $digit+
@pointfloat = @intpart? @fraction | @intpart \.
@exponentfloat = (@intpart | @pointfloat) @exponent
@floatliteral = @pointfloat | @exponentfloat

@imagliteral = (@floatliteral | @intpart) (j | J)

@keyword = False|None|True|and|as|assert|break|class|continue|def|
           del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|
           nonlocal|not|or|pass|raise|return|try|while|with|yield

@identifier = ($alpha|_) ($alpha | $digit | _)*

@operator = \+ | \- | \* | \*\* | \/ | \/\/ | \% | \<\< | \>\> | \& |
            \| | \^ | \~ | \< | \> | \<\= | \>\= | \=\= | \!\=

@delimiter = \( | \) | \[ | \] | \{ | \} | \, | \: | \. | \; | \@ | \= |
             \+\= | \-\= | \*\= | \/\= | \/\/\= | \%\= | \&\= | \!\= | \^\= |
             \>\>\= | \<\<\= | \*\*\=

@punctuation = (@operator | @delimiter)

@linecontinuation = \\ \n

$spacetab = [\t \ ]

tokens :-

       <0>                              \"                                                      { begin shortString }
       <0>                              \'                                                      { begin shortString' }
       <0>                              \"\"\"                                                  { begin longString }
       <0>                              \'\'\'                                                  { begin longString' }
       <0>                              @identifier                                             { identifier }
       <0>                              @keyword                                                { keyword }
       <0>                              @punctuation                                            { punct }
       <0>                              @linecontinuation                                       { skip }
       <0>                              ^$white*\n                                              { skip }
       <0>                              ^$white*\#.*\n                                          { skip }
       <shortString>                    \"                                                      { endStringLit 0 (\s -> subRegex (mkRegex "\\\\\"") s "\"") }
       <shortString>                    @shortstringitemdouble                                  { stringLit }
       <shortString'>                   \'                                                      { endStringLit 0 (\s -> subRegex (mkRegex "\\\\'") s "'") }
       <shortString'>                   @shortstringitemsingle                                  { stringLit }
       <shortString,shortString'>       \\\n                                                    { skip }
       <longString,longString'>         \\\n                                                    { skip }
       <longString,longString'>         @longstringitem                                         { stringLit }
       <longString>                     \"\"\"                                                  { endStringLit 0 id }
       <longString'>                    \'\'\'                                                  { endStringLit 0 id }
       <0>                              .                                                       { skip }
       <0>                              \n                                                      { newline }

{

-- The token type:

data Token =
       Indent
     | Dedent
     | Newline
     | EndMarker
     | Punct Char
     | Id String
     | Lit String
     | Keyword String
     deriving (Eq,Show)

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ust = Alex $ \s -> Right (s{alex_ust=ust}, ())

indent (_,_,input) len = do
       ust@AlexUserState{lexerIndentDepth=indentStack} <- alexGetUserState
       let (newStack, tokens) = popIndentStack len indentStack []
       alexSetUserState ust{lexerIndentDepth=newStack}
       return tokens

popIndentStack :: Int -> [Int] -> [Token] -> ([Int], [Token])
popIndentStack len indentStack accum =
               if len == (head indentStack)
               then (indentStack, accum)
               else if len < (head indentStack)
               then popIndentStack len (tail indentStack) (Dedent : accum)
               else ((len : indentStack), (Indent : accum))


newline (_,_,_) _ = return $ [Newline]



intLit (_,_,input) len = return $ [Lit (take len input)]
floatLit (_,_,input) len = return $ [Lit (take len input)]
rawLongStringLit (_,_,input) len = return $ [Lit $ drop 4 $ reverse $ drop 3 $ reverse (take len input)]
longStringLit (_,_,input) len = return $ [Lit $ drop 3 $ reverse $ drop 3 $ reverse (take len input)]
rawShortStringLit (_,_,input) len = return $ [Lit $ drop 2 $ init (take len input)]

identifier (_,_,input) len = return [Id (take len input)]

keyword :: (AlexPosn, Char, String) -> Int -> Alex [Token]
keyword (_,_,input) len = return [Keyword (take len input)]

punct (_,_,input) _ = return [Punct (head input)]

stringLit :: (AlexPosn, Char, String) -> Int -> Alex [Token]
stringLit (_,_,input) len = do
          ust@AlexUserState{lazyInput=accum} <- alexGetUserState
          alexSetUserState ust{lazyInput=(accum ++ (take len input))}
          return []

endStringLit code transform (_,_,input) len = do
             ust@AlexUserState{lazyInput=accum} <- alexGetUserState
             alexSetUserState ust{lazyInput=""}
             alexSetStartCode code
             return [Lit (transform accum)]


bytesShortLit (_,_,input) len = return $ [Lit $ drop 2 $ init (take len input)]
bytesLongLit (_,_,input) len = return $ [Lit $ drop 4 $ reverse $ drop 3 $ reverse (take len input)]
imagLit (_,_,input) len = return $ [Lit (take len input)]

data AlexUserState = AlexUserState {
     lexerIndentDepth :: [Int],
     lazyInput :: String,
     stateCodeStack :: [Int]
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                        lexerIndentDepth = [0],
                        lazyInput = [],
                        stateCodeStack = [0]
                    }

alexEOF = return [EndMarker]

scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                          [EndMarker] -> return (reverse toks)
                          _ -> let foo = loop (tok ++ toks) in foo
  loop []

printToken :: Token -> IO ()
printToken token = do
     case token of
          Newline -> putStrLn ""
          Punct c -> putStr $ c : " "
          Id s -> putStr (s ++ " ")
          Keyword s -> putStr (s ++ " ")
          Lit s -> putStr (s ++ " ")
          _ -> putStr $ (show token) ++ " "

main = do
     s <- getContents
     case (scanner s) of
          Left message -> print message
          Right tokens -> print tokens
}