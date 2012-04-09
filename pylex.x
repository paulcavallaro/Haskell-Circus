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
@longinteger = @integerliteral (l | L)

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
       <0>                              @keyword                                                { keyword }
       <0>                              @identifier                                             { identifier }
       <0>                              @punctuation                                            { punct }
       <0>                              @linecontinuation                                       { skip }
       <0>                              ^$white*\n                                              { skip }
       <0>                              ^$white*\#.*\n                                          { skip }
       <0>                              @decimalinteger                                         { intLiteral }
       <0>                              @imagliteral                                            { stringLiteral }
       <0>                              @floatliteral                                           { stringLiteral }
       <0>                              @longinteger                                            { stringLiteral }
       <shortString>                    \"                                                      { endStringLit 0 id }
       <shortString>                    @shortstringitemdouble                                  { stringLit }
       <shortString'>                   \'                                                      { endStringLit 0 id }
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
     | Punct String
     | Id String
     | StringLit String
     | IntLit Integer
     | Keyword String

showToken :: Token -> String
showToken EndMarker = "(ENDMARKER)"
showToken (Punct str) = "(PUNCT \"" ++ str ++ "\")"
showToken (Id str) = "(ID \"" ++ str ++ "\")"
showToken (StringLit str) = "(LIT \"" ++ str ++ "\")"
showToken (IntLit int) = "(LIT " ++ (show int) ++ ")"
showToken (Keyword str) = "(KEYWORD " ++ str ++ ")"
showToken Indent = "(INDENT)"
showToken Dedent = "(DEDENT)"
showToken Newline = "(NEWLINE)"

instance Show Token where show = showToken

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



stringLiteral (_,_,input) len = return $ [StringLit (take len input)]

intLiteral (_,_,input) len = return $ [IntLit (read (take len input))]

identifier (_,_,input) len = return [Id (take len input)]

keyword :: (AlexPosn, Char, String) -> Int -> Alex [Token]
keyword (_,_,input) len = return [Keyword (take len input)]

punct (_,_,input) len = return [Punct (take len input)]

stringLit :: (AlexPosn, Char, String) -> Int -> Alex [Token]
stringLit (_,_,input) len = do
          ust@AlexUserState{lazyInput=accum} <- alexGetUserState
          alexSetUserState ust{lazyInput=(accum ++ (take len input))}
          return []

endStringLit code transform (_,_,input) len = do
             ust@AlexUserState{lazyInput=accum} <- alexGetUserState
             alexSetUserState ust{lazyInput=""}
             alexSetStartCode code
             return [StringLit (transform accum)]

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
                          [EndMarker] -> return (reverse (EndMarker:toks))
                          _ -> let foo = loop (tok ++ toks) in foo
  loop []

main = do
     s <- getContents
     case (scanner s) of
          Left message -> print message
          Right tokens -> mapM_ print tokens
}
