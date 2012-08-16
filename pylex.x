{
module Main(main) where
import Text.Regex (subRegex, mkRegex)
import Data.List (mapAccumL)
import Data.Complex
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
$rawprefix = [rR]

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

@floatimagliteral = @floatliteral (j | J)
@intimagliteral = @intpart (j | J)

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

-- 0 start code is a logical line
-- line start code is within a logical line

tokens :-

                                        @linecontinuation                                       { skip }
       <0>                              \"                                                      { startLine $ begin shortString }
       <line>                           \"                                                      { begin shortString }
       <0>                              [rR]\'                                                  { startLine $ begin shortString' }
       <line>                           [rR]\'                                                  { begin shortString' }
       <0>                              [rR]\"                                                  { startLine $ begin shortString }
       <line>                           [rR]\"                                                  { begin shortString }
       <0>                              [uU]\'                                                  { startLine $ begin shortString' }
       <line>                           [uU]\'                                                  { begin shortString' }
       <0>                              [uU]\"                                                  { startLine $ begin shortString }
       <line>                           [uU]\"                                                  { begin shortString }
       <0>                              \'                                                      { startLine $ begin shortString' }
       <line>                           \'                                                      { begin shortString' }
       <0>                              \"\"\"                                                  { startLine $ begin longString }
       <line>                           \"\"\"                                                  { begin longString }
       <0>                              \'\'\'                                                  { startLine $ begin longString' }
       <line>                           \'\'\'                                                  { begin longString' }
       <0>                              @keyword                                                { startLine keyword }
       <line>                           @keyword                                                { keyword }
       <0>                              @identifier                                             { startLine identifier }
       <line>                           @identifier                                             { identifier }
       <0>                              @punctuation                                            { startLine punct }
       <line>                           @punctuation                                            { punct }
       <0>                              @decimalinteger                                         { startLine intLiteral }
       <line>                           @decimalinteger                                         { intLiteral }
       <0>                              @intimagliteral                                         { startLine intImagLiteral }
       <line>                           @intimagliteral                                         { intImagLiteral }
       <0>                              @floatimagliteral                                       { startLine floatImagLiteral }
       <line>                           @floatimagliteral                                       { floatImagLiteral }
       <0>                              @floatliteral                                           { startLine floatLiteral }
       <line>                           @floatliteral                                           { floatLiteral }
       <0>                              @longinteger                                            { startLine longLiteral }
       <line>                           @longinteger                                            { longLiteral }
       <shortString>                    \"                                                      { endStringLit line id }
       <shortString>                    @shortstringitemdouble                                  { stringLit }
       <shortString'>                   \'                                                      { endStringLit line id }
       <shortString'>                   @shortstringitemsingle                                  { stringLit }
       <longString,longString'>         @longstringitem                                         { stringLit }
       <longString>                     \"\"\"                                                  { endStringLit line id }
       <longString'>                    \'\'\'                                                  { endStringLit line id }
       <0,line>                         \#.*                                                    { skip }
       <line>                           \n                                                      { newline }
       <0>                              \n                                                      { clearCurrentIndent }
       <0>                              ^$spacetab+                                             { indent }
       <0,line>                         $white                                                  { skip }

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
     | ImagLit Double
     | LongLit Integer
     | FloatLit Double
     | IntLit Integer
     | Keyword String
     | NoOp
     deriving (Eq)

showToken :: Token -> String
showToken EndMarker = "(ENDMARKER)"
showToken (Punct str) = "(PUNCT \"" ++ str ++ "\")"
showToken (Id str) = "(ID \"" ++ str ++ "\")"
showToken (StringLit str) = "(LIT \"" ++ str ++ "\")"
showToken (FloatLit float) = "(LIT " ++ (show float) ++ ")"
showToken (LongLit long) = "(LIT " ++ (show long) ++ ")"
showToken (IntLit int) = "(LIT " ++ (show int) ++ ")"
showToken (ImagLit float) = "(LIT +" ++ (show float) ++ "i)"
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
    ust <- alexGetUserState
    alexSetUserState ust{currentIndent=len}
    return []

clearCurrentIndent _ _ = do
    ust <- alexGetUserState
    alexSetUserState ust{currentIndent=0}
    return []

startLine action ain@(_,_,input) len = do
    alexSetStartCode line
    ust@AlexUserState{indentDepth=indentStack,currentIndent=indent} <- alexGetUserState
    let (newStack, tokens) = popIndentStack indent indentStack []
    alexSetUserState ust{indentDepth=newStack}
    foo <- action ain len
    return (foo ++ tokens)

popIndentStack :: Int -> [Int] -> [Token] -> ([Int], [Token])
popIndentStack len indentStack accum =
               if len == (head indentStack)
               then (indentStack, accum)
               else if len < (head indentStack)
               then popIndentStack len (tail indentStack) (Dedent : accum)
               else ((len : indentStack), (Indent : accum))

newline (a,b,input) len = do
    ust@AlexUserState{punctStack=pStack} <- alexGetUserState
    alexSetUserState ust{currentIndent=0}
    if null pStack then do alexSetStartCode 0; return [Newline] else return []

longLiteral (_,_,input) len = return $ [LongLit (read (take len input))]

floatLiteral (_,_,input) len = return $ [FloatLit $ readFloatLiteral (take len input)]

readFloatLiteral :: String -> Double
readFloatLiteral ('.' : rest) = (read $ '0' : '.' : rest)
readFloatLiteral (start : '.' : 'e' : rest) = (read $ start : '.' : '0' : 'e' : rest)
readFloatLiteral x
  | (last x == '.') = (read $ x ++ "0")
  | otherwise = (read x)

intLiteral (_,_,input) len = return $ [IntLit (read (take len input))]

intImagLiteral (_,_,input) len = return $ [readIntImagLiteral (take (len-1) input)]
  where readIntImagLiteral x = ImagLit $ read x

floatImagLiteral (_,_,input) len = return $ [ImagLit $ readFloatLiteral (take (len-1) input)]

identifier (_,_,input) len = return [Id (take len input)]

keyword :: (AlexPosn, Char, String) -> Int -> Alex [Token]
keyword (_,_,input) len = return [Keyword (take len input)]

punct :: (AlexPosn, Char, String) -> Int -> Alex [Token]
punct (_,_,input) len =
    let str = (take len input) in
      case str of
        "(" -> openPunct $ Punct str
        "[" -> openPunct $ Punct str
        "{" -> openPunct $ Punct str
        ")" -> do closePunct ; return [Punct str]
        "]" -> do closePunct ; return [Punct str]
        "}" -> do closePunct ; return [Punct str]
        _ -> return [Punct str]

openPunct :: Token -> Alex [Token]
openPunct punct = do
    ust@AlexUserState{punctStack=pStack} <- alexGetUserState
    alexSetUserState ust{punctStack=(punct : pStack)}
    return [punct]


closePunct :: Alex ()
closePunct = do
    ust@AlexUserState{punctStack=pStack} <- alexGetUserState
    alexSetUserState ust{punctStack=(tail pStack)}


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
     indentDepth :: [Int],
     punctStack :: [Token],
     currentIndent :: Int,
     lazyInput :: String
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                        indentDepth = [0],
                        currentIndent = 0,
                        punctStack = [],
                        lazyInput = []
                    }

alexEOF = return [EndMarker]

scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                          [EndMarker] -> return $ (reverse (EndMarker:toks))
                          _ -> let foo = loop (tok ++ toks) in foo
  loop []

main = do
     s <- getContents
     case (scanner s) of
          Left message -> print message
          Right tokens -> mapM_ print tokens
}
