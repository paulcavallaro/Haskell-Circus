{
    module Main(main) where
}
%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

$longstringchar = [^ \\]
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

@operator = \+ | \- | \* | \*\* | \/ | \/\/ | \% | \<\< | \>\> | \& |
            \| | \^ | \~ | \< | \> | \<\= | \>\= | \=\= | \!\=

@delimiter = \( | \) | \[ | \] | \{ | \} | \, | \: | \. | \; | \@ | \= |
             \+\= | \-\= | \*\= | \/\= | \/\/\= | \%\= | \&\= | \!\= | \^\= |
             \>\>\= | \<\<\= | \*\*\=

tokens :-

    $white+                                                     ;
    \#.*                                                        ;
    @operator                                                   { \s -> Punct (head s) }
    @delimiter                                                  { \s -> Punct (head s) }
    @keyword                                                    { \s -> Keyword s }
    [$alpha \_][$alpha $digit \_]*                              { \s -> Id s }
    @integerliteral                                             { \s -> Lit s }
    @floatliteral                                               { \s -> Lit s }
    @rawlongstringliteral                                       { \s -> Lit $ drop 4 $ reverse $ drop 3 $ reverse s }
    @longstringliteral                                          { \s -> Lit $ drop 3 $ reverse $ drop 3 $ reverse s }
    @rawshortstringliteral                                      { \s -> Lit $ drop 2 $ init s }
    @shortstringliteral                                         { \s -> Lit $ tail $ init s }
    @bytesliteralshort                                          { \s -> Lit $ drop 2 $ init s }
    @bytesliterallong                                           { \s -> Lit $ drop 4 $ reverse $ drop 3 $ reverse s }
    @imagliteral                                                { \s -> Lit s }
{

-- Each action has type :: String -> Token

-- The token type:

data Token =
     Return                     |
     Punct Char                 |
     Id String                  |
     Lit String                 |
     Keyword String
     deriving (Eq,Show)

main = do
     s <- getContents
     print (alexScanTokens s)
}
