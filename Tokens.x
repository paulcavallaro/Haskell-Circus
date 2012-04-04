{
    module Main(main) where
}
%wrapper "monadUserState"

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

    $white+                                                     { skip }
    \#.*                                                        { skip }
    @operator                                                   { punct }
    @delimiter                                                  { punct }
    @keyword                                                    { keyword }
    [$alpha \_][$alpha $digit \_]*                              { idToken }
    @integerliteral                                             { intLit }
    @floatliteral                                               { floatLit }
    @rawlongstringliteral                                       { rawLongStringLit }
    @longstringliteral                                          { longStringLit }
    @rawshortstringliteral                                      { rawShortStringLit }
    @shortstringliteral                                         { shortStringLit }
    @bytesliteralshort                                          { bytesShortLit }
    @bytesliterallong                                           { bytesLongLit }
    @imagliteral                                                { imagLit }


{

-- The token type:

data Token =
       Return
     | EOF
     | Punct Char
     | Id String
     | Lit String
     | Keyword String
     deriving (Eq,Show)

punct (_,_,input) _ = return $ Punct (head input)
keyword (_,_,input) len = return $ Keyword (take len input)
idToken (_,_,input) len = return $ Id (take len input)
intLit (_,_,input) len = return $ Lit (take len input)
floatLit (_,_,input) len = return $ Lit (take len input)
rawLongStringLit (_,_,input) len = return $ Lit $ drop 4 $ reverse $ drop 3 $ reverse (take len input)
longStringLit (_,_,input) len = return $ Lit $ drop 3 $ reverse $ drop 3 $ reverse (take len input)
rawShortStringLit (_,_,input) len = return $ Lit $ drop 2 $ init (take len input)
shortStringLit (_,_,input) len = return $ Lit $ tail $ init (take len input)
bytesShortLit (_,_,input) len = return $ Lit $ drop 2 $ init (take len input)
bytesLongLit (_,_,input) len = return $ Lit $ drop 4 $ reverse $ drop 3 $ reverse (take len input)
imagLit (_,_,input) len = return $ Lit (take len input)



data AlexUserState = AlexUserState {
     lexerIndentDepth :: [Int]
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                        lexerIndentDepth = [0]
                    }

alexEOF = return EOF

scanner str = runAlex str $ do
  let loop i = do tok <- alexMonadScan; 
               	  if tok == EOF
	       	     	then return i
			else do let i' = i+1 in i' `seq` loop i'
  loop 0

main = do
     s <- getContents
     print (scanner s)
}
