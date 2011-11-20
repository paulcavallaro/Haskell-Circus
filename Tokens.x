{
    module Main(main) where
}
%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

$longstringchar = [^\\]
@stringescapeseq = \\ "."
$shortstringcharsingle = [^\\ \n ']
$shortstringchardouble = [^\\ \n "]
@longstringitem = ($longstringchar | @stringescapeseq)
@shortstringitemsingle = ($shortstringcharsingle | @stringescapeseq)
@shortstringitemdouble = ($shortstringchardouble | @stringescapeseq)
@shortstring = ("'" @shortstringitemsingle* "'" | "\"" @shortstringitemdouble* "\"")
@longstring = ("'''" @longstringitem* "'''" | "\"\"\"" @longstringitem* "\"\"\"")
$stringprefix = [rR]
@stringliteral = $stringprefix?(shortstring | longstring)

tokens :-

    $white+                                                     ;
    "#".*                                                       ;
    [\:\=\+\-\*\/\(\)\[\]\{\}]                                  { \s -> Punct (head s) }
    ("False"|"None"|"True"|"and"|"as"|
    "assert"|"break"|"class"|"continue"|"def"|
    "del"|"elif"|"else"|"except"|"finally"|"for"|
    "from"|"global"|"if"|"import"|"in"|"is"|"lambda"|
    "nonlocal"|"not"|"or"|"pass"|"raise"|"return"|
    "try"|"while"|"with"|"yield")                               { \s -> Keyword s }
    [$alpha \_][$alpha $digit \_]*                              { \s -> Id s }
    [$digit]+L?                                                 { \s -> Lit s }
    @stringliteral                                              { \s -> Lit s }

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
