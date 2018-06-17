module PureLambda.Parser exposing (..)

import Combine exposing (..)

type E = EVar String
       | ELambda ( List String ) E
       | EName String
       | EApp ( List E )
       | EComment String


comment : Parser s String
comment =
    regex "\\-\\-[^\n]*"
    <?> "comment"


spaces : Parser s String
spaces =
    regex "[ \t\r\n]*"


whitespace : Parser s String
whitespace =
    comment <|> spaces <?> "whitespace"


token : Parser s res -> Parser s res
token =
    between whitespace whitespace


keyword : String -> Parser s String
keyword s =
    string s <* spaces


varRegexp : Parser s String
varRegexp =
    regex "[a-z][a-z_]*[\']*"


var : Parser s E
var =
    EVar <$> varRegexp <?> "variable"


lambda : Parser s E
lambda =
    lazy <|
        \() ->
            ELambda
            <$> (keyword "\\" *> many1 (token varRegexp) <* keyword ".")
            <*>  expr
            <?> "lambda"


nameRegexp : Parser s String
nameRegexp =
    regex "[A-Z][A-Z]*[\']*"


name : Parser s E
name =
    EName <$> nameRegexp <?> "identify"


stmt : Parser s ( List E )
stmt = many (expr <* (string ";"))


apply : Parser s E
apply =
    let
        append e =
            map (\es -> e :: es) (many1 term)
    in
    EApp <$> (term |> andThen append)

expr : Parser s E
expr =
    lazy <|
        \() ->
            apply <|> term


term : Parser s E
term =
    lazy <|
        \() ->
            choice [ token var
                   , token name
                   , token lambda
                   , token ( parens expr )
                   ]
