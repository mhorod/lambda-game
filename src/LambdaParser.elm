module LambdaParser exposing (..)

import Lambda exposing (..)
import List
import List.Extra exposing (elemIndex, foldr1)
import Parser exposing (..)
import Set


type ParsedLambda
    = PVar String
    | PApp ParsedLambda ParsedLambda
    | PLam String ParsedLambda


parseLambda : String -> Maybe Lambda
parseLambda str =
    case run (termParser |. end) str of
        Err _ ->
            Nothing

        Ok parsed ->
            buildLambda parsed


varNameParser : Parser String
varNameParser =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


varParser : Parser ParsedLambda
varParser =
    succeed PVar
        |= varNameParser


lamParser : Parser ParsedLambda
lamParser =
    succeed PLam
        |. symbol "\\"
        |. spaces
        |= varNameParser
        |. spaces
        |. symbol "."
        |. spaces
        |= lazy (\_ -> termParser)


parParser : Parser ParsedLambda
parParser =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> termParser)
        |. spaces
        |. symbol ")"


termParser : Parser ParsedLambda
termParser =
    let
        termHelper : List ParsedLambda -> Parser (Step (List ParsedLambda) ParsedLambda)
        termHelper lambdas =
            oneOf
                [ succeed (\lambda -> Loop (lambda :: lambdas))
                    |. spaces
                    |= oneOf [ varParser, lamParser, parParser ]
                    |. spaces
                , lazy
                    (\_ ->
                        case foldr1 (\right left -> PApp left right) lambdas of
                            Just term ->
                                succeed (Done term)

                            Nothing ->
                                problem "lambda terms cannot be empty nor contain empty pairs of parentheses"
                    )
                ]
    in
    loop [] termHelper


buildLambda : ParsedLambda -> Maybe Lambda
buildLambda parsed =
    let
        buildLambda_ : List VarName -> ParsedLambda -> Maybe Lambda
        buildLambda_ varStack p =
            case p of
                PVar s ->
                    Maybe.map Var (elemIndex s varStack)

                PLam v body ->
                    if List.member v varStack then
                        Nothing

                    else
                        Maybe.map (Lam v)
                            (buildLambda_ (v :: varStack) body)

                PApp left right ->
                    Maybe.map2 App
                        (buildLambda_ varStack left)
                        (buildLambda_ varStack right)
    in
    buildLambda_ [] parsed
