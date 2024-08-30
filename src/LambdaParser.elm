module LambdaParser exposing (..)

import Lambda exposing (..)
import List
import List.Extra exposing (elemIndex, foldr1)
import Parser exposing (..)
import Set


varNameParser : Parser VarName
varNameParser =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


varParser : List VarName -> Parser Lambda
varParser varsInScope =
    let
        lookItUpInScope : VarName -> Parser Lambda
        lookItUpInScope name =
            case elemIndex name varsInScope of
                Nothing ->
                    problem (Debug.todo "explain that variables have to be in the scope")

                Just v ->
                    succeed (Var v)
    in
    varNameParser
        |> andThen lookItUpInScope


lamParser : List VarName -> Parser Lambda
lamParser varsInScope =
    succeed identity
        |. symbol "\\"
        |. spaces
        |= varNameParser
        |> andThen
            (\name ->
                if List.member name varsInScope then
                    problem (Debug.todo "explain that name collisions are ambigious")

                else
                    succeed (Lam name)
                        |. spaces
                        |. symbol "."
                        |. spaces
                        |= lazy (\_ -> termParser (name :: varsInScope))
            )


parParser : List VarName -> Parser Lambda
parParser varsInScope =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> termParser varsInScope)
        |. spaces
        |. symbol ")"


termParser : List VarName -> Parser Lambda
termParser varsInScope =
    let
        stepInTheLoop : List Lambda -> Parser (Step (List Lambda) Lambda)
        stepInTheLoop lambdas =
            oneOf
                [ succeed (\lambda -> Loop (lambda :: lambdas))
                    |. spaces
                    |= oneOf
                        [ varParser varsInScope
                        , lamParser varsInScope
                        , parParser varsInScope
                        ]
                    |. spaces
                , lazy (\_ -> endOfTheLoop lambdas)
                ]

        endOfTheLoop : List Lambda -> Parser (Step (List Lambda) Lambda)
        endOfTheLoop lambdas =
            case foldr1 (\right left -> App left right) lambdas of
                Just term ->
                    succeed (Done term)

                Nothing ->
                    problem "lambda terms cannot be empty nor contain empty pairs of parentheses"
    in
    loop [] stepInTheLoop


parseLambda : String -> Maybe Lambda
parseLambda str =
    case run (termParser [] |. end) str of
        Err _ ->
            Nothing

        Ok parsed ->
            Just parsed
