module LambdaParser exposing (..)

import Lambda exposing (..)
import List
import List.Extra exposing (elemIndex)
import Maybe.Extra exposing (combine)
import Parser exposing (..)


type ParsedLambda
    = PVar String
    | PApps (List ParsedLambda)
    | PLam String ParsedLambda


parseLambda : String -> Maybe Lambda
parseLambda str =
    case run lambdaParser str of
        Err _ ->
            Nothing

        Ok parsed ->
            buildLambda parsed


varNameParser : Parser String
varNameParser =
    getChompedString <|
        succeed ()
            |. chompIf Char.isLower
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')


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
        |= lazy (\_ -> lambdaParser)


parensParser : Parser ParsedLambda
parensParser =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> lambdaParser)
        |. spaces
        |. symbol ")"


termParser : Parser ParsedLambda
termParser =
    oneOf
        [ varParser
        , lamParser
        , parensParser
        ]


flattenApplications : List Lambda -> Maybe Lambda
flattenApplications lambdas =
    let
        flattenApplications_ : Lambda -> List Lambda -> Lambda
        flattenApplications_ l ls =
            case ls of
                [] ->
                    l

                x :: xs ->
                    flattenApplications_ (App l x) xs
    in
    case lambdas of
        [] ->
            Nothing

        x :: [] ->
            Just x

        x :: y :: zs ->
            Just (flattenApplications_ (App x y) zs)


lambdaParser : Parser ParsedLambda
lambdaParser =
    let
        lambdaParser_ : List ParsedLambda -> Parser (Step (List ParsedLambda) (List ParsedLambda))
        lambdaParser_ revLambdas =
            oneOf
                [ succeed (\lambda -> Loop (lambda :: revLambdas))
                    |= termParser
                    |. spaces
                , succeed (Done (List.reverse revLambdas))
                ]
    in
    succeed PApps
        |= loop [] lambdaParser_


buildLambda : ParsedLambda -> Maybe Lambda
buildLambda parsed =
    let
        buildLambda_ : List VarName -> ParsedLambda -> Maybe Lambda
        buildLambda_ varStack p =
            case p of
                PVar s ->
                    elemIndex s varStack
                        |> Maybe.map Var

                PLam v body ->
                    if List.member v varStack then
                        Nothing

                    else
                        buildLambda_ (v :: varStack) body
                            |> Maybe.map (Lam v)

                PApps xs ->
                    List.map (buildLambda_ varStack) xs
                        |> combine
                        |> Maybe.andThen flattenApplications
    in
    buildLambda_ [] parsed
