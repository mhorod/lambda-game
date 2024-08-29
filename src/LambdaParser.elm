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


termParser : Parser ParsedLambda
termParser =
    oneOf
        [ varParser
        , lamParser
        , parensParser
        ]


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


lambdaParser : Parser ParsedLambda
lambdaParser =
    succeed PApps
        |= loop [] lambdaParserHelper


lambdaParserHelper : List ParsedLambda -> Parser (Step (List ParsedLambda) (List ParsedLambda))
lambdaParserHelper revLambdas =
    oneOf
        [ succeed (\lambda -> Loop (lambda :: revLambdas))
            |= termParser
            |. spaces
        , succeed (Done (List.reverse revLambdas))
        ]


buildLambda : ParsedLambda -> Maybe Lambda
buildLambda parsed =
    buildLambda_ [] parsed


buildLambda_ : List VarName -> ParsedLambda -> Maybe Lambda
buildLambda_ varStack parsed =
    case parsed of
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


flattenApplications : List Lambda -> Maybe Lambda
flattenApplications lambdas =
    case lambdas of
        [] ->
            Nothing

        x :: [] ->
            Just x

        x :: y :: zs ->
            Just (flattenApplications_ (App x y) zs)


flattenApplications_ : Lambda -> List Lambda -> Lambda
flattenApplications_ lambda lambdas =
    case lambdas of
        [] ->
            lambda

        x :: xs ->
            flattenApplications_ (App lambda x) xs
