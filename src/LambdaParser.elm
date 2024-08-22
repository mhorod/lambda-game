module LambdaParser exposing (..)

import Lambda exposing (..)
import List exposing (foldl, foldr, length, member, tail)
import Parser exposing (..)
import String exposing (indexes)


type ParsedLambda
    = PVar String
    | PApps (List ParsedLambda)
    | PLam String ParsedLambda


parseLambda : String -> Maybe Lambda
parseLambda str =
    let
        parsingResult =
            run lambdaParser str
    in
    case parsingResult of
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
    succeed PVar |= varNameParser


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
    succeed PApps |= loop [] lambdaParserHelper


lambdaParserHelper : List ParsedLambda -> Parser (Step (List ParsedLambda) (List ParsedLambda))
lambdaParserHelper revLambdas =
    oneOf
        [ succeed (\lambda -> Loop (lambda :: revLambdas)) |= termParser |. spaces
        , succeed () |> map (\_ -> Done (List.reverse revLambdas))
        ]


buildLambda : ParsedLambda -> Maybe Lambda
buildLambda parsed =
    buildLambda_ parsed []


buildLambda_ : ParsedLambda -> List VarName -> Maybe Lambda
buildLambda_ parsed varStack =
    let
        _ =
            Debug.log "aaa" parsed
    in
    case parsed of
        PVar s ->
            index s varStack |> Maybe.map (\i -> Var i)

        PLam v body ->
            if member v varStack then
                Nothing

            else
                buildLambda_ body (v :: varStack) |> Maybe.map (Lam v)

        PApps xs ->
            List.map (\x -> buildLambda_ x varStack) xs |> transform |> Maybe.andThen flattenApplications


transform : List (Maybe a) -> Maybe (List a)
transform list =
    case list of
        [] ->
            Just []

        x :: xs ->
            Maybe.map2 (\y ys -> y :: ys) x (transform xs)


flattenApplications : List Lambda -> Maybe Lambda
flattenApplications lambdas =
    case lambdas of
        [] ->
            Nothing

        x :: [] ->
            Just x

        x :: y :: ys ->
            Just (flattenApplications_ (App x y) ys)


flattenApplications_ : Lambda -> List Lambda -> Lambda
flattenApplications_ lambda lambdas =
    case lambdas of
        [] ->
            lambda

        x :: xs ->
            flattenApplications_ (App lambda x) xs


index : a -> List a -> Maybe Int
index x xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            if x == y then
                Just 0

            else
                index x ys |> Maybe.map (\i -> i + 1)
