module Main exposing (..)

import Html exposing (text)


type alias VarName =
    String


type Lambda
    = Var Int
    | App Lambda Lambda
    | Lam VarName Lambda


type ReductionResult
    = Reduced Lambda
    | Unreduced Lambda


toString : Lambda -> Maybe String
toString lambda =
    toStringWithStack lambda []


toStringWithStack : Lambda -> List VarName -> Maybe String
toStringWithStack lambda stack =
    case lambda of
        Var t ->
            index t stack

        App f x ->
            case ( toStringWithStack f stack, toStringWithStack x stack ) of
                ( Just fString, Just xString ) ->
                    Just (addParensL f fString ++ " " ++ addParensR x xString)

                _ ->
                    Nothing

        Lam name f ->
            Maybe.map (\x -> "λ" ++ name ++ ". " ++ x) (toStringWithStack f (name :: stack))


index : Int -> List a -> Maybe a
index n xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            if n == 0 then
                Just y

            else
                index (n - 1) ys


addParensL : Lambda -> String -> String
addParensL lambda rendered =
    case lambda of
        Lam _ _ ->
            "(" ++ rendered ++ ")"

        _ ->
            rendered


addParensR : Lambda -> String -> String
addParensR lambda rendered =
    case lambda of
        Var _ ->
            rendered

        _ ->
            "(" ++ rendered ++ ")"


type Direction
    = Left
    | Down
    | Right


type alias Path =
    List Direction



findBetas : Lambda -> List Path


betaReduce : Lambda -> Lambda
betaReduce lambda =
    case lambda of
        App f x ->
            substLam f x

        _ ->
            lambda


substLam lambda arg =
    case lambda of
        Lam _ body ->
            substAtDepth body arg 0

        _ ->
            lambda


substAtDepth lambda arg depth =
    case lambda of
        Var v ->
            case compare v depth of
                LT ->
                    lambda

                EQ ->
                    arg

                GT ->
                    Var (v - 1)

        App f x ->
            App (substAtDepth f arg depth) (substAtDepth x arg depth)

        Lam v f ->
            Lam v (substAtDepth f arg (depth + 1))


naszaLambda =
    Lam "x" (Lam "y" (App (Var 1) (Var 0)))


kCombinator =
    Lam "x" (Lam "y" (Var 1))


sCombinator =
    Lam "x" (Lam "y" (Lam "z" (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))))


skk =
    App (App sCombinator kCombinator) kCombinator


ksk =
    App kCombinator (App sCombinator kCombinator)


main =
    case toString (betaReduce (betaReduce ksk)) of
        Just s ->
            text s

        Nothing ->
            text ":("
