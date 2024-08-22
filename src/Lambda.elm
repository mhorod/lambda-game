module Lambda exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra exposing (getAt)
import Maybe
import Maybe.Extra exposing (orElseLazy)
import String


type alias VarName =
    String


type Lambda
    = Var Int
    | App Lambda Lambda
    | Lam VarName Lambda


lambdaToString : Lambda -> Maybe String
lambdaToString lambda =
    let
        toStringWithStack : Lambda -> List VarName -> Maybe String
        toStringWithStack l stack =
            case l of
                Var t ->
                    getAt t stack |> Maybe.map (\name -> name ++ String.fromInt t)

                App f x ->
                    case ( toStringWithStack f stack, toStringWithStack x stack ) of
                        ( Just fString, Just xString ) ->
                            Just (addParensL f fString ++ " " ++ addParensR x xString)

                        _ ->
                            Nothing

                Lam name f ->
                    Maybe.map (\x -> "λ" ++ name ++ ". " ++ x) (toStringWithStack f (name :: stack))
    in
    toStringWithStack lambda []


toDebugString : Lambda -> String
toDebugString lambda =
    let
        toDebugStringWithStack : Lambda -> List VarName -> String
        toDebugStringWithStack l stack =
            case l of
                Var t ->
                    getAt t stack |> Maybe.map (\name -> name ++ String.fromInt t) |> Maybe.withDefault (String.fromInt t)

                App f x ->
                    let
                        left =
                            toDebugStringWithStack f stack

                        right =
                            toDebugStringWithStack x stack
                    in
                    addParensL f left ++ " " ++ addParensR x right

                Lam name f ->
                    "λ" ++ name ++ ". " ++ toDebugStringWithStack f (name :: stack)
    in
    toDebugStringWithStack lambda []


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


type alias Location =
    List Direction


isBetaReducible : Lambda -> Bool
isBetaReducible lambda =
    case lambda of
        App (Lam _ _) _ ->
            True

        _ ->
            False


tryBetaReduce : Lambda -> Maybe Lambda
tryBetaReduce lambda =
    let
        adjustFreeVars : Int -> Lambda -> Int -> Lambda
        adjustFreeVars increase lam depth =
            case lam of
                Var v ->
                    if v < depth then
                        Var v

                    else
                        Var (v + increase)

                App left right ->
                    App
                        (adjustFreeVars increase left depth)
                        (adjustFreeVars increase right depth)

                Lam name body ->
                    Lam name
                        (adjustFreeVars increase body (depth + 1))

        substituteTopVar : Int -> Lambda -> Lambda -> Lambda
        substituteTopVar depth arg lam =
            case lam of
                Var v ->
                    case compare v depth of
                        LT ->
                            Var v

                        EQ ->
                            adjustFreeVars depth arg 0

                        GT ->
                            Var (v - 1)

                App left right ->
                    App
                        (substituteTopVar depth arg left)
                        (substituteTopVar depth arg right)

                Lam name body ->
                    Lam name
                        (substituteTopVar (depth + 1) arg body)
    in
    case lambda of
        App (Lam _ body) arg ->
            Just (substituteTopVar 0 arg body)

        _ ->
            Nothing


findBetas : Lambda -> List Location
findBetas lambda =
    case lambda of
        App left right ->
            (if isBetaReducible lambda then
                [ [] ]

             else
                []
            )
                ++ (findBetas left |> List.map (\p -> Left :: p))
                ++ (findBetas right |> List.map (\p -> Right :: p))

        Lam _ body ->
            findBetas body |> List.map (\p -> Down :: p)

        Var _ ->
            []


betaReduceAt : Location -> Lambda -> Maybe Lambda
betaReduceAt location lambda =
    case ( location, lambda ) of
        ( [], _ ) ->
            tryBetaReduce lambda

        ( Down :: path, Lam _ body ) ->
            betaReduceAt path body

        ( Left :: path, App left _ ) ->
            betaReduceAt path left

        ( Right :: path, App _ right ) ->
            betaReduceAt path right

        _ ->
            Nothing


betaReduceAnywhere : Lambda -> Maybe Lambda
betaReduceAnywhere lambda =
    case lambda of
        Var _ ->
            Nothing

        Lam name body ->
            betaReduceAnywhere body |> Maybe.map (\b -> Lam name b)

        App left right ->
            tryBetaReduce lambda
                |> orElseLazy (\_ -> betaReduceAnywhere left |> Maybe.map (\l -> App l right))
                |> orElseLazy (\_ -> betaReduceAnywhere right |> Maybe.map (\r -> App left r))


type FullReductionResult
    = Terminated (List Lambda)
    | Unterminated (List Lambda)


mapFullReductionResult : (List Lambda -> List Lambda) -> FullReductionResult -> FullReductionResult
mapFullReductionResult f result =
    case result of
        Terminated xs ->
            Terminated (f xs)

        Unterminated xs ->
            Unterminated (f xs)


fullyBetaReduce : Int -> Lambda -> FullReductionResult
fullyBetaReduce fuel lambda =
    mapFullReductionResult (\ls -> lambda :: ls) <|
        case ( fuel, betaReduceAnywhere lambda ) of
            ( 0, Just _ ) ->
                Unterminated []

            ( _, Just reduced ) ->
                fullyBetaReduce (fuel - 1) reduced

            ( _, Nothing ) ->
                Terminated []


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


sus =
    Lam "x" (App (App (Var 0) (Var 0)) (Var 0))


sus2 =
    App sus sus
