module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Maybe exposing (withDefault)
import String exposing (left, right)


type alias VarName =
    String


type Lambda
    = Var Int
    | App Lambda Lambda
    | Lam VarName Lambda


type ReductionResult
    = Reduced Lambda
    | Unreduced Lambda


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


mapReductionResult : (Lambda -> Lambda) -> ReductionResult -> ReductionResult
mapReductionResult f r =
    case r of
        Reduced lambda ->
            Reduced (f lambda)

        Unreduced lambda ->
            Unreduced (f lambda)


toString : Lambda -> Maybe String
toString lambda =
    toStringWithStack lambda []


toStringWithStack : Lambda -> List VarName -> Maybe String
toStringWithStack lambda stack =
    case lambda of
        Var t ->
            index t stack |> Maybe.map (\name -> name ++ String.fromInt t)

        App f x ->
            case ( toStringWithStack f stack, toStringWithStack x stack ) of
                ( Just fString, Just xString ) ->
                    Just (addParensL f fString ++ " " ++ addParensR x xString)

                _ ->
                    Nothing

        Lam name f ->
            Maybe.map (\x -> "λ" ++ name ++ ". " ++ x) (toStringWithStack f (name :: stack))


toDebugString : Lambda -> String
toDebugString lambda =
    toDebugStringWithStack lambda []


toDebugStringWithStack : Lambda -> List VarName -> String
toDebugStringWithStack lambda stack =
    case lambda of
        Var t ->
            index t stack |> Maybe.map (\name -> name ++ String.fromInt t) |> withDefault (String.fromInt t)

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
findBetas lambda =
    findBetasAcc lambda [] []


findBetasAcc : Lambda -> Path -> List Path -> List Path
findBetasAcc lambda path paths =
    case lambda of
        Var _ ->
            paths

        Lam _ body ->
            findBetasAcc body (Down :: path) paths

        App left right ->
            (if isLam left then
                -- Since we add directions at the beginning of the list
                -- we have to reverse the path at the end
                [ List.reverse path ]

             else
                []
            )
                ++ findBetasAcc left (Left :: path) paths
                ++ findBetasAcc right (Right :: path) paths


isLam : Lambda -> Bool
isLam lambda =
    case lambda of
        Lam _ _ ->
            True

        _ ->
            False


betaReduce_ : Lambda -> ReductionResult
betaReduce_ lambda =
    case lambda of
        App (Lam _ body) x ->
            Reduced (substAtDepth body x 0)

        _ ->
            Unreduced lambda


betaReduce : Lambda -> Lambda
betaReduce lambda =
    getReducedLambda (betaReduce_ lambda)


getReducedLambda : ReductionResult -> Lambda
getReducedLambda result =
    case result of
        Reduced lambda ->
            lambda

        Unreduced lambda ->
            lambda


betaReducePath : Lambda -> Path -> Maybe Lambda
betaReducePath lambda path =
    case path of
        [] ->
            Just (betaReduce lambda)

        dir :: dirs ->
            case lambda of
                Var _ ->
                    Nothing

                Lam _ body ->
                    if dir == Down then
                        betaReducePath body dirs

                    else
                        Nothing

                App left right ->
                    case dir of
                        Down ->
                            Nothing

                        Left ->
                            betaReducePath left dirs

                        Right ->
                            betaReducePath right dirs


substAtDepth lambda arg depth =
    case lambda of
        Var v ->
            case compare v depth of
                LT ->
                    lambda

                EQ ->
                    increaseFreeVariables depth arg 0

                GT ->
                    Var (v - 1)

        App f x ->
            App (substAtDepth f arg depth) (substAtDepth x arg depth)

        Lam v f ->
            Lam v (substAtDepth f arg (depth + 1))


increaseFreeVariables : Int -> Lambda -> Int -> Lambda
increaseFreeVariables increase lambda depth =
    case lambda of
        Var v ->
            if v < depth then
                lambda

            else
                Var (v + increase)

        App f x ->
            App (increaseFreeVariables increase f depth) (increaseFreeVariables increase x depth)

        Lam v f ->
            Lam v (increaseFreeVariables increase f (depth + 1))


betaReduceDfsFirst : Lambda -> ReductionResult
betaReduceDfsFirst lambda =
    case lambda of
        Var _ ->
            Unreduced lambda

        Lam x body ->
            betaReduceDfsFirst body |> mapReductionResult (\l -> Lam x l)

        App left right ->
            case betaReduce_ lambda of
                Reduced r ->
                    Reduced r

                Unreduced _ ->
                    case betaReduceDfsFirst left of
                        Reduced r ->
                            Reduced (App r right)

                        Unreduced _ ->
                            case betaReduceDfsFirst right of
                                Reduced r ->
                                    Reduced (App left r)

                                Unreduced _ ->
                                    Unreduced lambda


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


type alias BetaReducer =
    Lambda -> ReductionResult


betaReduceMultipleTimes : BetaReducer -> Int -> Lambda -> FullReductionResult
betaReduceMultipleTimes reducer n lambda =
    if n == 0 then
        Unterminated []

    else
        let
            reduced =
                reducer lambda
        in
        (case reduced of
            Reduced r ->
                betaReduceMultipleTimes reducer (n - 1) r

            Unreduced _ ->
                Terminated []
        )
            |> mapFullReductionResult (\r -> lambda :: r)


sus =
    Lam "x" (App (App (Var 0) (Var 0)) (Var 0))


sus2 =
    App sus sus


main =
    div []
        [ findBetas ksk |> Debug.toString |> text
        , case betaReduceMultipleTimes betaReduceDfsFirst 5 skk of
            Terminated list ->
                div [] [ text "yay", displayReductionSteps list ]

            Unterminated list ->
                div [] [ text "nay", displayReductionSteps list ]
        ]


displayReductionSteps : List Lambda -> Html msg
displayReductionSteps lambdas =
    lambdas
        |> List.map (toString >> withDefault "error" >> text >> (\t -> div [] [ t ]))
        |> div [ style "display" "flex", style "flex-direction" "column" ]
