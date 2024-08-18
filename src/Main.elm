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


mapReductionResult : (Lambda -> Lambda) -> ReductionResult -> ReductionResult
mapReductionResult f r =
    case r of
        Reduced lambda ->
            Reduced (f lambda)

        Unreduced lambda ->
            Unreduced (f lambda)


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


lambdaToString : Lambda -> Maybe String
lambdaToString lambda =
    let
        toStringWithStack : Lambda -> List VarName -> Maybe String
        toStringWithStack l stack =
            case l of
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
    in
    toStringWithStack lambda []


toDebugString : Lambda -> String
toDebugString lambda =
    let
        toDebugStringWithStack : Lambda -> List VarName -> String
        toDebugStringWithStack l stack =
            case l of
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


isBetaReducible : Lambda -> Bool
isBetaReducible lambda =
    case lambda of
        App (Lam _ _) _ ->
            True

        _ ->
            False


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


betaReduceAt : Lambda -> Location -> Maybe Lambda
betaReduceAt lambda location =
    case location of
        [] ->
            Just (betaReduce lambda)

        dir :: dirs ->
            case lambda of
                Var _ ->
                    Nothing

                Lam _ body ->
                    if dir == Down then
                        betaReduceAt body dirs

                    else
                        Nothing

                App left right ->
                    case dir of
                        Down ->
                            Nothing

                        Left ->
                            betaReduceAt left dirs

                        Right ->
                            betaReduceAt right dirs


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


type alias BetaReducer =
    Lambda -> ReductionResult


fullyReduce : Int -> BetaReducer -> Lambda -> FullReductionResult
fullyReduce fuel reducer lambda =
    (case ( fuel, reducer lambda ) of
        ( 0, Reduced _ ) ->
            Unterminated []

        ( _, Reduced r ) ->
            fullyReduce (fuel - 1) reducer r

        ( _, Unreduced _ ) ->
            Terminated []
    )
        |> mapFullReductionResult (\ls -> lambda :: ls)


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


displayReductionSteps : List Lambda -> Html msg
displayReductionSteps lambdas =
    lambdas
        |> List.map (lambdaToString >> withDefault "error" >> text >> (\t -> div [] [ t ]))
        |> div [ style "display" "flex", style "flex-direction" "column" ]


main =
    div []
        [ findBetas ksk |> Debug.toString |> text
        , case fullyReduce 4 betaReduceDfsFirst skk of
            Terminated list ->
                div [] [ text "yay", displayReductionSteps list ]

            Unterminated list ->
                div [] [ text "nay", displayReductionSteps list ]
        ]
