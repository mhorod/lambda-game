module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)
import Lambda exposing (..)
import LambdaParser exposing (parseLambda)
import List
import Maybe.Extra


type alias Model =
    { userInput : String
    }


type Msg
    = UpdateUserInput String


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init () =
    ( { userInput = "" }, Cmd.none )


view model =
    { title = "lambda game"
    , body =
        let
            parsed =
                parseLambda model.userInput
        in
        [ div []
            [ input [ placeholder "Enter your lambda here!", value model.userInput, onInput UpdateUserInput ] []
            , div []
                (text (parsedLambdaToString parsed)
                    :: List.map reductionResult (Maybe.Extra.toList parsed)
                )
            ]
        ]
    }


parsedLambdaToString : Maybe Lambda -> String
parsedLambdaToString maybeLambda =
    case maybeLambda of
        Nothing ->
            "Ooops, your lambda has a syntax error :("

        Just lambda ->
            case lambdaToString lambda of
                Just str ->
                    str

                Nothing ->
                    "Oops, the lambda is invalid :(("


update : Msg -> Model -> ( Model, Cmd Msg )
update (UpdateUserInput str) model =
    ( { userInput = str }, Cmd.none )


displayReductionSteps : List Lambda -> Html msg
displayReductionSteps lambdas =
    div [ style "display" "flex", style "flex-direction" "column" ]
        (lambdas
            |> List.map lambdaToString
            |> List.map (Maybe.withDefault "error")
            |> List.map (\t -> div [] [ text t ])
        )


reductionResult : Lambda -> Html msg
reductionResult lambda =
    div []
        [ findBetas lambda |> Debug.toString |> text
        , case fullyBetaReduce 50 lambda of
            Terminated list ->
                div [] [ text "yay", displayReductionSteps list ]

            Unterminated list ->
                div [] [ text "nay", displayReductionSteps list ]
        ]
