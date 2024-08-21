module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Lambda exposing (..)
import LambdaParser exposing (parseLambda)


type alias Model =
    { userInput : String
    , parsed : Maybe Lambda
    }


type Msg
    = UpdateUserInput String


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


emptyModel =
    { userInput = "", parsed = Nothing }


init () =
    ( emptyModel, Cmd.none )


view model =
    { title = "lambda game"
    , body =
        [ div []
            [ input [ placeholder "Enter your lambda here!", value model.userInput, onInput UpdateUserInput ] []
            , div []
                (text (parsedLambdaToString model.parsed)
                    :: Maybe.withDefault [] (model.parsed |> Maybe.map reductionResult |> Maybe.map (\x -> [ x ]))
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
            let
                maybeStr =
                    lambdaToString lambda
            in
            case maybeStr of
                Just str ->
                    str

                Nothing ->
                    "Oops, the lambda is invalid :(("


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUserInput str ->
            ( { userInput = str, parsed = parseLambda str }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


displayReductionSteps : List Lambda -> Html msg
displayReductionSteps lambdas =
    lambdas
        |> List.map (lambdaToString >> Maybe.withDefault "error" >> text >> (\t -> div [] [ t ]))
        |> div [ style "display" "flex", style "flex-direction" "column" ]


reductionResult lambda =
    div []
        [ findBetas lambda |> Debug.toString |> text
        , case fullyBetaReduce 50 lambda of
            Terminated list ->
                div [] [ text "yay", displayReductionSteps list ]

            Unterminated list ->
                div [] [ text "nay", displayReductionSteps list ]
        ]
