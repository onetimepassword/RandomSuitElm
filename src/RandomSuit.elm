module RandomSuit exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (decodeValue, int)
import Random exposing (Generator, Seed)
import Browser
import Platform.Cmd as Cmd


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Types


type alias Model =
    { suit : Maybe Suit
    , seed : Seed
    }


type Msg
    = PickRandomSuit


type Suit
    = Hearts
    | Diamonds
    | Spades
    | Clubs



-- Init


init : Decode.Value -> ( Model, Cmd Msg )
init json =
    let
        initialSeed =
            case decodeValue int json of
                Ok seed ->
                    seed
                Err err ->
                    -1
        mdl = 
            Model Nothing (Random.initialSeed initialSeed)
    in
    ( mdl, Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickRandomSuit ->
            let
                ( suit, nextSeed ) =
                    Random.step suitGenerator model.seed
                mdl = 
                    Model (Just suit) nextSeed
            in
                ( mdl, Cmd.none )


numToSuit : Int -> Suit
numToSuit num =
    case num of
        0 ->
            Hearts

        1 ->
            Diamonds

        2 ->
            Spades

        _ ->
            Clubs


suitGenerator : Generator Suit
suitGenerator =
    Random.map numToSuit (Random.int 0 3)



-- Views


view : Model -> Html Msg
view model =
    div
        []
        [ button
            [ onClick PickRandomSuit ]
            [ text "Random Suit" ]
        , suitView model.suit
        ]


suitView : Maybe Suit -> Html Msg
suitView suit_ =
    suit_
        |> Maybe.map withSuit
        |> Maybe.withDefault noSuit


noSuit : Html Msg
noSuit =
    text "No suit! Click the button!"


withSuit : Suit -> Html Msg
withSuit suit =
    case suit of
        Hearts ->
            text "❤ Hearts"

        Diamonds ->
            text "♦️️ Diamonds"

        Spades ->
            text "♠ Spades"

        Clubs ->
            text "️♣️ Clubs"
