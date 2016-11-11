module Translate exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Char exposing (isDigit, toCode)
import String exposing (filter)
import Result exposing (Result, toMaybe)
import Maybe.Extra exposing (combine)
import BinaryBase64 exposing (encode, decode)
import Bitwise exposing (..)


main =
    Html.beginnerProgram { model = init, view = view, update = update }



-- MODEL


type alias Model =
    { decoded : String
    , encoded : String
    }


init : Model
init =
    { decoded = ""
    , encoded = ""
    }



-- UPDATE


type Msg
    = NoOp
    | Encode (Maybe (List Int))
    | Encoded String
    | Decode String
    | Decoded String


encodeMaybe : Maybe (List Int) -> String
encodeMaybe ml =
    case ml of
        Nothing ->
            ""

        Just l ->
            encode l


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Encode ml ->
            { model | encoded = encodeMaybe ml }

        Encoded s ->
            { model | encoded = s }

        Decode s ->
            { model | decoded = toStr <| decode s }

        Decoded s ->
            { model | decoded = s }


fromStr : String -> Maybe (List Int)
fromStr =
    String.split ","
        >> List.map toByte
        >> List.map toMaybe
        >> combine



-- parse the string and return an OK Int result if it's between 0 and 255


toByte : String -> Result String Int
toByte s =
    let
        r =
            String.toInt s
    in
        case r of
            Ok i ->
                if (i >= 0 && i <= 255) then
                    r
                else
                    Err "Out of range"

            _ ->
                r


digitOrComma : Char -> Bool
digitOrComma c =
    isDigit c || toCode c == 44


toStr : Result String (List Int) -> String
toStr r =
    case r of
        Err text ->
            text

        Ok ls ->
            ls
                |> toString
                |> String.filter digitOrComma


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Ints to encode less (in range 0-255) e.g. 1,44,78,255"
            , value model.decoded
            , onInput Decoded
            , myStyle
            ]
            []
        , button [ onClick (Encode <| fromStr model.decoded) ] [ text "encode" ]
        , input
            [ placeholder "text to decode"
            , value model.encoded
            , onInput Encoded
            , myStyle
            ]
            []
        , button [ onClick (Decode model.encoded) ] [ text "decode" ]
        ]


myStyle : Attribute Msg
myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]
