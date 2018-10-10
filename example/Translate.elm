module Translate exposing (Model, Msg(..), digitOrComma, encodeMaybe, fromStr, init, listIntToString, listIntToStringHelp, main, myStyle, toByte, toStr, update, view)

import BinaryBase64 exposing (decode, encode)
import Bitwise exposing (..)
import Browser
import Char exposing (isDigit, toCode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Maybe.Extra exposing (combine)
import Result exposing (Result, toMaybe)
import String exposing (filter)


main =
    Browser.sandbox
        { init = init, view = view, update = update }



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
        Just i ->
            if i >= 0 && i <= 255 then
                Result.Ok i

            else
                Err "Out of range"

        Nothing ->
            Result.Err "Unspecified integer parsing error"


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
                |> listIntToString
                |> String.filter digitOrComma


listIntToString : List Int -> String
listIntToString l =
    case l of
        [] ->
            "[]"

        [ i ] ->
            "[" ++ String.fromInt i ++ "]"

        ls ->
            "[" ++ listIntToStringHelp ls ++ "]"


listIntToStringHelp : List Int -> String
listIntToStringHelp l =
    case l of
        [] ->
            "Damnit, how???"

        i :: [] ->
            String.fromInt i

        i :: is ->
            (String.fromInt i ++ ", ") ++ listIntToStringHelp is


view : Model -> Html Msg
view model =
    div []
        [ input
            ([ placeholder "Ints to encode less (in range 0-255) e.g. 1,44,78,255"
             , value model.decoded
             , onInput Decoded
             ]
                ++ myStyle
            )
            []
        , button [ onClick (Encode <| fromStr model.decoded) ] [ text "encode" ]
        , input
            ([ placeholder "text to decode"
             , value model.encoded
             , onInput Encoded
             ]
                ++ myStyle
            )
            []
        , button [ onClick (Decode model.encoded) ] [ text "decode" ]
        ]


myStyle : List (Attribute Msg)
myStyle =
    List.map (\( p, v ) -> style p v)
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]
