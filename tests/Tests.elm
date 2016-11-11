module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (intRange, list)
import Expect
import String
import BinaryBase64 exposing (encode, decode)


roundTrip : List Int -> List Int
roundTrip l =
    let
        decoded =
            decode <| encode l
    in
        case decoded of
            Err s ->
                [ -1 ]

            Ok l ->
                l


all : Test
all =
    describe "A Test Suite"
        [ fuzz (list (intRange 0 255)) "decode inverse of encode" <|
            \is ->
                is
                    |> roundTrip
                    |> Expect.equal is
        ]
