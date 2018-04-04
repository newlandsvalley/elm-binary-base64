module Test.StackSafety exposing (..)

import Test exposing (..)
import Expect exposing (..)
import String
import BinaryBase64 exposing (encode, decode)


-- just ensure that the tests actually run
-- I'm not sure of the behavious of elm-test if there's a stack issue
-- When set to 100K, the test seems to hang


tests : Test
tests =
    describe "stack safety tests"
        [ test "encode big input" <|
            \_ ->
                let
                    bigInput =
                        List.repeat 50000 0x00
                in
                    Expect.greaterThan 0 (String.length (encode bigInput))
        , test "decode big input" <|
            \_ ->
                let
                    bigInput =
                        String.repeat 50000 "0"
                in
                    case decode bigInput of
                        Ok a ->
                            Expect.greaterThan 0 (List.length a)

                        Err a ->
                            Expect.fail "Decoding shouldn't fail."
        ]
