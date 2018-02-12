module Tests exposing (..)

import Test exposing (..)
import Test.RoundTrip exposing (tests)
import Test.StackSafety exposing (tests)


all : Test
all =
    concat
        [ Test.RoundTrip.tests
        , Test.StackSafety.tests
        ]
