module Tests exposing (..)

import Test exposing (..)
import Test.RoundTrip exposing (tests)


all : Test
all =
    concat
        [ Test.RoundTrip.tests
        ]
