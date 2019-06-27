module Tests exposing (..)

import Test exposing (..)
import Expect


all : Test
all =
    describe "A Test Suite"
        [ test "HAHA UNIT TESTS? WHAT ARE THOSE?" <|
            \_ ->
                Expect.fail "lol"
        ]
