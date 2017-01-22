module InfixTests exposing (..)

import CombineTestUtil exposing (..)
import Expect
import Parser.Types as Types exposing (..)
import Test exposing (..)
import Parser.Infix as Infix exposing (..)


all : Test
all =
    describe "InfixTests"
        [ test "right infix" <|
            \() ->
                parseFullStringState emptyState "infixr 3 <<" Infix.infixDefinition
                    |> Expect.equal (Just ({ direction = Right, precedence = 3, operator = "<<" }))
        , test "left infix" <|
            \() ->
                parseFullStringState emptyState "infixl 5 >>" Infix.infixDefinition
                    |> Expect.equal (Just ({ direction = Left, precedence = 5, operator = ">>" }))
        ]
