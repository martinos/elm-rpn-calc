module Main (..) where

import ElmTest exposing (..)
import RpnCalculator exposing (..)
import Calculator exposing (..)
import Graphics.Element exposing (..)
import String
import Char


assertEntry : Entry -> Model -> Assertion
assertEntry expected model =
  assertEqual expected model.entry


assertStack : Float -> Model -> Assertion
assertStack expected model =
  assertEqual (Just expected) (List.head model.stack)


run : String -> Model -> Model
run commands initModel =
  String.foldl
    (\c model -> update (toAction (Char.toCode c)) model)
    initModel
    commands


tests : Test
tests =
  suite
    "My Test Suite"
    -- Number entries
    [ test "Enter a number with decimal <Enter>: the stack contains the same number"
        <| assertStack 1.2 (initModel |> run "1.2\x0D")
    , test "After an operation pressing <.>: should prepend a zero in front of the <.>"
        <| assertEntry (Input "0.") (initModel |> run "1\x0D+.")
    , test "Press . then <Enter>: the stack contains 0"
        <| assertStack 0 (initModel |> run ".\x0D")
    , test "Enter a number with 2 <.>: the second . should be filtered out"
        <| assertStack 1.2 (initModel |> run "1.2.\x0D")
      -- Operations
    , test "Addition"
        <| assertEntry (Result' 8) (initModel |> run "1\x0D2+5+")
    , test "Division"
        <| assertEntry (Result' 2) (initModel |> run "10\x0D5/")
    , test "When the stack is empty we forbid operations"
        <| assertEntry (Input "3") (initModel |> run "3/")
    , test "After a calculation an new entry pushes the previous result to the stack"
        <| assertEntry (Input "2") (initModel |> run "23" |> update (ApplyCommand Backspace))
      -- Commands
    , test "Enter key keep entry as is"
        <| assertEntry (Copy 1) (initModel |> run "1\x0D")
    , test "Press <Enter> then a digit: the stack does not change"
        <| assertStack 1 (initModel |> run "1\x0D3")
    , test "<Enter> pushes to first stack register"
        <| assertStack 1 (initModel |> run "1\x0D")
    , test "Press <Enter> then a digit: the entry is filled with that number"
        <| assertEntry (Input "3") (initModel |> run "1\x0D3")
    , test "Backspace erase a digit"
        <| assertEntry (Input "2") (initModel |> run "23" |> update (ApplyCommand Backspace))
    , test "<Clear> should clear the stack"
        <| assertEqual [] (initModel |> run "23" |> update (ApplyCommand Clear)).stack
    , test "<Clear> should clear the entry"
        <| assertEntry (Input "") (initModel |> run "23" |> update (ApplyCommand Clear))
    , test "<Swap> the registry"
        <| assertEntry (Result' 2) (initModel |> run "2\x0D3" |> update (ApplyCommand Swap))
    , test "<Drop> the registry"
        <| assertEntry (Result' 2) (initModel |> run "2\x0D3" |> update (ApplyCommand Drop))
      -- functions
    , test "function should ouput result"
        <| assertEntry (Result' -2) (initModel |> run "2" |> update (ApplyFunction negate))
    ]


main : Graphics.Element.Element
main =
  elementRunner tests

