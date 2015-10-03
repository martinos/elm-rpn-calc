import ElmTest.Test exposing  (..)
import ElmTest.Assertion exposing (..)
import ElmTest.Runner.Element exposing (..)
import RpnCalculator exposing (..)
import Graphics.Element exposing (..)
import String
import Char


assertEntry: Entry -> Model -> Assertion
assertEntry expected model =
  assertEqual expected model.entry

assertStack: Float -> Model -> Assertion
assertStack expected model =
  assertEqual (Just expected) (List.head model.stack)

run: String -> Model -> Model
run commands initModel =
  String.foldl (\c model -> update (toAction (Char.toCode c)) model)
               initModel
               commands

tests: Test
tests = suite "My Test Suite"
        [ test "<Enter> pushes to first stack register"
            <| assertStack 1 (initModel |> run "1\r")
        -- , test "Enter key keep entry as is"
        --     <| assertEntry (Result' 1) (initModel |> run "1\r")
        -- , test "When the stack is empty we disable the operations"
        --     <| assertEntry (Input "3") (initModel |> run "3/")
        , test "Press <Enter> then a digit: the entry is filled with that number"
            <| assertEntry (Input "3") (initModel |> run "1\r3")
        , test "Press <Enter> then a digit: the stack does not change"
            <| assertStack 1 (initModel |> run "1\r3")
        , test "Press . then <Enter>: the stack contains 0"
            <| assertStack 0 (initModel |> run ".\r")
        , test "Enter a number with decimal <Enter>: the stack contains the same number"
            <| assertStack 1.2 (initModel |> run "1.2\r")
        , test "Enter a number with 2 <.>: the second . should be filtered out"
            <| assertStack 1.2 (initModel |> run "1.2.\r")
        , test "Addition"
            <| assertEntry (Result' 8) (initModel |> run "1\r2+5+")
        , test "Backspace erase a digit"
            <| assertEntry (Input "2") (initModel |> run "23" |> update (Command Backspace))
        , test "After a calculation an new entry pushes the previous result to the stack"
            <| assertEntry (Input "2") (initModel |> run "23" |> update (Command Backspace))
        , test "<Clear> should clear the stack"
            <| assertEqual [] (initModel |> run "23" |> update (Command Clear)).stack
        , test "<Clear> should clear the entry"
            <| assertEntry (Input "") (initModel |> run "23" |> update (Command Clear))
        ]

main: Graphics.Element.Element
main =
  runDisplay tests
