module RpnTest exposing (tests)

import Test exposing (..)
import Html
import RpnCalculator exposing (..)
import Calculator exposing (..)
import Expect exposing (..)
import String
import Char


enter =
    "\x0D"


tests : Test
tests =
    describe
        "My Test Suite"
        -- Number entries
        [ test "Enter a number with decimal <Enter>: the stack contains the same number" <|
            \_ -> assertStack 1.2 (initModel |> run "1.2\x0D")
        , test "After an operation pressing <.>: should prepend a zero in front of the <.>" <|
            \_ -> assertEntry (Input "0.") (initModel |> run "1\x0D+.")
        , test "Press . then <Enter>: the stack contains 0" <|
            \_ -> assertStack 0 (initModel |> run ".\x0D")
        , test "Enter a number with 2 <.>: the second . should be filtered out" <|
            \_ -> assertStack 1.2 (initModel |> run ("1.2." ++ enter))
          -- Operations
        , test "Addition" <|
            \_ -> assertEntry (Result_ 8) (initModel |> run ("1" ++ enter ++ "2+5+"))
        , test "Division" <|
            \_ -> assertEntry (Result_ 2) (initModel |> run ("10" ++ enter ++ "5/"))
        , test "When the stack is empty we forbid operations" <|
            \_ -> assertEntry (Input "3") (initModel |> run "3/")
        , test "After a calculation an new entry pushes the previous result to the stack" <|
            \_ -> assertEntry (Input "2") (initModel |> run "23" |> update (ApplyCommand Backspace))
          -- Commands
        , test "Enter key keep entry as is" <|
            \_ -> assertEntry (Copy 1) (initModel |> run ("1" ++ enter))
        , test "Press <Enter> then a digit: the stack does not change" <|
            \_ -> assertStack 1 (initModel |> run ("1" ++ enter))
        , test "<Enter> pushes to first stack register" <|
            \_ -> assertStack 1 (initModel |> run ("1" ++ enter))
        , test "Press <Enter> then a digit: the entry is filled with that number" <|
            \_ -> assertEntry (Input "3") (initModel |> run ("1" ++ enter ++ "3"))
        , test "Backspace erase a digit" <|
            \_ -> assertEntry (Input "2") (initModel |> run "23" |> update (ApplyCommand Backspace))
        , test "<Clear> should clear the stack" <|
            \_ -> equal [] (initModel |> run "23" |> update (ApplyCommand Clear)).stack
        , test "<Clear> should clear the entry" <|
            \_ -> assertEntry (Input "") (initModel |> run "23" |> update (ApplyCommand Clear))
        , test "<Swap> the registry" <|
            \_ -> assertEntry (Result_ 2) (initModel |> run ("2" ++ enter ++ "3") |> update (ApplyCommand Swap))
        , test "<Drop> the registry" <|
            \_ -> assertEntry (Result_ 2) (initModel |> run ("2" ++ enter ++ "3") |> update (ApplyCommand Drop))
          -- functions
        , test "function should ouput result" <|
            \_ -> assertEntry (Result_ -2) (initModel |> run "2" |> update (ApplyFunction negate))
        ]


assertEntry : Entry -> Model -> Expectation
assertEntry expected model =
    equal expected model.entry


assertStack : Float -> Model -> Expectation
assertStack expected model =
    equal (Just expected) (List.head model.stack)


run : String -> Model -> Model
run commands initModel =
    String.foldl
        (\c model -> update (toAction (Char.toCode c)) model)
        initModel
        commands
