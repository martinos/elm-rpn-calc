module RpnCalculator where

-- In javascrip https://gist.github.com/b016fae84bd52d6d1a31

import Html exposing (..)
import Keyboard exposing (KeyCode)
import Char
import Signal exposing ((<~))
import Maybe
import Debug
import Calculator exposing (..)

-- View --

view: Model -> Html
view model =
  ul []
     (regElem model.entry :: List.map recordElem model.stack |> List.reverse)

recordElem: Float -> Html
recordElem num =
  li []
     [text <| toString num]

regElem: Entry -> Html
regElem reg =
  li []
     [text <| regToString reg]

-- Signal ---

toAction: KeyCode -> Action
toAction keycode =
  let
    backspace = 8
    enter = 13
    escape = 27
    dollar = Char.toCode '$'
    char = Char.fromCode keycode
  in
    if
      | isNumber keycode -> InputNumber char
      | isOperator char ->
          case getOperator char of
            Just oper -> Operator oper
            Nothing -> NoOp
      | keycode == backspace -> Command Backspace
      | keycode == enter -> Command Enter
      | keycode == escape -> Command Clear
      | keycode == dollar -> Command Swap
      | otherwise -> NoOp

isNumber: KeyCode -> Bool
isNumber keycode =
  List.member keycode numberKeyCodes

numberKeyCodes: List KeyCode
numberKeyCodes =
  List.map Char.toCode ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

keyboard: Signal KeyCode
keyboard = Keyboard.presses

loggedAction: KeyCode -> Action
loggedAction =
  Debug.log "Action" << toAction << Debug.log "Keycode"

main: Signal Html
main = view <~ (Signal.foldp update initModel (loggedAction <~ keyboard))
