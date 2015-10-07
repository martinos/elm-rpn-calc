module RpnCalculator where

-- In javascrip https://gist.github.com/b016fae84bd52d6d1a31

import Keyboard exposing (KeyCode)
import Char
import Signal exposing ((<~))
import Maybe
import Debug
import Calculator exposing (..)
import Dict
import UndoList as UL
import Graphics.Element exposing (..)
import NewView

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
          case Dict.get char operators of
            Just oper -> ApplyOperation oper
            Nothing -> NoOp
      | keycode == backspace -> ApplyCommand Backspace
      | keycode == enter -> ApplyCommand Enter
      | keycode == escape -> ApplyCommand Clear
      | keycode == dollar -> ApplyCommand Swap
      | otherwise -> NoOp

isNumber: KeyCode -> Bool
isNumber keycode =
  List.member keycode numberKeyCodes

numberKeyCodes: List KeyCode
numberKeyCodes =
  List.map Char.toCode ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

keyboard: Signal KeyCode
keyboard = Keyboard.presses

loggedAction: KeyCode -> Calculator.Action
loggedAction =
  Debug.log "Action" << toAction << Debug.log "Keycode"

-- Signals

ulSignal: Signal.Mailbox (UL.Action Action)
ulSignal = Signal.mailbox (UL.New NoOp)

inputSignal: Signal (UL.Action Action)
inputSignal = Signal.merge ulSignal.signal (UL.New <~ (loggedAction <~ keyboard))

appSignal: AppSignal
appSignal =
  (Signal.foldp (UL.update update) (UL.fresh initModel) inputSignal)

type alias AppSignal = Signal {future: List Model, past: List Model, present: Model}
type alias AppAddress = Signal.Address AppSignal

main: Signal Element
main = (UL.view NewView.view ulSignal.address) <~ appSignal
