module RpnCalculator where

-- In javascript https://gist.github.com/b016fae84bd52d6d1a31

import Char exposing (KeyCode)
import Keyboard
import Char
import Signal
import Maybe
import Debug
import Calculator exposing (..)
import Dict
import Graphics.Element exposing (..)
import View

toAction: KeyCode -> Action
toAction keycode =
  let
    backspace = 8
    enter = 13
    escape = 27
    dollar = Char.toCode '$'
    char = Char.fromCode keycode
  in
    if isNumber keycode then InputNumber char
    else if isOperator char then
          case Dict.get char operators of
            Just oper -> ApplyOperation oper
            Nothing -> NoOp
    else if keycode == backspace then ApplyCommand Backspace
    else if  keycode == enter then ApplyCommand Enter 
    else if keycode == escape then ApplyCommand Clear 
    else if keycode == dollar then  ApplyCommand Swap 
    else  NoOp

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

actions: Signal.Mailbox Action
actions = Signal.mailbox NoOp

inputSignal: Signal Action
inputSignal = Signal.merge actions.signal ( keyboard |> Signal.map loggedAction)

appSignal: Signal Model
appSignal =
  Signal.foldp update initModel inputSignal

main: Signal Element
main = appSignal |> Signal.map (View.view actions.address)
