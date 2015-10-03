module RpnCalculator where

-- In javascrip https://gist.github.com/b016fae84bd52d6d1a31

import Html exposing (..)
import Keyboard exposing (KeyCode)
import String
import Char
import Signal exposing ((<~))
import Maybe
import Debug
import Stack exposing (Operator, Stack)

-- Signal ---

keyboard: Signal KeyCode
keyboard = Keyboard.presses

toAction: KeyCode -> Action
toAction keycode =
  let
    backspace = 8
    enter = 13
    escape = 27
  in
    if
      | isNumber (keycode |> Debug.log "KeyCode") -> InputNumber (Char.fromCode keycode)
      | isOperator (Char.fromCode keycode) ->
          case getOperator (Char.fromCode keycode) of
            Just oper -> Operator oper
            Nothing -> NoOp
      | keycode == backspace -> Command Backspace
      | keycode == enter -> Command Enter
      | keycode == escape -> Command Clear
      | otherwise -> NoOp

main: Signal Html
main = Signal.foldp update initModel (Signal.map toAction keyboard) |> Signal.map view

type alias Model = { stack: Stack, entry: Entry }

isNumber: KeyCode -> Bool
isNumber keycode =
  List.member keycode numberKeyCodes

numberKeyCodes: List KeyCode
numberKeyCodes =
  List.map Char.toCode ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

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

-- Model --

type alias OperatorMap = (Char, Operator)

type Command
  = Clear
  | Enter
  | Swap
  | Drop
  | Erase
  | Backspace

type Action
  = NoOp
  | InputNumber Char
  | Operator OperatorMap
  | Command Command -- Clear Enter Swap Drop Pop
  | Function String (Float -> Float)

type Entry
  = Input String
  | Result' Float
  | Copy Float

initModel: Model
initModel =
  { stack = [], entry = Input ""}

initStack: Stack
initStack = [2,3]

update: Action -> Model -> Model
update action model =
  let
    backspace = 8
    enter = 13
  in
    case action of
      NoOp -> model
      InputNumber char ->
        model |> updateNumber char
      Operator (_, operator) ->
        model |> push |> applyOperator operator |> pop
      Command command ->
        case command of
          Backspace -> { model | entry <- deleteChar' model.entry }
          Enter ->  model |> push
          Clear -> model |> clear
    |> Debug.watch "Model"

push: Model -> Model
push model =
  {model| stack <- Stack.push (toFloat model.entry) model.stack
        , entry <- Input ""}

pop: Model -> Model
pop model =
  let
    (val, stack) = Stack.pop model.stack
  in
    {model| stack <- stack
          , entry <- Result' val}

clear: Model -> Model
clear model =
  {model| entry <- Input "",stack <- []}

regToString: Entry -> String
regToString register =
  case register of
    Input string -> if String.isEmpty string then "0" else string
    Result' float -> toString float

-- Operators --

applyOperator: Operator -> Model -> Model
applyOperator operator model  =
  {model| stack <- Stack.calc model.stack operator}

getOperator: Char -> Maybe OperatorMap
getOperator input =
  List.filter (\(char, oper) -> input == char) operators |> List.head

isOperator: Char -> Bool
isOperator char =
  case getOperator char of
    Nothing -> False
    Just a -> True

operators: List (OperatorMap)
operators =
  [('+', (+)), ('-', (-)), ('/', (/)), (('*'), (*))]

-- input --

updateNumber: Char -> Model -> Model
updateNumber digit model =
  case model.entry of
    Input string -> {model | entry <- Input (string |> addZeroToDecimals digit |> appendDigit digit)}
    Result' number -> {model | stack <- Stack.push number model.stack
                             , entry <- Input <| String.fromChar digit}

addZeroToDecimals: Char -> String -> String
addZeroToDecimals digit numberStr =
  if  digit == '.' && String.isEmpty numberStr then
    "0"
  else
    numberStr

appendDigit: Char -> String -> String
appendDigit digit numberStr =
  if digit == '.' && String.contains (String.fromChar digit) numberStr then
    numberStr
  else
    numberStr ++ (String.fromChar digit)

toFloat: Entry -> Float
toFloat register =
  case register of
    Input string -> Result.toMaybe (String.toFloat string) |> Maybe.withDefault 0.0
    Result' float -> float

addToInput: String -> KeyCode -> String
addToInput input keycode =
  input ++ (Char.fromCode keycode |> String.fromChar)

deleteChar': Entry -> Entry
deleteChar' register =
  case register of
    Input string -> Input <| String.slice 0 -1 string
    Result' float -> register
