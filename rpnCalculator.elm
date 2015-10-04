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

type alias Model = { stack: Stack, entry: Entry }

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
        model |> applyOperator operator
      Command command ->
        case command of
          Backspace -> { model | entry <- deleteChar model.entry }
          Enter ->  model |> push
          Clear -> model |> clear
          Swap -> model |> swap |> Debug.log "Swap"
    |> Debug.watch "Model"

push: Model -> Model
push model =
  {model| stack <- Stack.push (toFloat model.entry) model.stack
        , entry <- Copy (toFloat model.entry)}

clear: Model -> Model
clear model =
  {model| entry <- Input "",stack <- []}

swap: Model -> Model
swap model =
  case Stack.pop' model.stack of
    Nothing -> model
    Just (registry, stack) ->
      {model| entry <- Result' registry
            , stack <- Stack.push (toFloat model.entry) stack}

regToString: Entry -> String
regToString register =
  case register of
    Input string -> if String.isEmpty string then "0" else string ++ "_"
    Result' float -> toString float
    Copy float -> toString float

-- Operators --
applyOperator: Operator -> Model -> Model
applyOperator operator model =
  let
    calculate yRegister =
      operator yRegister <| toFloat model.entry
    update yRegister stack =
      {model| stack <- stack,
              entry <- Result' (calculate yRegister) }
  in
    (Stack.pop' model.stack) `Maybe.andThen`
                             (\(val, stack) -> Just (update val stack))
                             |> Maybe.withDefault model


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
    Input string -> {model | entry <- Input (string |> prepend0ToTrailingDot digit |> appendDigit digit)}
    Result' number -> {model | stack <- Stack.push number model.stack
                             , entry <- Input ""} |> updateNumber digit
    Copy number -> {model | entry <- Input ""} |> updateNumber digit


prepend0ToTrailingDot: Char -> String -> String
prepend0ToTrailingDot digit numberStr =
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
    Copy float -> float

deleteChar: Entry -> Entry
deleteChar register =
  case register of
    Input string -> Input <| String.slice 0 -1 string
    Result' float -> register
    Copy float -> register

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
