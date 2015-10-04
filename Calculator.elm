module Calculator where

import Stack exposing (Stack)
import Debug
import String

type alias Model = { stack: Stack, entry: Entry }
type alias Operator = Float -> Float -> Float
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
          Swap -> model |> swap
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
  case Stack.pop model.stack of
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
    (Stack.pop model.stack) `Maybe.andThen`
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
