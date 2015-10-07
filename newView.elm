module NewView where

import Text exposing (Text)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Signal exposing ((<~))
import Calculator exposing (..)
import UndoList as UL
import Color

type alias ViewAction = UL.Action Action
type alias ActionAddress = Signal.Address ViewAction

-- Configuration --

-- Dimensions

calcSizeX = calcBorder + stackSizeX + calcBorder + commandSizeX + calcBorder
calcSizeY = calcBorder + stackSizeY + calcBorder + numpadSizeY + calcBorder

keySize = (50, 50)
(keyWidth, keyHeight) = keySize

keySpace = 1
keySpacer = spacer keySpace keySpace

numpadSizeX = round (4 * (fst keySize) + 5 * keySpace)
numpadSizeY = round (5 * (snd keySize) + 6 * keySpace)

calcBorder = 1

stackSizeX = numpadSizeX - commandSizeX - calcBorder
stackSizeY = commandSizeY

commandSizeX = keyWidth + 2 * keySpace
commandSizeY = keySpace + keyHeight + keySpace + keyHeight + keySpace

-- Colors

keyColor = color <| Color.rgb 217 217 217
keyTextColor = Text.color Color.black
keyPadBackgroudColor = color <| Color.grayscale 0.40

calcColor = color Color.red
displayColor = color <| Color.rgb 112 114 117
displayTextColor = Text.color Color.white

keyFont: Text -> Text
keyFont =
  Text.typeface [ "Roboto-Regular", "helvetica", "arial", "sans-serif"]

view: Signal.Address ViewAction -> Model -> Element
view address model =
    display address model

display: Signal.Address ViewAction -> Model -> Element
display address model =
  [ spacer calcBorder calcBorder
  , [spacer calcBorder calcBorder, stackDisplay model, spacer calcBorder calcBorder, commandKeys address |> commandContainer] |> flow right
  , spacer calcBorder calcBorder
  , [spacer calcBorder calcBorder, numpadContainer (keys address)] |> flow right]|> flow down |> mainContainer

mainContainer: Element -> Element
mainContainer  =
  container calcSizeX calcSizeY topLeft >> calcColor

commandContainer: Element -> Element
commandContainer =
  container commandSizeX commandSizeY topLeft >> keyPadBackgroudColor

commandKeys: Signal.Address ViewAction -> Element
commandKeys address =
  [ keySpacer
  , key ("swap", Signal.message address (UL.New (ApplyCommand Swap)))
  , keySpacer
  , key ("drop", Signal.message address (UL.New (ApplyCommand Drop)))] |> flow down

numpadContainer: Element -> Element
numpadContainer =
  container numpadSizeX numpadSizeY topLeft >> keyPadBackgroudColor

stackContainer: Element -> Element
stackContainer =
  container stackSizeX stackSizeY bottomLeft >> displayColor

displayText: String -> Text
displayText =
  Text.fromString >> keyFont >> displayTextColor

stackDisplay: Model -> Element
stackDisplay model =
  let
    displayData =  (regToString model.entry):: List.map toString model.stack
  in
    List.map (displayText  >> leftAligned) displayData
      |> flow up |> stackContainer

keyContent: Signal.Address ViewAction -> List (List (String, Signal.Message))
keyContent address =
  let
    data =
      [ [("C", ApplyCommand Clear), ("±", ApplyFunction negate),              ("%", ApplyOperation (percent)),            ("÷", ApplyOperation (/))]
      , [("7", InputNumber '7'),    ("8", InputNumber '8'),   ("9", InputNumber '9'), ("x", ApplyOperation (*))]
      , [("4", InputNumber '4'),    ("5", InputNumber '5'),   ("6", InputNumber '6'), ("+", ApplyOperation (+))]
      , [("1", InputNumber '1'),    ("2", InputNumber '2'),   ("3", InputNumber '3'), ("-", ApplyOperation (-))]
      , [("0", InputNumber '0'),    ("", NoOp),               (".", InputNumber '.'), ("enter", ApplyCommand Enter)]]
    toMsg (str, action) =
      (str, Signal.message address (UL.New action))
    lineConv line =
      List.map toMsg line
  in
    List.map lineConv data

keys: ActionAddress -> Element
keys address =
  keySpacer :: (List.map numRow (keyContent address) |> List.intersperse keySpacer) |> flow down

numRow content =
  ( keySpacer
    :: (List.map key content |> List.intersperse keySpacer))
    ++ [keySpacer]|> flow right

keyText: String -> Text
keyText =
  Text.fromString >> keyFont >> keyTextColor

key: (String, Signal.Message) -> Element
key (text, message) =
  text |> keyText >> leftAligned |> container keyWidth keyHeight middle |> keyColor |> clickable message
