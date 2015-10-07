module NewView where

import Text exposing (Text)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Signal exposing ((<~))
import Calculator exposing (..)
import UndoList as UL
import Color
import Debug

type alias ViewAction = UL.Action Action
type alias ActionAddress = Signal.Address ViewAction
-- type alias AppSignal = Signal {future: List Model, past: List Model, present: Model}
-- type alias AppAddress = Signal.Address AppSignal

-- dimensions
calcSize = (calcSizeX, calcSizeY)
(calcWidth, calcHeight) = calcSize

keySize = (50, 50)
(keyWidth, keyHeight) = keySize

keySpace = 2
keySpacer = spacer keySpace keySpace

numpadSizeX = round (4 * (fst keySize) + 5 * keySpace) |> Debug.log "numpadSizeX"
numpadSizeY = round (5 * (snd keySize) + 6 * keySpace) |> Debug.log "sizeY"

calcBorder = 10

stackSizeY = 90

calcSizeX = numpadSizeX + 2 * calcBorder
calcSizeY = calcBorder + stackSizeY + calcBorder + numpadSizeY + calcBorder


view: Signal.Address ViewAction -> Model -> Element
view address model =
  display address model


display: Signal.Address ViewAction -> Model -> Element
display address model =
  [ spacer 10 10
  , stackDisplay model
  , spacer 10 10
  , numpadContainer (keys address)]|> flow down |> mainContainer

numpadContainer: Element -> Element
numpadContainer =
  container numpadSizeX numpadSizeY topLeft >> color Color.green

keyContent: Signal.Address ViewAction -> List (List (String, Signal.Message))
keyContent address =
  let
    data =
      [ [("C", ApplyCommand Clear),            ("±", NoOp),            ("%", NoOp),            ("÷", ApplyOperation (/))]
      , [("7", InputNumber '7'), ("8", InputNumber '8'), ("9", InputNumber '9'), ("x", ApplyOperation (*))]
      , [("4", InputNumber '4'), ("5", InputNumber '5'), ("6", InputNumber '6'), ("+", ApplyOperation (+))]
      , [("1", InputNumber '1'), ("2", InputNumber '2'), ("3", InputNumber '3'), ("-", ApplyOperation (-))]
      , [("0", InputNumber '0'), ("", NoOp),             (".", InputNumber '.'), ("enter", ApplyCommand Enter)]]
    toMsg (str, action) =
      (str, Signal.message address (UL.New action))
    lineConv line =
      List.map toMsg line
  in
    List.map lineConv data



keys: ActionAddress -> Element
keys address =
  keySpacer :: (List.map row (keyContent address) |> List.intersperse keySpacer) |> flow down

row content =
  ( keySpacer
    :: (List.map key content |> List.intersperse keySpacer))
    ++ [keySpacer]|> flow right

key: (String, Signal.Message) -> Element
key (text, message) =
  text |> textElem |> container keyWidth keyHeight middle |> color Color.blue |> clickable message

mainContainer: Element -> Element
mainContainer  =
  container calcWidth calcHeight midTop >> color Color.red

stackContainer: Element -> Element
stackContainer =
  container numpadSizeX stackSizeY bottomLeft >> color Color.blue

textElem: String -> Element
textElem =
  Text.fromString >> Text.color Color.white >> calcFont >> leftAligned

stackDisplay: Model -> Element
stackDisplay model =
  let
    displayData =  (regToString model.entry):: List.map toString model.stack
  in
    List.map (textElem) displayData
      |> flow up |> stackContainer

calcFont: Text -> Text
calcFont =
  Text.typeface [ "Roboto-Regular", "helvetica", "arial", "sans-serif"]
