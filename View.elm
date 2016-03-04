module View (..) where

import Text exposing (Text)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Signal
import Calculator exposing (..)
import Color
import Debug


type alias ActionAddress =
  Signal.Address Action



-- CONFIGURATION --
-- Dimensions


calcSizeX =
  (calcBorder + numpadSizeX + calcBorder)


calcSizeY =
  calcBorder + stackSizeY + calcBorder + numpadSizeY + calcBorder


keySize =
  ( 50, 50 )


( keyWidth, keyHeight ) =
  keySize


keySpace =
  1


keySpacer =
  spacer keySpace keySpace


numpadSizeX =
  round (4 * (fst keySize) + 3 * keySpace) |> Debug.log "numpadSizeX"


numpadSizeY =
  round (5 * (snd keySize) + 4 * keySpace)


calcBorder =
  2


calcBorderSpacer =
  spacer calcBorder calcBorder


stackSizeX =
  (calcSizeX |> Debug.log "calcSizeX") - commandSizeX - 2 * calcBorder - keySpace


stackSizeY =
  commandSizeY


commandSizeX =
  keyWidth


commandSizeY =
  keyHeight + keySpace + keyHeight



-- Colors


keyColor =
  color <| Color.rgb 217 217 217


keyTextColor =
  Text.color Color.black


keyPadBackgroudColor =
  color <| Color.grayscale 0.4


calcColor =
  color Color.red


displaySeparatorColor =
  color <| Color.white


displayBackgroudColor =
  color <| Color.rgb 118 123 126


displayTextColor =
  Text.color Color.white


keyFont : Text -> Text
keyFont =
  Text.typeface [ "Roboto-Regular", "helvetica", "arial", "sans-serif" ]



----------
-- VIEW --
----------


view : Signal.Address Action -> Model -> Element
view address model =
  [ [ stackDisplay model, keySpacer, commandKeys address ] |> flow right
  , calcBorderSpacer
  , numpadContainer (keys address)
  ]
    |> flow down
    |> mainContainer


mainContainer : Element -> Element
mainContainer =
  container calcSizeX calcSizeY middle >> calcColor



-- COMMAND KEYS --


commandContainer : Element -> Element
commandContainer =
  container commandSizeX commandSizeY topLeft >> keyPadBackgroudColor


commandKeys : Signal.Address Action -> Element
commandKeys address =
  [ key ( "swap", Signal.message address (ApplyCommand Swap) )
  , keySpacer
  , key ( "drop", Signal.message address (ApplyCommand Drop) )
  ]
    |> flow up



-- STACK CONTAINER --


stackDisplay : Model -> Element
stackDisplay model =
  let
    displayData =
      (regToString model.entry) :: List.map toString model.stack
  in
    List.map register displayData |> List.intersperse (spacer stackSizeX keySpace |> keyPadBackgroudColor) |> flow up |> stackContainer


stackContainer : Element -> Element
stackContainer =
  container stackSizeX stackSizeY bottomLeft >> displayBackgroudColor


register : String -> Element
register text =
  spacer 5 5 `beside` (displayText text |> rightAligned) |> container stackSizeX 25 midLeft |> displayBackgroudColor


displayText : String -> Text
displayText =
  Text.fromString >> keyFont >> displayTextColor



-- NUM PAD --


numpadContainer : Element -> Element
numpadContainer =
  container numpadSizeX numpadSizeY topLeft >> keyPadBackgroudColor


keys : ActionAddress -> Element
keys address =
  List.map numRow (keyContent address) |> List.intersperse keySpacer |> flow down


keyContent : Signal.Address Action -> List (List ( String, Signal.Message ))
keyContent address =
  let
    data =
      [ [ ( "C", ApplyCommand Clear ), ( "±", ApplyFunction negate ), ( "%", ApplyOperation percent ), ( "÷", ApplyOperation (/) ) ]
      , [ ( "7", InputNumber '7' ), ( "8", InputNumber '8' ), ( "9", InputNumber '9' ), ( "x", ApplyOperation (*) ) ]
      , [ ( "4", InputNumber '4' ), ( "5", InputNumber '5' ), ( "6", InputNumber '6' ), ( "+", ApplyOperation (+) ) ]
      , [ ( "1", InputNumber '1' ), ( "2", InputNumber '2' ), ( "3", InputNumber '3' ), ( "-", ApplyOperation (-) ) ]
      , [ ( "0", InputNumber '0' ), ( "", NoOp ), ( ".", InputNumber '.' ), ( "enter", ApplyCommand Enter ) ]
      ]

    toMsg ( str, action ) =
      ( str, Signal.message address action )

    lineConv line =
      List.map toMsg line
  in
    List.map lineConv data


numRow : List ( String, Signal.Message ) -> Element
numRow content =
  List.map key content |> List.intersperse keySpacer |> flow right


keyText : String -> Text
keyText =
  Text.fromString >> keyFont >> keyTextColor


key : ( String, Signal.Message ) -> Element
key ( text, message ) =
  text |> keyText >> leftAligned |> container keyWidth keyHeight middle |> keyColor |> clickable message

