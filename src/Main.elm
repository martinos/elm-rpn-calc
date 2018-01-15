module Main exposing (..)

import Element as El exposing (..)
import Html
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Style exposing (StyleSheet)
import Style.Color
import Style.Font
import Color
import Calculator as Calc exposing (..)


type Styles
    = None
    | Debug
    | Button
    | LCDLine
    | LCD


main =
    Html.beginnerProgram
        { model = initModel, update = update, view = view }


view : Model -> Html.Html Msg
view model =
    El.layout stylesheet (body model)


body : Model -> Element Styles v Msg
body model =
    El.column None
        [ spacing keySpacing, center ]
        [ row None
            []
            [ column LCD
                [ spacing keySpacing ]
                ((model.stack |> display |> List.reverse |> List.map lcd)
                    ++ [ row None [] [ model.entry |> displayInput |> lcd ]
                       ]
                )
            , column None
                [ spacing keySpacing, paddingLeft keySpacing ]
                [ button "Swap" (ApplyCommand Swap)
                , button "Drop" (ApplyCommand Drop)
                ]
            ]
        , row None
            [ spacing keySpacing ]
            [ button "C" (ApplyCommand Clear)
            , button "+/-" (ApplyFunction negate)
            , button "%" (ApplyOperation Calc.percent)
            , button "/" (ApplyOperation (/))
            ]
        , row None
            [ spacing keySpacing ]
            [ button "7" (InputNumber '7')
            , button "8" (InputNumber '8')
            , button "9" (InputNumber '9')
            , button "*" (ApplyOperation (*))
            ]
        , row None
            [ spacing keySpacing ]
            [ button "4" (InputNumber '4')
            , button "5" (InputNumber '5')
            , button "6" (InputNumber '6')
            , button "+" (ApplyOperation (+))
            ]
        , row None
            [ spacing keySpacing ]
            [ button "1" (InputNumber '1')
            , button "2" (InputNumber '2')
            , button "3" (InputNumber '3')
            , button "-" (ApplyOperation (-))
            ]
        , row None
            [ spacing keySpacing ]
            [ button "0" (InputNumber '7')
            , button "" NoOp
            , button "." (InputNumber '.')
            , button "Enter" (ApplyCommand Enter)
            ]
        ]


lcd str =
    el LCDLine
        [ width (buttonSize * 3 + 2 * keySpacing |> px)
        , height <| px (buttonSize / (2.0) - 1)
        , padding 3
        , verticalCenter
        ]
        (str |> text)


button : String -> Msg -> Element Styles v Msg
button str msg =
    el Button
        [ width (px buttonSize)
        , height (px buttonSize)
        , onClick msg
        ]
        (el None [ center, verticalCenter ] <| text str)


buttonSize =
    70


keySpacing =
    2


fontSize =
    Style.Font.size 20


stylesheet =
    Style.styleSheet
        [ Style.style Debug
            [ Style.Color.background Color.red ]
        , Style.style Button
            [ Style.Color.background Color.grey, fontSize ]
        , Style.style LCDLine [ Style.Color.background Color.green, Style.Color.text Color.white, fontSize ]
        , Style.style LCD [ Style.Color.background Color.grey, fontSize ]
        ]


displayInput entry =
    case entry of
        Input str ->
            str

        Result_ str ->
            str |> toString

        Copy str ->
            str |> toString


display stack =
    let
        count =
            3

        lines =
            stack |> List.take count |> List.map toString

        len =
            lines |> List.length
    in
        if len < count then
            lines ++ (List.repeat (count - len) "")
        else
            lines
