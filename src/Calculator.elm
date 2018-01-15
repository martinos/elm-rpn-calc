module Calculator exposing (..)

import Stack exposing (Stack)
import Debug
import String
import Dict exposing (Dict)


type alias Model =
    { stack : Stack, entry : Entry }


type alias Operation =
    Float -> Float -> Float


type Command
    = Clear
    | Enter
    | Swap
    | Drop
    | Erase
    | Backspace


type Msg
    = NoOp
    | InputNumber Char
    | ApplyOperation Operation
    | ApplyCommand Command
      -- Clear Enter Swap Drop Pop
    | ApplyFunction (Float -> Float)


type Entry
    = Input String
    | Result_ Float
    | Copy Float


initModel : Model
initModel =
    { stack = initStack, entry = Input "" }


initStack : Stack
initStack =
    []


update : Msg -> Model -> Model
update msg model =
    let
        backspace =
            8

        enter =
            13
    in
        case msg of
            NoOp ->
                model

            InputNumber char ->
                model |> updateNumber char

            ApplyOperation operator ->
                model |> applyOperator operator

            ApplyCommand command ->
                case command of
                    Backspace ->
                        { model | entry = deleteChar model.entry }

                    Enter ->
                        model |> push

                    Clear ->
                        model |> clear

                    Swap ->
                        model |> swap

                    Drop ->
                        model |> drop

                    Erase ->
                        model

            ApplyFunction function ->
                { model | entry = Result_ (function (toFloat model.entry)) }
                    |> Debug.log "Model"



-- input --


updateNumber : Char -> Model -> Model
updateNumber digit model =
    case model.entry of
        Input string ->
            { model | entry = Input (string |> prepend0ToTrailingDot digit |> appendDigit digit) }

        Result_ number ->
            { model
                | stack = Stack.push number model.stack
                , entry = Input ""
            }
                |> updateNumber digit

        Copy number ->
            { model | entry = Input "" } |> updateNumber digit


prepend0ToTrailingDot : Char -> String -> String
prepend0ToTrailingDot digit numberStr =
    if digit == '.' && String.isEmpty numberStr then
        "0"
    else
        numberStr


appendDigit : Char -> String -> String
appendDigit digit numberStr =
    if digit == '.' && String.contains (String.fromChar digit) numberStr then
        numberStr
    else
        numberStr ++ (String.fromChar digit)


toFloat : Entry -> Float
toFloat register =
    case register of
        Input string ->
            Result.toMaybe (String.toFloat string) |> Maybe.withDefault 0.0

        Result_ float ->
            float

        Copy float ->
            float



-- Commands --


deleteChar : Entry -> Entry
deleteChar register =
    case register of
        Input string ->
            Input <| String.slice 0 -1 string

        Result_ float ->
            register

        Copy float ->
            register


push : Model -> Model
push model =
    { model
        | stack = Stack.push (toFloat model.entry) model.stack
        , entry = Copy (toFloat model.entry)
    }


clear : Model -> Model
clear model =
    { model | entry = Input "", stack = [] }


swap : Model -> Model
swap model =
    case Stack.pop model.stack of
        Nothing ->
            model

        Just ( registry, stack ) ->
            { model
                | entry = Result_ registry
                , stack = Stack.push (toFloat model.entry) stack
            }


drop : Model -> Model
drop model =
    case Stack.pop model.stack of
        Nothing ->
            { model | entry = Result_ 0 }

        Just ( registry, stack ) ->
            { model
                | entry = Result_ registry
                , stack = stack
            }


regToString : Entry -> String
regToString register =
    case register of
        Input string ->
            if String.isEmpty string then
                "0"
            else
                string ++ "_"

        Result_ float ->
            toString float

        Copy float ->
            toString float



-- Operation --


applyOperator : Operation -> Model -> Model
applyOperator operator model =
    let
        calculate yRegister =
            operator yRegister <| toFloat model.entry

        update yRegister stack =
            { model
                | stack = stack
                , entry = Result_ (calculate yRegister)
            }
    in
        (Stack.pop model.stack)
            |> Maybe.map (uncurry update)
            |> Maybe.withDefault model


isOperator : Char -> Bool
isOperator char =
    case Dict.get char operators of
        Nothing ->
            False

        Just a ->
            True


operators : Dict Char Operation
operators =
    Dict.fromList [ ( '+', (+) ), ( '-', (-) ), ( '/', (/) ), ( ('*'), (*) ) ]


percent : Float -> Float -> Float
percent number percent =
    number * percent / 100
