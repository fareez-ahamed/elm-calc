module Main exposing (main)

import Browser
import Html exposing (button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program () Model Key
main =
    Browser.sandbox
        { init = initModel, update = update, view = view }


type alias Model =
    { register : Float
    , accumulator : Float
    , mode : DisplayMode
    , operation : BinaryOperation
    , decimal : Bool
    , decimalCount : Int
    }


type DisplayMode
    = Acc
    | Reg


type BinaryOperation
    = Addition
    | Subtraction
    | Division
    | Multiplication
    | None


type Key
    = Num Int
    | Operation BinaryOperation
    | Minus
    | Decimal
    | Equals
    | Clear
    | ClearEntry
    | Percent


initModel : Model
initModel =
    { register = 0
    , accumulator = 0
    , mode = Reg
    , operation = None
    , decimal = False
    , decimalCount = 0
    }


update : Key -> Model -> Model
update msg model =
    case msg of
        Num i ->
            { model
                | register = addNum model i
                , mode = Reg
                , decimalCount =
                    if model.decimal then
                        model.decimalCount + 1

                    else
                        0
            }

        Minus ->
            { model | register = -model.register }

        Clear ->
            initModel

        ClearEntry ->
            case model.mode of
                Acc ->
                    initModel

                Reg ->
                    { model | register = 0, decimal = False, decimalCount = 0 }

        Operation x ->
            { model
                | operation = x
                , accumulator =
                    calculateBinary model.operation model.accumulator model.register
                , register = 0
                , decimal = False
                , decimalCount = 0
                , mode = Acc
            }

        Equals ->
            { model
                | operation = None
                , accumulator =
                    calculateBinary model.operation model.accumulator model.register
                , register = 0
                , decimal = False
                , decimalCount = 0
                , mode = Acc
            }

        Decimal ->
            { model | decimal = True }

        _ ->
            model


calculateBinary oper acc reg =
    case oper of
        Addition ->
            acc + reg

        Subtraction ->
            acc - reg

        Multiplication ->
            acc * reg

        Division ->
            acc / reg

        None ->
            reg


addNum : Model -> Int -> Float
addNum model keyVal =
    if model.decimal then
        let
            fraction = 10 ^ (model.decimalCount+1) |> toFloat
                
        in
        model.register + 1 / fraction * toFloat keyVal

    else
        model.register * 10 + toFloat keyVal


view : Model -> Html.Html Key
view model =
    div [] [ resultBox model, keyBox ]


resultBox : Model -> Html.Html msg
resultBox model =
    div
        [ style "background-color" "blue"
        , style "font-size" "30px"
        , style "text-align" "right"
        , style "padding" "1rem"
        ]
        [ case model.mode of
            Acc ->
                String.fromFloat model.accumulator |> text

            Reg ->
                String.fromFloat model.register |> text
        ]


keyBox : Html.Html Key
keyBox =
    div []
        [ buttonRow
            [ calcButton "%" Percent
            , calcButton "CE" ClearEntry
            , calcButton "C" Clear
            , calcButton "/" <| Operation Division
            ]
        , buttonRow
            [ calcButton "7" <| Num 7
            , calcButton "8" <| Num 8
            , calcButton "9" <| Num 9
            , calcButton "X" <| Operation Multiplication
            ]
        , buttonRow
            [ calcButton "4" <| Num 4
            , calcButton "5" <| Num 5
            , calcButton "6" <| Num 6
            , calcButton "-" <| Operation Subtraction
            ]
        , buttonRow
            [ calcButton "1" <| Num 1
            , calcButton "2" <| Num 2
            , calcButton "3" <| Num 3
            , calcButton "+" <| Operation Addition
            ]
        , buttonRow
            [ calcButton "+/-" Minus
            , calcButton "0" <| Num 0
            , calcButton "." Decimal
            , calcButton "=" Equals
            ]
        ]


buttonRow buttons =
    div [ style "display" "flex" ] buttons


calcButton : String -> Key -> Html.Html Key
calcButton lbl clickHandler =
    button
        [ style "flex-grow" "1", style "height" "32px", onClick clickHandler ]
        [ text lbl ]
