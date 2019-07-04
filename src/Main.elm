module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed
import Task



-- Main


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Board =
    List (List Int)


type alias Model =
    { isX : Bool
    , isGameOver : Bool
    , roundCount : Int
    , board : Board
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model True
        False
        0
        [ [ 0, 0, 0 ]
        , [ 0, 0, 0 ]
        , [ 0, 0, 0 ]
        ]
    , Cmd.none
    )



--Update


type Msg
    = Click Int Int Int
    | EndRound Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click i j cell ->
            if cell == 0 && not model.isGameOver then
                let
                    newModel =
                        { model
                            | board = addMove i j model.isX model.board
                            , isX = not model.isX
                            , roundCount = model.roundCount + 1
                        }
                in
                ( newModel
                , Task.perform EndRound (Task.succeed (calculateRound newModel))
                )

            else
                ( model, Cmd.none )

        EndRound isGameOver ->
            ( { model | isGameOver = isGameOver }, Cmd.none )


addMove : Int -> Int -> Bool -> Board -> Board
addMove i_ j_ isX board =
    List.indexedMap
        (\i row ->
            List.indexedMap
                (\j cell ->
                    if i == i_ && j == j_ then
                        if isX then
                            1

                        else
                            2

                    else
                        cell
                )
                row
        )
        board


rotateBoard : Board -> Board
rotateBoard board =
    List.foldl
        (\cur acc ->
            List.indexedMap
                (\i item ->
                    if i == 0 then
                        List.take 1 cur ++ item

                    else if i == 1 then
                        (cur |> List.take 2 |> List.drop 1) ++ item

                    else
                        (cur |> List.reverse |> List.take 1) ++ item
                )
                acc
        )
        [ []
        , []
        , []
        ]
        board


isRow : Board -> Bool
isRow board =
    List.foldl
        (\cur acc ->
            if List.all ((==) 1) cur || List.all ((==) 2) cur then
                True

            else
                acc
        )
        False
        board


isDiagonal : Board -> Bool
isDiagonal board =
    let
        diagonalCheck : Int -> Int -> Int -> Int -> Int
        diagonalCheck i j cell symbol =
            if i == 0 && j == 0 then
                symbol

            else if i == 1 && j == 1 then
                symbol

            else if i == 2 && j == 2 then
                symbol

            else
                cell

        board_ =
            List.indexedMap
                (\i_ row ->
                    List.indexedMap
                        (\j_ cell ->
                            if cell == 1 then
                                diagonalCheck i_ j_ cell 10

                            else if cell == 2 then
                                diagonalCheck i_ j_ cell 20

                            else
                                cell
                        )
                        row
                )
                board
    in
    if List.all (List.member 10) board_ || List.all (List.member 20) board_ then
        True

    else
        False


calculateRound : Model -> Bool
calculateRound { board } =
    let
        isColumn =
            rotateBoard board |> isRow

        isDiagonalX =
            isDiagonal board

        isDiagonalY =
            board |> rotateBoard |> isDiagonal
    in
    if isRow board || isColumn || isDiagonalX || isDiagonalY then
        True

    else
        False



--Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic-tac-toe" ]
        , h2 []
            [ text <|
                if model.isGameOver then
                    "Game over!"

                else
                    ""
            ]
        , div [ class "board" ] <| renderBoard model.board
        ]


renderBoard : Board -> List (Html Msg)
renderBoard board =
    List.indexedMap
        (\i row ->
            List.indexedMap
                (\j cell ->
                    Html.Keyed.node "div"
                        [ class "cell", onClick <| Click i j cell ]
                        [ ( String.fromInt i ++ String.fromInt j
                          , text <| String.fromInt cell
                          )
                        ]
                )
                row
        )
        board
        |> List.concat
