module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed
import Task



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type Turn
    = Player1
    | Player2


type GameState
    = NotStarted
    | Playing
    | Winner Turn
    | Draw


type alias Board =
    List (List Int)


type alias Model =
    { turn : Turn
    , gameState : GameState
    , roundCount : Int
    , board : Board
    }


initModel =
    Model
        Player1
        NotStarted
        0
        [ [ 0, 0, 0 ]
        , [ 0, 0, 0 ]
        , [ 0, 0, 0 ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



--Update


type Msg
    = Click Int Int Int
    | EndRound GameState
    | RestartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ gameState, turn, board, roundCount } as model) =
    case msg of
        Click i j cell ->
            if cell == 0 && not (isGameOver gameState) then
                let
                    newModel =
                        { model
                            | board = addMove i j turn board
                            , roundCount = roundCount + 1
                            , gameState = setGameState roundCount
                        }
                in
                ( newModel
                , Task.perform EndRound (Task.succeed (calculateRound newModel))
                )

            else
                ( model, Cmd.none )

        EndRound state ->
            ( { model
                | gameState = state
                , turn = changeTurn turn
              }
            , Cmd.none
            )

        RestartGame ->
            ( initModel, Cmd.none )


isGameOver : GameState -> Bool
isGameOver state =
    case state of
        Winner _ ->
            True

        Draw ->
            True

        _ ->
            False


changeTurn : Turn -> Turn
changeTurn turn =
    case turn of
        Player1 ->
            Player2

        Player2 ->
            Player1


setGameState : Int -> GameState
setGameState count =
    if count == 0 then
        NotStarted

    else
        Playing


addMove : Int -> Int -> Turn -> Board -> Board
addMove i_ j_ turn board =
    List.indexedMap
        (\i row ->
            List.indexedMap
                (\j cell ->
                    if i == i_ && j == j_ then
                        case turn of
                            Player1 ->
                                1

                            Player2 ->
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


calculateRound : Model -> GameState
calculateRound { board, turn, roundCount } =
    let
        isColumn =
            rotateBoard board |> isRow

        isDiagonalX =
            isDiagonal board

        isDiagonalY =
            board |> rotateBoard |> isDiagonal
    in
    if isRow board || isColumn || isDiagonalX || isDiagonalY then
        Winner turn

    else if roundCount == 9 then
        Draw

    else
        Playing



--Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--View


view : Model -> Html Msg
view { gameState, board } =
    div []
        [ h1 [] [ text "Tic-tac-toe" ]
        , h2 []
            [ text <|
                case gameState of
                    NotStarted ->
                        "Start game!"

                    Playing ->
                        "Playing ..."

                    Winner Player1 ->
                        "Player1 won!"

                    Winner Player2 ->
                        "Player2 won!"

                    Draw ->
                        "Draw!"
            ]
        , if isGameOver gameState then
            button [ onClick RestartGame ] [ text "Restart game" ]

          else
            div [] []
        , div [ class "board" ] <| renderBoard board
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
