module Waves exposing (..)

import Collage exposing (Form, circle, collage, filled, move)
import Color
import Element exposing (Element, toHtml)
import Html exposing (Attribute)
import Mouse exposing (Position)
import Task
import Transform exposing (..)
import Window exposing (Size)
import Tuple exposing (first)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions =
            subscriptions
        , view =
            toHtml << view
            -- , view = \m -> Html.text <| toString m.bend
        }


subscriptions : Model -> Sub Msg
subscriptions ({ bend, window } as model) =
    case bend of
        Just _ ->
            Sub.batch
                [ Mouse.moves (fixPosition window ClickAt)
                , Mouse.ups (fixPosition window ClickEnd)
                ]

        Nothing ->
            Sub.batch
                [ Window.resizes Window
                , Mouse.downs (fixPosition window ClickStart)
                ]



-- HELPERS


fixPosition : Size -> (Position -> msg) -> (Position -> msg)
fixPosition { width, height } msg =
    \{ x, y } ->
        msg
            { x = x - width // 2
            , y = height // 2 - y
            }



-- MODEL


type alias Model =
    { window : Size
    , mouse : Position
    , dots : List ( Position, Int )
    , bend : Maybe Bend
    }


type alias Bend =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { window = Size 0 0
            , mouse = Position 0 0
            , dots = []
            , bend = Nothing
            }

        cmds =
            Task.perform Window Window.size
    in
        ( model, cmds )



-- UPDATE


type Msg
    = Window Size
    | ClickStart Position
    | ClickAt Position
    | ClickEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ window, mouse, dots, bend } as model) =
    case msg of
        Window ws ->
            ( { model | window = ws }
            , Cmd.none
            )

        ClickStart pos ->
            let
                ( start, s ) =
                    List.filter (\( p, s ) -> isTouched p s pos) dots
                        |> List.head
                        |> Maybe.withDefault ( pos, 0 )

                newDots =
                    ( start, s ) :: (List.filter ((/=) start << first) dots)

                newBend =
                    Just (Bend start pos)
            in
                ( { model
                    | dots = newDots
                    , bend = newBend
                  }
                , Cmd.none
                )

        ClickAt pos ->
            case bend of
                Just { start, current } ->
                    let
                        ds =
                            let
                                m =
                                    current.x - start.x

                                n =
                                    current.y - start.y
                            in
                                (round << sqrt << toFloat) (m * m + n * n)

                        newDots =
                            List.map
                                (\( p, s ) ->
                                    if isTouched start s p then
                                        ( p, ds )
                                    else
                                        ( p, s )
                                )
                                dots

                        newBend =
                            Maybe.map (\b -> Bend b.start pos) bend
                    in
                        ( { model
                            | dots = newDots
                            , bend = newBend
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        ClickEnd pos ->
            let
                newBend =
                    Nothing
            in
                ( { model
                    | bend = newBend
                  }
                , Cmd.none
                )



-- HELPERS


isTouched : Position -> Int -> Position -> Bool
isTouched element s { x, y } =
    let
        m =
            x - element.x

        n =
            y - element.y
    in
        (sqrt << toFloat) (m * m + n * n) <= (toFloat s)



-- VIEW


view : Model -> Element
view ({ window, dots } as model) =
    collage
        window.width
        window.height
        (List.map viewDot dots)


viewDot : ( Position, Int ) -> Form
viewDot ( { x, y }, s ) =
    circle (toFloat s)
        |> filled Color.black
        |> move ( toFloat x, toFloat y )
