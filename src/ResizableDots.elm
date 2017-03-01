module ResizableDots exposing (..)

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
    , others : List ( Position, Int )
    , bending : List ( Position, Int )
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
            , others = []
            , bending = []
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
update msg ({ window, others, bending, bend } as model) =
    case msg of
        Window ws ->
            ( { model | window = ws }
            , Cmd.none
            )

        ClickStart pos ->
            let
                newBending =
                    case List.filter (\( p, s ) -> isTouched p s pos) others of
                        [] ->
                            [ ( pos, 0 ) ]

                        a ->
                            a

                newOthers =
                    List.filter (\o -> not <| List.any ((==) o) newBending) others

                newBend =
                    Just (Bend pos pos)
            in
                ( { model
                    | others = newOthers
                    , bending = newBending
                    , bend = newBend
                  }
                , Cmd.none
                )

        ClickAt pos ->
            case bend of
                Just { start, current } ->
                    let
                        ds st =
                            let
                                m =
                                    current.x - st.x

                                n =
                                    current.y - st.y
                            in
                                (round << sqrt << toFloat) (m * m + n * n)

                        newBending =
                            List.map (\( p, s ) -> ( p, ds p )) bending

                        newBend =
                            Maybe.map (\b -> Bend b.start pos) bend
                    in
                        ( { model
                            | bending = newBending
                            , bend = newBend
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        ClickEnd pos ->
            let
                newOthers =
                    bending ++ others
            in
                ( { model
                    | bend = Nothing
                    , bending = []
                    , others = newOthers
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
view ({ window, bending, others } as model) =
    collage
        window.width
        window.height
        (List.map viewDot (bending ++ others))


viewDot : ( Position, Int ) -> Form
viewDot ( { x, y }, s ) =
    circle (toFloat s)
        |> filled Color.black
        |> move ( toFloat x, toFloat y )
