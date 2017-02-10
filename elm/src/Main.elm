module Main exposing (..)

import Html exposing (Html, text, div)
import Collage as C exposing (defaultLine)
import Element as E
import Text
import Color
import AnimationFrame
import Keyboard
import Random
import List.Extra
import Process
import Time exposing (Time)
import Task exposing (Task)


type alias Model =
    GameState


type Msg
    = NoOp
    | Frame Time
    | KeyUp Int
    | KeyDown Int
    | LevelCreated Level
    | Resume ()


type alias PlayingState =
    { path : List Point
    , position : Point
    , direction : Direction
    , level : Level
    }


type alias WaitingState =
    { path : List Point
    , nextLevel : Level
    }


type GameState
    = Boot
    | Playing PlayingState
    | Waiting WaitingState
    | Sleeping WaitingState


type Direction
    = Up
    | Down


type alias Point =
    ( Float, Float )


type alias Star =
    { x : Float
    , y : Float
    , r : Float
    }


type alias Level =
    { level : Int
    , stars : List Star
    , door : Float
    , speed : Float
    , entry : Float
    , exit : Float
    }


canvas : { width : Int, height : Int }
canvas =
    { width = 800
    , height = 500
    }


canvasHalf =
    { width = (toFloat canvas.width) / 2
    , height = (toFloat canvas.height) / 2
    }


translate : Point -> Point
translate ( x, y ) =
    ( x - canvasHalf.width, y - canvasHalf.height )


translateMap : List Point -> List Point
translateMap =
    List.map translate


move ( x, y ) w h =
    let
        newX =
            x + w / 2

        newY =
            y + h / 2
    in
        C.move <| translate ( newX, newY )


runGenerator generator now =
    Random.initialSeed (round now)
        |> Random.step generator
        |> Tuple.first


type alias LevelValues =
    { stars : List Point
    , entry : Float
    , exit : Float
    }


generateLevel : Int -> Task Never Level
generateLevel level =
    let
        fLevel =
            toFloat level

        d =
            decay 15.0 25.0 fLevel

        door =
            decay 100.0 20.0 fLevel

        starGenerator =
            generateStars d

        entryGenerator =
            Random.float (50.0 + door / 2.0) (450.0 - door / 2.0)

        generator =
            Random.map3 LevelValues starGenerator entryGenerator entryGenerator
    in
        Time.now
            |> Task.map (runGenerator generator)
            |> Task.map (generateLevelHelp level)


generateLevelHelp : Int -> LevelValues -> Level
generateLevelHelp level randomValues =
    let
        fLevel =
            toFloat level

        door =
            decay 100.0 20.0 fLevel

        speed =
            decay 0.09 0.12 fLevel

        r =
            decay 3.0 15.0 fLevel

        stars =
            List.indexedMap (populateStarsHelp r) randomValues.stars
    in
        { level = level
        , stars = stars
        , door = door
        , speed = speed
        , entry = randomValues.entry
        , exit = randomValues.exit
        }


sleep ms =
    Task.perform Resume <| Process.sleep ms


init : ( Model, Cmd Msg )
init =
    let
        level =
            0
    in
        Boot
            ! [ Task.perform LevelCreated (generateLevel level) ]


randomPoint x y =
    Random.pair (Random.float x y) (Random.float x y)


generateStars : Float -> Random.Generator (List Point)
generateStars d =
    Random.list 45 <| randomPoint -d d


exp n =
    2.718 ^ n


decay at0 at7 n =
    (1.0 - exp (-1 * n / 2.5)) * (at7 - at0) + at0


clamp a max =
    if a > max then
        max
    else
        a


updateIfSpace : PlayingState -> Int -> Direction -> PlayingState
updateIfSpace state code target =
    if code == 32 then
        { state | direction = target }
    else
        state


populateStarsHelp : Float -> Int -> Point -> Star
populateStarsHelp r index ( dx, dy ) =
    let
        x =
            toFloat (index % 9)

        y =
            toFloat (index // 9)
    in
        { x = 100.0 + x * 75.0 + dx
        , y = 100.0 + y * 70.0 + dy
        , r = r
        }


toPoint : Maybe Point -> { x : Float, y : Float }
toPoint p =
    Maybe.map (\( x, y ) -> { x = x, y = y }) p
        |> Maybe.withDefault { x = -999, y = -999 }


len2 p =
    p.x * p.x + p.y * p.y


dist2 p1 p2 =
    len2 (subtract p2 p1)


subtract p1 p2 =
    { x = p1.x - p2.x, y = p1.y - p2.y }


testCollision level p1 p2 =
    if p1.y <= 50.0 then
        True
    else if p1.y >= 450.0 then
        True
    else if p1.x > 750.0 && (p1.y < level.exit - level.door / 2.0 || p1.y > level.exit + level.door / 2.0) then
        True
    else if List.any (\star -> dist2 p1 star < star.r * star.r) level.stars then
        True
    else
        False


testOutOfDoor level ( x, y ) =
    (x > 750.0 && y > level.exit - level.door / 2.0 && y < level.exit + level.door / 2.0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame t ->
            case model of
                Boot ->
                    model ! []

                Waiting _ ->
                    model ! []

                Sleeping _ ->
                    model ! []

                Playing state ->
                    let
                        level =
                            state.level

                        ( x, y ) =
                            state.position

                        dt =
                            clamp t 50.0

                        dy =
                            if state.direction == Up then
                                1.0
                            else
                                -1.0

                        position =
                            ( x + level.speed * dt, y + dy * level.speed * dt )

                        last =
                            List.Extra.last state.path

                        collision =
                            testCollision state.level (toPoint <| Just position) (toPoint last)

                        escaped =
                            testOutOfDoor state.level position

                        nextPath =
                            state.path ++ [ position ]

                        wState =
                            { nextLevel = level
                            , path = nextPath
                            }
                    in
                        if escaped then
                            Sleeping wState
                                ! [ Task.perform LevelCreated (generateLevel <| level.level + 1)
                                  , sleep 500
                                  ]
                        else if collision then
                            Sleeping wState
                                ! [ sleep 500 ]
                        else
                            Playing
                                { state
                                    | position = position
                                    , path = nextPath
                                }
                                ! []

        KeyDown code ->
            case model of
                Boot ->
                    model ! []

                Sleeping _ ->
                    model ! []

                Waiting state ->
                    let
                        level =
                            state.nextLevel

                        pos =
                            ( 50, level.entry )
                    in
                        if code == 32 then
                            Playing
                                { path = [ pos, pos ]
                                , position = pos
                                , direction = Down
                                , level = level
                                }
                                ! []
                        else
                            model ! []

                Playing state ->
                    (Playing <| updateIfSpace state code Down) ! []

        KeyUp code ->
            case model of
                Boot ->
                    model ! []

                Sleeping _ ->
                    model ! []

                Waiting _ ->
                    model ! []

                Playing state ->
                    (Playing <| updateIfSpace state code Up) ! []

        LevelCreated level ->
            let
                newState =
                    { nextLevel = level
                    , path = []
                    }
            in
                case model of
                    Boot ->
                        Waiting newState ! []

                    Sleeping _ ->
                        Sleeping newState ! []

                    Waiting _ ->
                        Waiting newState ! []

                    Playing _ ->
                        Debug.crash "updating level while playing?"

        Resume () ->
            case model of
                Sleeping state ->
                    Waiting state
                        ! []

                _ ->
                    Debug.crash "Should not resume if not sleeping"

        NoOp ->
            model ! []


shape =
    C.filled Color.black <| C.rect 800 500


star c =
    C.filled c <| C.circle 30


generate : Int -> Float -> List Point
generate x y =
    List.range 0 x
        |> List.map (\val -> translate ( toFloat val, y ))


pathForm path =
    C.traced (C.solid Color.yellow) (C.path path)


border x =
    C.outlined { defaultLine | color = Color.blue }
        (C.rect (700 + x) (400 + x))


scene path stars doors levelText introText =
    C.collage 800 500 <|
        [ shape
        , (border 0)
        , (border 10)
        , pathForm path
        ]
            ++ stars
            ++ doors
            ++ levelText
            ++ introText


drawStar { x, y, r } =
    C.circle r
        |> C.filled Color.blue
        |> move ( x, y ) 1 1


drawDoors door entry exit =
    let
        base =
            C.rect 5 door |> C.filled Color.green

        left =
            base |> move ( 45, (entry - door / 2.0) ) 5 door

        right =
            base |> move ( 750, (exit - door / 2.0) ) 5 door
    in
        [ left, right ]


drawLevelText { level } =
    let
        l =
            toString <| level + 1

        el =
            Text.fromString ("Level: " ++ l)
                |> Text.color Color.blue
                |> Text.monospace
                |> Text.height 20
                |> E.leftAligned

        width =
            toFloat <| E.widthOf el
    in
        el
            |> C.toForm
            |> move ( 50, 470 ) width 0
            |> List.singleton


drawIntroText text =
    Text.fromString text
        |> Text.color Color.yellow
        |> Text.monospace
        |> Text.height 20
        |> E.centered
        |> C.toForm
        |> List.singleton


render : Level -> List Point -> Maybe String -> E.Element
render l path text =
    let
        translated =
            List.map translate path

        stars =
            List.map drawStar l.stars

        doors =
            drawDoors l.door l.entry l.exit

        introText =
            Maybe.map drawIntroText text
                |> Maybe.withDefault []

        position =
            E.middleAt (E.relative 0.5) (E.relative 0.5)

        result =
            scene translated stars doors (drawLevelText l) introText
    in
        E.container canvas.width canvas.height position result


view : Model -> Html Msg
view model =
    case model of
        Boot ->
            text ""

        Sleeping state ->
            render state.nextLevel
                state.path
                (Just "Press Space to Start")
                |> E.toHtml

        Waiting state ->
            render
                state.nextLevel
                state.path
                (Just "Press Space to Start")
                |> E.toHtml

        Playing state ->
            render state.level
                state.path
                Nothing
                |> E.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Frame
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]


main : Program Never Model Msg
main =
    Html.program { view = view, init = init, update = update, subscriptions = subscriptions }
