module Main exposing (main)

import Browser
import Browser.Events
import Camera
import Game
import Html exposing (Html, button, div, h1, main_, p, section, strong, text, video)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Pose
import Random
import Svg
import Svg.Attributes as SA
import Task
import Time


type Model
    = Landing
    | Loading
    | Failed String
    | Shuffling
    | Running Game.Game
    | Debugging DebugState


type alias DebugState =
    { game : Game.Game
    , selected : Int
    , poses : List Camera.RawPose
    , sampling : Bool
    }


type Msg
    = Start
    | CameraStarted (Result String ())
    | Shuffled (List Pose.Target)
    | GameStarted (List Pose.Target) Time.Posix
    | Tick Time.Posix
    | PosesSampled Int (Maybe String) (Result String (List Camera.RawPose))
    | DebugPosesSampled (Result String (List Camera.RawPose))
    | SelectDebugPose Int
    | ToggleDebug


type alias SkeletonPoint =
    { x : Float
    , y : Float
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Landing, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( Loading
            , Camera.startCamera { maxPoses = 4 }
                |> Task.attempt CameraStarted
            )

        CameraStarted (Ok ()) ->
            ( Shuffling, Random.generate Shuffled (Pose.randomTargets 12) )

        CameraStarted (Err error) ->
            ( Failed (friendlyCameraError error), Cmd.none )

        Shuffled targets ->
            ( Shuffling, Task.perform (GameStarted targets) Time.now )

        GameStarted targets now ->
            ( Running (Game.begin (Time.posixToMillis now) targets), Cmd.none )

        Tick now ->
            case model of
                Running game ->
                    advance now game

                Debugging debug ->
                    if debug.sampling then
                        ( model, Cmd.none )

                    else
                        ( Debugging { debug | sampling = True }
                        , Camera.samplePoses now |> Task.attempt DebugPosesSampled
                        )

                _ ->
                    ( model, Cmd.none )

        PosesSampled capturedAt requestedTarget result ->
            case ( model, result ) of
                ( Running game, Ok poses ) ->
                    ( Running (Game.recordPoses capturedAt requestedTarget poses game), Cmd.none )

                ( Running _, Err error ) ->
                    ( Failed (friendlyCameraError error), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DebugPosesSampled result ->
            case ( model, result ) of
                ( Debugging debug, Ok poses ) ->
                    ( Debugging { debug | poses = poses, sampling = False }, Cmd.none )

                ( Debugging _, Err error ) ->
                    ( Failed (friendlyCameraError error), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectDebugPose index ->
            case model of
                Debugging debug ->
                    ( Debugging { debug | selected = modBy (List.length Pose.allTargets) index }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleDebug ->
            case model of
                Running game ->
                    ( Debugging
                        { game = { game | sampling = False }
                        , selected = 0
                        , poses = game.poses
                        , sampling = False
                        }
                    , Cmd.none
                    )

                Debugging debug ->
                    let
                        game =
                            debug.game
                    in
                    ( Running { game | poses = debug.poses, sampling = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


advance : Time.Posix -> Game.Game -> ( Model, Cmd Msg )
advance now game =
    case Game.tick (Time.posixToMillis now) game of
        Game.Restart ->
            shuffle

        Game.Continue advanced ->
            case advanced.phase of
                Game.Results _ ->
                    ( Running advanced, Cmd.none )

                _ ->
                    if advanced.sampling then
                        ( Running advanced, Cmd.none )

                    else
                        let
                            requestedTarget =
                                Game.currentPose advanced |> Maybe.map .id

                            capturedAt =
                                Time.posixToMillis now
                        in
                        ( Running { advanced | sampling = True }
                        , Camera.samplePoses now
                            |> Task.attempt (PosesSampled capturedAt requestedTarget)
                        )


shuffle : ( Model, Cmd Msg )
shuffle =
    ( Shuffling, Random.generate Shuffled (Pose.randomTargets 12) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown debugKey
        , case model of
            Running _ ->
                Time.every 100 Tick

            Debugging _ ->
                Time.every 100 Tick

            _ ->
                Sub.none
        ]


debugKey : Decode.Decoder Msg
debugKey =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "repeat" Decode.bool)
        |> Decode.andThen
            (\( key, repeated ) ->
                if String.toLower key == "d" && not repeated then
                    Decode.succeed ToggleDebug

                else
                    Decode.fail "Not the debug shortcut"
            )


view : Model -> Html Msg
view model =
    main_ [ HA.class "relative h-dvh w-screen overflow-hidden bg-zinc-950 text-white" ]
        [ video
            [ HA.id "camera"
            , HA.autoplay True
            , HA.attribute "playsinline" ""
            , HA.attribute "muted" ""
            , HA.class "absolute inset-0 h-full w-full -scale-x-100 object-cover"
            ]
            []
        , case model of
            Running game ->
                skeletons game.poses

            Debugging debug ->
                skeletons debug.poses

            _ ->
                text ""
        , div [ HA.class "pointer-events-none absolute inset-0 bg-[linear-gradient(to_bottom,rgba(0,0,0,.55),transparent_24%,transparent_62%,rgba(0,0,0,.72))]" ] []
        , case model of
            Running game ->
                gameHud game

            Debugging debug ->
                debugHud debug

            _ ->
                text ""
        , cameraOverlay model
        ]


skeletons : List Camera.RawPose -> Html Msg
skeletons poses =
    Svg.svg
        [ SA.viewBox "0 0 16 9"
        , SA.preserveAspectRatio "xMidYMid slice"
        , SA.class "pointer-events-none absolute inset-0 h-full w-full -scale-x-100"
        , HA.attribute "aria-hidden" "true"
        ]
        (List.indexedMap skeleton poses)


skeleton : Int -> Camera.RawPose -> Svg.Svg Msg
skeleton index rawPose =
    let
        color =
            playerColor index

        bone stroke width ( fromIndex, toIndex ) =
            case ( Pose.landmarkAt fromIndex rawPose, Pose.landmarkAt toIndex rawPose ) of
                ( Just from, Just to ) ->
                    if from.visibility >= 0.5 && to.visibility >= 0.5 then
                        -- Landmarks are in frame-height units, so both axes scale by the viewBox height.
                        Svg.line
                            [ SA.x1 (String.fromFloat (from.x * 9))
                            , SA.y1 (String.fromFloat (from.y * 9))
                            , SA.x2 (String.fromFloat (to.x * 9))
                            , SA.y2 (String.fromFloat (to.y * 9))
                            , SA.stroke stroke
                            , SA.strokeWidth width
                            , SA.strokeLinecap "round"
                            ]
                            []

                    else
                        Svg.text ""

                _ ->
                    Svg.text ""

        joint jointIndex =
            case Pose.landmarkAt jointIndex rawPose of
                Just landmark ->
                    if landmark.visibility >= 0.5 then
                        Svg.circle
                            [ SA.cx (String.fromFloat (landmark.x * 9))
                            , SA.cy (String.fromFloat (landmark.y * 9))
                            , SA.r "0.09"
                            , SA.fill color
                            , SA.stroke "#09090b"
                            , SA.strokeWidth "0.025"
                            ]
                            []

                    else
                        Svg.text ""

                Nothing ->
                    Svg.text ""
    in
    Svg.g []
        (List.map (bone "#09090b" "0.10") Pose.skeleton
            ++ List.map (bone color "0.075") Pose.skeleton
            ++ List.map joint skeletonJoints
        )


skeletonJoints : List Int
skeletonJoints =
    [ 11, 12, 13, 14, 15, 16, 23, 24, 25, 26, 27, 28 ]


cameraOverlay : Model -> Html Msg
cameraOverlay model =
    case model of
        Landing ->
            splashScreen "Lusterko" "Move · Match · Score" playButton

        Loading ->
            splashScreen "Getting ready" "Allow camera access" loader

        Failed error ->
            statusOverlay "Camera check needed" error True

        Shuffling ->
            splashScreen "New game" "Shuffling poses" loader

        Running game ->
            if List.isEmpty game.poses then
                div [ HA.class "pointer-events-none absolute bottom-36 left-1/2 -translate-x-1/2 rounded-full border border-white/15 bg-black/60 px-4 py-2 text-center text-xl font-bold backdrop-blur-md" ]
                    [ text "Step back so your whole body is visible" ]

            else
                text ""

        Debugging _ ->
            text ""


{-| Full-screen splash shared by the landing, loading and shuffling states.
The slot below the subtitle holds either the Play button or the loader.
-}
splashScreen : String -> String -> Html Msg -> Html Msg
splashScreen title subtitle slot =
    div
        [ HA.class "absolute inset-0 grid place-items-center bg-zinc-950 p-6 text-center"
        , HA.attribute "role" "status"
        , HA.attribute "aria-live" "polite"
        ]
        [ div [ HA.class "flex flex-col items-center" ]
            [ h1 [ HA.class "text-[12vw] font-black text-white sm:text-[10vw] lg:text-[8rem]" ] [ text title ]
            , p [ HA.class "text-sm font-bold uppercase tracking-[.35em] text-white/40 sm:text-base" ] [ text subtitle ]
            , slot
            ]
        ]


playButton : Html Msg
playButton =
    button
        [ onClick Start
        , HA.class "start-pulse mt-12 grid size-28 shrink-0 place-items-center rounded-full bg-fuchsia-300 text-sm font-black uppercase tracking-[.2em] text-zinc-950 transition hover:scale-105 hover:bg-fuchsia-200 focus-visible:outline-2 focus-visible:outline-offset-4 focus-visible:outline-fuchsia-300 sm:size-36 sm:text-base"
        ]
        [ text "Play" ]


loader : Html msg
loader =
    div
        [ HA.class "mt-12 size-28 shrink-0 animate-spin rounded-full border-[6px] border-white/10 border-t-fuchsia-300 sm:size-36 sm:border-8"
        , HA.attribute "aria-label" "Loading"
        ]
        []


statusOverlay : String -> String -> Bool -> Html Msg
statusOverlay title body retry =
    div
        [ HA.class "absolute inset-0 grid place-items-center bg-zinc-950/90 p-6 text-center backdrop-blur-sm"
        , HA.attribute "role" "status"
        , HA.attribute "aria-live" "polite"
        ]
        [ div [ HA.class "max-w-md" ]
            [ p [ HA.class "mb-3 text-xs font-bold tracking-[.3em] text-emerald-300" ] [ text "LUSTERKO" ]
            , h1 [ HA.class "text-4xl font-black tracking-[-.04em]" ] [ text title ]
            , p [ HA.class "mx-auto mt-4 max-w-sm leading-7 text-white/60" ] [ text body ]
            , if retry then
                button
                    [ onClick Start
                    , HA.class "mt-7 min-h-14 rounded-2xl bg-emerald-300 px-8 font-black text-zinc-950 hover:bg-emerald-200"
                    ]
                    [ text "Try again" ]

              else
                div [ HA.class "mx-auto mt-7 size-7 animate-spin rounded-full border-2 border-white/20 border-t-emerald-300" ] []
            ]
        ]


debugHud : DebugState -> Html Msg
debugHud debug =
    case Pose.allTargets |> List.drop debug.selected |> List.head of
        Nothing ->
            text ""

        Just target ->
            let
                matched =
                    List.any (Pose.matches target) debug.poses

                closest =
                    debug.poses
                        |> List.filterMap (Pose.matchError target)
                        |> List.sortBy (\error -> error.average + error.worst)
                        |> List.head

                errorText =
                    case closest of
                        Just error ->
                            "avg " ++ String.fromInt (round error.average) ++ "° · worst " ++ String.fromInt (round error.worst) ++ "°"

                        Nothing ->
                            "no full body in view"

                statusClass =
                    if matched then
                        " bg-emerald-300 text-zinc-950"

                    else
                        " bg-black/60 text-white"
            in
            section
                [ HA.class "pointer-events-none absolute inset-0"
                , HA.attribute "aria-live" "polite"
                ]
                [ div [ HA.class "absolute left-4 top-4 rounded-full bg-black/60 px-5 py-3 text-sm font-black uppercase tracking-[.25em] backdrop-blur-sm sm:left-6 sm:top-6" ]
                    [ text "Debug · D to exit" ]
                , div [ HA.class "absolute right-4 top-4 flex flex-col items-center sm:right-6 sm:top-6" ]
                    [ div [ HA.class "flex size-64 flex-col items-center rounded-[2rem] bg-black/60 shadow-xl backdrop-blur-sm sm:size-80 lg:size-96" ]
                        [ targetSkeleton "min-h-0 w-full flex-1 p-6 pb-0" target
                        , div [ HA.class "pointer-events-auto flex shrink-0 items-center gap-3 pb-4" ]
                            [ button
                                [ onClick (SelectDebugPose (debug.selected - 1))
                                , HA.class "grid size-10 place-items-center rounded-full bg-white text-2xl font-black text-zinc-950 hover:bg-fuchsia-200 sm:size-12 sm:text-3xl"
                                , HA.attribute "aria-label" "Previous pose"
                                ]
                                [ text "‹" ]
                            , div [ HA.class "min-w-32 text-center text-sm font-black uppercase tracking-[.2em] text-fuchsia-300 sm:min-w-36 sm:text-base" ]
                                [ text ("Pose " ++ String.fromInt (debug.selected + 1) ++ "/" ++ String.fromInt (List.length Pose.allTargets)) ]
                            , button
                                [ onClick (SelectDebugPose (debug.selected + 1))
                                , HA.class "grid size-10 place-items-center rounded-full bg-white text-2xl font-black text-zinc-950 hover:bg-fuchsia-200 sm:size-12 sm:text-3xl"
                                , HA.attribute "aria-label" "Next pose"
                                ]
                                [ text "›" ]
                            ]
                        ]
                    ]
                , div
                    [ HA.class ("absolute bottom-6 left-1/2 -translate-x-1/2 rounded-[2rem] px-10 py-5 text-center text-5xl font-black uppercase tracking-[-.04em] shadow-xl backdrop-blur-sm sm:text-7xl" ++ statusClass)
                    , HA.attribute "role" "status"
                    ]
                    [ text
                        (if matched then
                            "Match"

                         else
                            "No match"
                        )
                    , div [ HA.class "mt-2 text-base font-bold normal-case tracking-normal opacity-70 sm:text-lg" ] [ text errorText ]
                    ]
                ]


gameHud : Game.Game -> Html Msg
gameHud game =
    section
        [ HA.class "pointer-events-none absolute inset-0 flex flex-col justify-between p-4 sm:p-6"
        , HA.attribute "aria-live" "polite"
        ]
        [ targetPoseOverlay game
        , scoreBoard game
        ]


targetPoseOverlay : Game.Game -> Html Msg
targetPoseOverlay game =
    case game.phase of
        Game.Countdown deadline _ ->
            div
                [ HA.class "absolute inset-0 z-20 grid place-items-center bg-black/20 text-center backdrop-blur-[2px]"
                , HA.attribute "role" "timer"
                ]
                [ div []
                    [ div [ HA.class "text-[10rem] font-black leading-none tabular-nums text-white drop-shadow-2xl sm:text-[16rem]" ] [ text (secondsLeft game.now deadline) ] ]
                ]

        Game.Playing target deadline ->
            div [ HA.class "contents" ]
                [ div [ HA.class "absolute right-4 top-4 flex flex-col items-center sm:right-6 sm:top-6" ]
                    [ div [ HA.class "flex size-64 flex-col items-center rounded-[2rem] bg-black/35 shadow-xl backdrop-blur-sm sm:size-80 lg:size-96 xl:size-[28rem]" ]
                        [ targetSkeleton "min-h-0 w-full flex-1 p-7 pb-1" target
                        , div [ HA.class "shrink-0 pb-5 text-sm font-black uppercase tracking-[.2em] text-fuchsia-300 sm:text-base" ]
                            [ text ("Pose " ++ String.fromInt game.shown ++ "/" ++ String.fromInt game.total) ]
                        ]
                    ]
                , div [ HA.class "absolute bottom-4 right-4 z-10 min-w-40 px-8 py-4 text-center text-8xl font-black leading-none tabular-nums text-white shadow-xl backdrop-blur-sm sm:bottom-6 sm:right-6 sm:min-w-56 sm:text-[10rem]" ]
                    [ text (secondsLeft game.now deadline) ]
                ]

        Game.Celebrating _ scorers ->
            div
                [ HA.class "absolute inset-0 z-20 flex text-center"
                , HA.attribute "role" "status"
                , HA.attribute "aria-label" (scorerText scorers)
                ]
                (List.map
                    (\player ->
                        div
                            [ HA.class "grid min-w-0 flex-1 place-items-center"
                            , HA.style "background" (playerColor (player - 1))
                            , HA.attribute "aria-hidden" "true"
                            ]
                            [ div [ HA.class "text-[9rem] font-black leading-none text-zinc-950 sm:text-[14rem]" ] [ text "+1" ] ]
                    )
                    scorers
                )

        Game.Results deadline ->
            div
                [ HA.class "absolute inset-0 z-20 flex bg-zinc-950 text-center"
                , HA.attribute "role" "status"
                ]
                ([ div [ HA.class "absolute left-1/2 top-6 z-10 -translate-x-1/2 rounded-full bg-black/35 px-8 py-3 text-2xl font-black tracking-widest text-white backdrop-blur-sm sm:text-4xl" ] [ text (resultHeading game.players) ]
                 , div
                    [ HA.class "absolute bottom-6 left-1/2 z-10 -translate-x-1/2 rounded-2xl bg-black/35 px-6 py-3 text-4xl font-black tabular-nums text-white backdrop-blur-sm"
                    , HA.attribute "aria-label" ("New game in " ++ secondsLeft game.now deadline ++ " seconds")
                    ]
                    [ text (secondsLeft game.now deadline) ]
                 ]
                    ++ List.indexedMap
                        (\index player ->
                            div
                                [ HA.class "grid min-w-0 flex-1 place-items-center"
                                , HA.style "background" (playerColor index)
                                , HA.attribute "aria-label" (playerName index ++ " final score " ++ String.fromInt player.score)
                                ]
                                [ div [ HA.class "text-7xl font-black tabular-nums text-zinc-950 sm:text-9xl lg:text-[10rem]" ] [ text (String.fromInt player.score) ] ]
                        )
                        game.players
                )


targetSkeleton : String -> Pose.Target -> Html Msg
targetSkeleton className target =
    case target.expected of
        [ torso, leftUpperArm, leftLowerArm, rightUpperArm, rightLowerArm, leftThigh, leftShin, rightThigh, rightShin ] ->
            let
                hipsMiddle =
                    { x = 50, y = 57 }

                shouldersMiddle =
                    endpoint hipsMiddle 24 torso

                leftShoulder =
                    { x = shouldersMiddle.x - 9, y = shouldersMiddle.y }

                rightShoulder =
                    { x = shouldersMiddle.x + 9, y = shouldersMiddle.y }

                leftHip =
                    { x = hipsMiddle.x - 4, y = hipsMiddle.y }

                rightHip =
                    { x = hipsMiddle.x + 4, y = hipsMiddle.y }

                leftElbow =
                    endpoint leftShoulder 17 leftUpperArm

                leftWrist =
                    endpoint leftElbow 16 leftLowerArm

                rightElbow =
                    endpoint rightShoulder 17 rightUpperArm

                rightWrist =
                    endpoint rightElbow 16 rightLowerArm

                leftKnee =
                    endpoint leftHip 20 leftThigh

                leftAnkle =
                    endpoint leftKnee 22 leftShin

                rightKnee =
                    endpoint rightHip 20 rightThigh

                rightAnkle =
                    endpoint rightKnee 22 rightShin

                segments =
                    [ ( leftShoulder, rightShoulder )
                    , ( leftShoulder, leftHip )
                    , ( rightShoulder, rightHip )
                    , ( leftHip, rightHip )
                    , ( leftShoulder, leftElbow )
                    , ( leftElbow, leftWrist )
                    , ( rightShoulder, rightElbow )
                    , ( rightElbow, rightWrist )
                    , ( leftHip, leftKnee )
                    , ( leftKnee, leftAnkle )
                    , ( rightHip, rightKnee )
                    , ( rightKnee, rightAnkle )
                    ]

                joints =
                    [ leftShoulder, rightShoulder, leftElbow, rightElbow, leftWrist, rightWrist, leftHip, rightHip, leftKnee, rightKnee, leftAnkle, rightAnkle ]

                bone stroke width ( from, to ) =
                    Svg.line
                        [ SA.x1 (String.fromFloat from.x)
                        , SA.y1 (String.fromFloat from.y)
                        , SA.x2 (String.fromFloat to.x)
                        , SA.y2 (String.fromFloat to.y)
                        , SA.stroke stroke
                        , SA.strokeWidth width
                        , SA.strokeLinecap "round"
                        ]
                        []

                joint point =
                    Svg.circle
                        [ SA.cx (String.fromFloat point.x)
                        , SA.cy (String.fromFloat point.y)
                        , SA.r "1.15"
                        , SA.fill "#f0abfc"
                        , SA.stroke "#09090b"
                        , SA.strokeWidth "0.45"
                        ]
                        []
            in
            Svg.svg
                [ SA.viewBox "0 0 100 100"
                , SA.class className
                , HA.attribute "aria-hidden" "true"
                ]
                (List.map (bone "#09090b" "2.4") segments
                    ++ List.map (bone "#f0abfc" "1.25") segments
                    ++ List.map joint joints
                )

        _ ->
            text ""


endpoint : SkeletonPoint -> Float -> Float -> SkeletonPoint
endpoint from length angle =
    let
        radians =
            angle * pi / 180
    in
    { x = from.x + length * cos radians
    , y = from.y - length * sin radians
    }


scoreBoard : Game.Game -> Html Msg
scoreBoard game =
    div [ HA.class "mt-auto flex flex-wrap gap-2", HA.attribute "aria-label" "Scores" ]
        (if List.isEmpty game.players then
            [ div
                [ HA.class "size-3 animate-pulse rounded-full bg-white/40"
                , HA.attribute "aria-label" "Finding players"
                ]
                []
            ]

         else
            List.indexedMap (playerCard game) game.players
        )


playerCard : Game.Game -> Int -> Game.Player -> Html Msg
playerCard game index player =
    let
        opacity =
            if player.active then
                " opacity-100"

            else
                " opacity-45"

        progress =
            round (Game.holdFraction game.now player * 100)

        color =
            playerColor index

        matching =
            if player.matching then
                " scale-110"

            else
                ""
    in
    div
        [ HA.class ("min-w-24 rounded-3xl px-5 py-4 text-zinc-950 shadow-xl transition sm:min-w-32 sm:px-6 sm:py-5" ++ opacity ++ matching)
        , HA.style "background" color
        , HA.attribute "aria-label" (playerName index ++ " score " ++ String.fromInt player.score)
        ]
        [ strong [ HA.class "block text-center text-5xl font-black leading-none tabular-nums sm:text-7xl" ] [ text (String.fromInt player.score) ]
        , div [ HA.class "mt-4 h-1.5 overflow-hidden rounded-full bg-black/20" ]
            [ div
                [ HA.class "h-full rounded-full bg-zinc-950"
                , HA.style "width" (String.fromInt progress ++ "%")
                ]
                []
            ]
        ]


secondsLeft : Int -> Int -> String
secondsLeft now deadline =
    String.fromInt (ceiling (toFloat (max 0 (deadline - now)) / 1000))


scorerText : List Int -> String
scorerText scorers =
    let
        names =
            List.map (\player -> playerName (player - 1)) scorers
    in
    case names of
        [ name ] ->
            name ++ " scores"

        _ ->
            String.join " & " names ++ " score"


resultHeading : List Game.Player -> String
resultHeading players =
    case List.maximum (List.map .score players) of
        Nothing ->
            "NO PLAYERS"

        Just 0 ->
            "NO SCORE"

        Just highScore ->
            if List.length (List.filter (\player -> player.score == highScore) players) == 1 then
                "WINNER"

            else
                "TIE"


playerName : Int -> String
playerName index =
    case modBy 4 index of
        0 ->
            "GREEN"

        1 ->
            "YELLOW"

        2 ->
            "PINK"

        _ ->
            "BLUE"


playerColor : Int -> String
playerColor index =
    case modBy 4 index of
        0 ->
            "#34d399"

        1 ->
            "#fbbf24"

        2 ->
            "#fb7185"

        _ ->
            "#60a5fa"


friendlyCameraError : String -> String
friendlyCameraError error =
    if String.contains "NotAllowedError" error || String.contains "Permission" error then
        "Camera access was blocked. Allow it in your browser’s site settings, then try again."

    else if String.contains "NotFoundError" error then
        "No webcam was found. Connect one, then try again."

    else
        "The camera or pose model could not start. Check your connection and camera, then try again. " ++ error
