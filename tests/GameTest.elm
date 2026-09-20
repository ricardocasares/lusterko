module GameTest exposing (tests)

import Camera
import Expect
import Game
import Json.Decode as Decode
import Json.Encode as Encode
import Pose
import Random
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "Lusterko"
        [ test "each game generates twelve complete poses" <|
            \_ ->
                let
                    targets =
                        generatedTargets 42
                in
                Expect.equal ( 12, True )
                    ( List.length targets
                    , List.all (\target -> List.length target.expected == 9) targets
                    )
        , test "different games generate different pose combinations" <|
            \_ ->
                Expect.notEqual
                    (List.map .expected (generatedTargets 42))
                    (List.map .expected (generatedTargets 43))
        , test "countdown reveals a pose for five seconds" <|
            \_ ->
                case Game.tick 3000 (Game.begin 0 (List.take 2 (generatedTargets 42))) of
                    Game.Continue game ->
                        case game.phase of
                            Game.Playing _ deadline ->
                                Expect.equal ( 1, 8000 ) ( game.shown, deadline )

                            _ ->
                                Expect.fail "expected an active round"

                    Game.Restart ->
                        Expect.fail "restarted too early"
        , test "a timed-out final pose shows results and restarts after five seconds" <|
            \_ ->
                let
                    active =
                        startSingle syntheticTarget
                in
                case Game.tick 8000 active of
                    Game.Continue resultGame ->
                        case ( resultGame.phase, Game.tick 13000 resultGame ) of
                            ( Game.Results 13000, Game.Restart ) ->
                                Expect.pass

                            _ ->
                                Expect.fail "expected five-second results then restart"

                    Game.Restart ->
                        Expect.fail "restarted without results"
        , test "two players can score on the same earliest sample" <|
            \_ ->
                let
                    first =
                        Game.recordPoses 3100 (Just syntheticTarget.id) [ syntheticPose, shiftedPose 0.2 syntheticPose ] (startSingle syntheticTarget)

                    scored =
                        Game.recordPoses 3600 (Just syntheticTarget.id) [ syntheticPose, shiftedPose 0.2 syntheticPose ] first
                in
                case scored.phase of
                    Game.Celebrating _ [ 1, 2 ] ->
                        Expect.equal [ 1, 1 ] (List.map .score scored.players)

                    _ ->
                        Expect.fail "expected both players to score"
        , test "player slots grow, stay left-to-right, and never shrink" <|
            \_ ->
                let
                    two =
                        Game.recordPoses 100 Nothing [ syntheticPose, shiftedPose 0.2 syntheticPose ] (Game.begin 0 [ syntheticTarget ])

                    one =
                        Game.recordPoses 200 Nothing [ syntheticPose ] two
                in
                Expect.equal
                    ( [ 0.7, 0.5 ], [ True, False ] )
                    ( List.map (Pose.centerX >> round2) two.poses
                    , List.map .active one.players
                    )
        , test "low-quality pose candidates do not create player slots" <|
            \_ ->
                let
                    game =
                        Game.recordPoses 100 Nothing [ syntheticPose, lowConfidencePose ] (Game.begin 0 [ syntheticTarget ])
                in
                Expect.equal ( 1, 1 ) ( List.length game.poses, List.length game.players )
        , test "a mismatch resets the half-second hold" <|
            \_ ->
                let
                    started =
                        Game.recordPoses 3100 (Just syntheticTarget.id) [ syntheticPose ] (startSingle syntheticTarget)

                    reset =
                        Game.recordPoses 3400 (Just syntheticTarget.id) [ lowConfidencePose ] started

                    restarted =
                        Game.recordPoses 4000 (Just syntheticTarget.id) [ syntheticPose ] reset

                    tooSoon =
                        Game.recordPoses 4300 (Just syntheticTarget.id) [ syntheticPose ] restarted
                in
                Expect.equal [ 0 ] (List.map .score tooSoon.players)
        , test "a hold completed at the deadline scores, one completed after it does not" <|
            \_ ->
                let
                    atDeadline =
                        startSingle syntheticTarget
                            |> Game.recordPoses 7400 (Just syntheticTarget.id) [ syntheticPose ]
                            |> Game.recordPoses 8000 (Just syntheticTarget.id) [ syntheticPose ]

                    afterDeadline =
                        startSingle syntheticTarget
                            |> Game.recordPoses 7400 (Just syntheticTarget.id) [ syntheticPose ]
                            |> Game.recordPoses 8001 (Just syntheticTarget.id) [ syntheticPose ]
                in
                Expect.equal ( [ 1 ], [ 0 ] )
                    ( List.map .score atDeadline.players, List.map .score afterDeadline.players )
        , test "pose tolerance accepts the boundary neighborhood and rejects large errors" <|
            \_ ->
                let
                    target =
                        { syntheticTarget | expected = List.repeat 9 0 }
                in
                Expect.equal
                    ( True, False, False )
                    ( Pose.matchesFeatures target (List.repeat 9 12)
                    , Pose.matchesFeatures target (List.repeat 9 13)
                    , Pose.matchesFeatures target (26 :: List.repeat 8 0)
                    )
        , test "a lazy neighbouring pose does not pass" <|
            \_ ->
                let
                    tPose =
                        { syntheticTarget | expected = [ 90, 180, 180, 0, 0, -90, -90, -90, -90 ] }

                    drooping =
                        [ 90, 150, 150, 30, 30, -90, -90, -90, -90 ]
                in
                Expect.equal False (Pose.matchesFeatures tPose drooping)
        , test "a single dropped sample does not reset the hold" <|
            \_ ->
                let
                    scored =
                        startSingle syntheticTarget
                            |> Game.recordPoses 3100 (Just syntheticTarget.id) [ syntheticPose ]
                            |> Game.recordPoses 3200 (Just syntheticTarget.id) []
                            |> Game.recordPoses 3300 (Just syntheticTarget.id) [ syntheticPose ]
                            |> Game.recordPoses 3700 (Just syntheticTarget.id) [ syntheticPose ]
                in
                Expect.equal [ 1 ] (List.map .score scored.players)
        , test "hidden ankles assume a straight leg, so a bent target still needs them" <|
            \_ ->
                let
                    hiddenAnkles =
                        syntheticPose
                            |> setAt 27 { x = 0.9, y = 0.2, z = 0, visibility = 0.05 }
                            |> setAt 28 { x = 0.1, y = 0.2, z = 0, visibility = 0.05 }

                    bentTarget =
                        { syntheticTarget
                            | expected =
                                case syntheticTarget.expected of
                                    [ torso, a, b, c, d, leftThigh, _, rightThigh, _ ] ->
                                        [ torso, a, b, c, d, leftThigh, leftThigh - 60, rightThigh, rightThigh + 60 ]

                                    other ->
                                        other
                        }
                in
                Expect.equal
                    ( True, False )
                    ( Pose.matches syntheticTarget hiddenAnkles
                    , Pose.matches bentTarget hiddenAnkles
                    )
        , test "limbs are slotted by frame position, so anatomical labels on the far side still match" <|
            \_ ->
                let
                    -- Unmirrored webcam frame: MediaPipe's left-labelled landmarks have the larger x.
                    wideStance =
                        List.repeat 33 (landmark 0.5 0.5)
                            |> setAt 11 (landmark 0.6 0.3)
                            |> setAt 12 (landmark 0.4 0.3)
                            |> setAt 13 (landmark 0.6 0.45)
                            |> setAt 14 (landmark 0.4 0.45)
                            |> setAt 15 (landmark 0.6 0.6)
                            |> setAt 16 (landmark 0.4 0.6)
                            |> setAt 23 (landmark 0.55 0.55)
                            |> setAt 24 (landmark 0.45 0.55)
                            |> setAt 25 (landmark 0.65 0.7232)
                            |> setAt 26 (landmark 0.35 0.7232)
                            |> setAt 27 (landmark 0.75 0.8964)
                            |> setAt 28 (landmark 0.25 0.8964)

                    target =
                        { syntheticTarget | expected = [ 90, -90, -90, -90, -90, -120, -120, -60, -60 ] }
                in
                Expect.equal
                    ( Just [ 90, -90, -90, -90, -90, -120, -120, -60, -60 ], True )
                    ( Pose.features wideStance |> Maybe.map (List.map (round >> toFloat))
                    , Pose.matches target wideStance
                    )
        , test "segment angles follow the maths convention on a uniform grid" <|
            \_ ->
                let
                    diagonal =
                        syntheticPose
                            |> setAt 11 (landmark 0.4 0.3)
                            |> setAt 13 (landmark 0.3 0.2)
                in
                Pose.features diagonal
                    |> Maybe.andThen (List.drop 1 >> List.head)
                    |> Maybe.map round
                    |> Expect.equal (Just 135)
        , test "missing or low-confidence upper-body landmarks do not match" <|
            \_ ->
                Expect.equal
                    ( Nothing, False )
                    ( Pose.features (List.take 10 syntheticPose)
                    , Pose.matches syntheticTarget lowConfidencePose
                    )
        , test "uncertain ankles do not block an otherwise matching pose" <|
            \_ ->
                Expect.equal True
                    (Pose.matches syntheticTarget
                        (syntheticPose
                            |> setAt 27 { x = 0.35, y = 0.95, z = 0, visibility = 0.05 }
                            |> setAt 28 { x = 0.65, y = 0.95, z = 0, visibility = 0.05 }
                        )
                    )
        , test "malformed camera data is rejected at the FFI boundary" <|
            \_ ->
                let
                    point =
                        Encode.object
                            [ ( "x", Encode.float 0 )
                            , ( "y", Encode.float 0 )
                            , ( "z", Encode.float 0 )
                            , ( "visibility", Encode.float 1 )
                            ]
                in
                case Decode.decodeValue Camera.posesDecoder (Encode.list identity [ Encode.list identity [ point ] ]) of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "accepted a pose without the required landmarks"
        ]


startSingle : Pose.Target -> Game.Game
startSingle target =
    case Game.tick 3000 (Game.begin 0 [ target ]) of
        Game.Continue game ->
            game

        Game.Restart ->
            Game.begin 0 [ target ]


syntheticTarget : Pose.Target
syntheticTarget =
    { id = "test"
    , expected = Maybe.withDefault [] (Pose.features syntheticPose)
    }


generatedTargets : Int -> List Pose.Target
generatedTargets seed =
    Random.step (Pose.randomTargets 12) (Random.initialSeed seed)
        |> Tuple.first


syntheticPose : Camera.RawPose
syntheticPose =
    List.repeat 33 (landmark 0.5 0.5)
        |> setAt 11 (landmark 0.4 0.3)
        |> setAt 12 (landmark 0.6 0.3)
        |> setAt 13 (landmark 0.3 0.45)
        |> setAt 14 (landmark 0.7 0.45)
        |> setAt 15 (landmark 0.2 0.6)
        |> setAt 16 (landmark 0.8 0.6)
        |> setAt 23 (landmark 0.45 0.55)
        |> setAt 24 (landmark 0.55 0.55)
        |> setAt 25 (landmark 0.4 0.75)
        |> setAt 26 (landmark 0.6 0.75)
        |> setAt 27 (landmark 0.35 0.95)
        |> setAt 28 (landmark 0.65 0.95)


lowConfidencePose : Camera.RawPose
lowConfidencePose =
    setAt 11 { x = 0.4, y = 0.3, z = 0, visibility = 0.1 } syntheticPose


landmark : Float -> Float -> Camera.Landmark
landmark x y =
    { x = x, y = y, z = 0, visibility = 1 }


setAt : Int -> value -> List value -> List value
setAt index replacement values =
    List.indexedMap
        (\current value ->
            if current == index then
                replacement

            else
                value
        )
        values


shiftedPose : Float -> Camera.RawPose -> Camera.RawPose
shiftedPose amount =
    List.map (\point -> { point | x = point.x + amount })


round2 : Float -> Float
round2 value =
    toFloat (round (value * 100)) / 100
