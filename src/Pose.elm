module Pose exposing
    ( Features
    , MatchError
    , Target
    , allTargets
    , centerX
    , features
    , landmarkAt
    , matchError
    , matches
    , matchesFeatures
    , randomTargets
    , skeleton
    )

import Camera exposing (Landmark, RawPose)
import Random


type alias Features =
    List Float


type alias MatchError =
    { average : Float
    , worst : Float
    }


type alias Target =
    { id : String, expected : Features }


allTargets : List Target
allTargets =
    List.indexedMap (\index expected -> target (String.fromInt (index + 1)) expected) plausibleFeatures


randomTargets : Int -> Random.Generator (List Target)
randomTargets count =
    Random.list (List.length plausibleFeatures) (Random.float 0 1)
        |> Random.map
            (\keys ->
                List.map2 Tuple.pair keys plausibleFeatures
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
                    |> List.take count
                    |> List.indexedMap (\index expected -> target (String.fromInt (index + 1)) expected)
            )


plausibleFeatures : List Features
plausibleFeatures =
    List.concatMap
        (\arms -> List.map (\legs -> 90 :: (arms ++ legs)) legPoses)
        armPoses


armPoses : List Features
armPoses =
    [ [ -90, -90, -90, -90 ]
    , [ 180, 180, 0, 0 ]
    , [ 90, 90, 90, 90 ]
    , [ 180, 90, 0, 90 ]
    , [ 90, 90, -90, -90 ]
    , [ -135, -35, -45, -145 ]
    , [ 135, 135, 45, 45 ]
    , [ 180, 180, 90, 90 ]
    , [ 180, 180, 180, 180 ]
    , [ 135, 45, 45, 135 ]
    , [ -135, -135, -45, -45 ]
    , [ 0, 0, 180, 180 ]
    ]


{-| Thigh and shin angles for the left then right leg. Each pose must be
readable from a front-facing camera and holdable for half a second:
standing, wide stance, sumo squat (knees out, shins upright), and a bent
knee lifted out to the side.
-}
legPoses : List Features
legPoses =
    [ [ -90, -90, -90, -90 ]
    , [ -120, -120, -60, -60 ]
    , [ -120, -90, -60, -90 ]
    , [ -90, -90, -35, -100 ]
    ]


target : String -> Features -> Target
target id expected =
    { id = id, expected = expected }


matches : Target -> RawPose -> Bool
matches targetPose rawPose =
    features rawPose
        |> Maybe.map (matchesFeatures targetPose)
        |> Maybe.withDefault False


matchError : Target -> RawPose -> Maybe MatchError
matchError targetPose rawPose =
    features rawPose
        |> Maybe.andThen
            (\actual ->
                case ( measure targetPose.expected actual, measure targetPose.expected (mirror actual) ) of
                    ( Just direct, Just mirrored ) ->
                        if direct.average + direct.worst <= mirrored.average + mirrored.worst then
                            Just direct

                        else
                            Just mirrored

                    ( Just direct, Nothing ) ->
                        Just direct

                    ( Nothing, Just mirrored ) ->
                        Just mirrored

                    _ ->
                        Nothing
            )


matchesFeatures : Target -> Features -> Bool
matchesFeatures targetPose actual =
    passes targetPose.expected actual || passes targetPose.expected (mirror actual)


{-| The largest single-segment error allowed. Target poses differ by at least
45° per limb, so anything under half of that keeps a lazy version of a
neighbouring pose from passing while leaving room for human imprecision.
-}
worstTolerance : Float
worstTolerance =
    25


{-| The mean error across all nine segments. Keeps the whole pose honest
rather than letting one perfect limb hide several sloppy ones.
-}
averageTolerance : Float
averageTolerance =
    12


passes : Features -> Features -> Bool
passes expected actual =
    measure expected actual
        |> Maybe.map (\error -> error.worst <= worstTolerance && error.average <= averageTolerance)
        |> Maybe.withDefault False


measure : Features -> Features -> Maybe MatchError
measure expected actual =
    let
        errors =
            List.map2 angleDifference expected actual
    in
    if List.length errors == List.length expected && List.length actual == List.length expected && not (List.isEmpty errors) then
        Just
            { average = List.sum errors / toFloat (List.length errors)
            , worst = Maybe.withDefault 360 (List.maximum errors)
            }

    else
        Nothing


{-| Absolute 2D angle of each body segment, in degrees, counter-clockwise from
"pointing right" (so straight up is 90 and straight down is -90). Landmarks
must be in a uniform coordinate space (see `Camera.Landmark`).
-}
features : RawPose -> Maybe Features
features rawPose =
    case List.filterMap (\index -> landmarkAt index rawPose) requiredLandmarks of
        [ shoulderA, shoulderB, elbowA, elbowB, wristA, wristB, hipA, hipB, kneeA, kneeB, ankleA, ankleB ] ->
            let
                sideA =
                    Side shoulderA elbowA wristA hipA kneeA ankleA

                sideB =
                    Side shoulderB elbowB wristB hipB kneeB ankleB

                -- MediaPipe labels landmarks anatomically, so a player's "left" side sits
                -- on the frame's right. Target poses instead call the limb drawn on the
                -- left "left". Assign slots by position in the frame so the two agree;
                -- `mirror` still accepts the reflected reading.
                ( left, right ) =
                    if shoulderA.x + hipA.x <= shoulderB.x + hipB.x then
                        ( sideA, sideB )

                    else
                        ( sideB, sideA )
            in
            if List.all visible [ left.shoulder, right.shoulder, left.elbow, right.elbow, left.wrist, right.wrist, left.hip, right.hip, left.knee, right.knee ] then
                Just
                    [ angle (midpoint left.hip right.hip) (midpoint left.shoulder right.shoulder)
                    , angle left.shoulder left.elbow
                    , angle left.elbow left.wrist
                    , angle right.shoulder right.elbow
                    , angle right.elbow right.wrist
                    , angle left.hip left.knee
                    , shin left.hip left.knee left.ankle
                    , angle right.hip right.knee
                    , shin right.hip right.knee right.ankle
                    ]

            else
                Nothing

        _ ->
            Nothing


type alias Side =
    { shoulder : Landmark
    , elbow : Landmark
    , wrist : Landmark
    , hip : Landmark
    , knee : Landmark
    , ankle : Landmark
    }


visible : Landmark -> Bool
visible point =
    point.visibility >= 0.3


{-| Ankles are often cropped out by the camera. MediaPipe still guesses a
position for them, but that guess is noise, so when an ankle is not visible
assume the leg is straight and reuse the thigh angle. Targets with a bent knee
therefore still require a visible ankle, while a plain stance does not.
-}
shin : Landmark -> Landmark -> Landmark -> Float
shin hip knee ankle =
    if visible ankle then
        angle knee ankle

    else
        angle hip knee


requiredLandmarks : List Int
requiredLandmarks =
    [ 11, 12, 13, 14, 15, 16, 23, 24, 25, 26, 27, 28 ]


mirror : Features -> Features
mirror values =
    case values of
        [ torso, leftUpperArm, leftLowerArm, rightUpperArm, rightLowerArm, leftThigh, leftShin, rightThigh, rightShin ] ->
            [ reflect torso
            , reflect rightUpperArm
            , reflect rightLowerArm
            , reflect leftUpperArm
            , reflect leftLowerArm
            , reflect rightThigh
            , reflect rightShin
            , reflect leftThigh
            , reflect leftShin
            ]

        _ ->
            values


reflect : Float -> Float
reflect value =
    normalizeAngle (180 - value)


angle : Landmark -> Landmark -> Float
angle from to =
    atan2 (from.y - to.y) (to.x - from.x) * 180 / pi


angleDifference : Float -> Float -> Float
angleDifference left right =
    let
        difference =
            abs (normalizeAngle left - normalizeAngle right)
    in
    min difference (360 - difference)


normalizeAngle : Float -> Float
normalizeAngle value =
    value - 360 * toFloat (floor ((value + 180) / 360))


midpoint : Landmark -> Landmark -> Landmark
midpoint left right =
    { x = (left.x + right.x) / 2
    , y = (left.y + right.y) / 2
    , z = (left.z + right.z) / 2
    , visibility = min left.visibility right.visibility
    }


centerX : RawPose -> Float
centerX rawPose =
    case ( landmarkAt 23 rawPose, landmarkAt 24 rawPose ) of
        ( Just leftHip, Just rightHip ) ->
            (leftHip.x + rightHip.x) / 2

        _ ->
            case ( landmarkAt 11 rawPose, landmarkAt 12 rawPose ) of
                ( Just leftShoulder, Just rightShoulder ) ->
                    (leftShoulder.x + rightShoulder.x) / 2

                _ ->
                    landmarkAt 0 rawPose |> Maybe.map .x |> Maybe.withDefault 0.5


landmarkAt : Int -> RawPose -> Maybe Landmark
landmarkAt index rawPose =
    rawPose |> List.drop index |> List.head


skeleton : List ( Int, Int )
skeleton =
    [ ( 11, 12 )
    , ( 11, 13 )
    , ( 13, 15 )
    , ( 12, 14 )
    , ( 14, 16 )
    , ( 11, 23 )
    , ( 12, 24 )
    , ( 23, 24 )
    , ( 23, 25 )
    , ( 25, 27 )
    , ( 24, 26 )
    , ( 26, 28 )
    ]
