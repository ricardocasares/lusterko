module Game exposing
    ( Game
    , Phase(..)
    , Player
    , Step(..)
    , begin
    , currentPose
    , holdFraction
    , recordPoses
    , roundMillis
    , tick
    )

import Camera exposing (RawPose)
import Pose


type alias Player =
    { score : Int
    , active : Bool
    , matching : Bool
    , holdStarted : Maybe Int
    , lastMatched : Maybe Int
    }


{-| How long a hold survives without a matching sample. Samples arrive every
~100 ms, so this forgives a single dropped detection or a boundary flicker
without letting anyone rest mid-hold.
-}
holdGraceMillis : Int
holdGraceMillis =
    200


holdMillis : Int
holdMillis =
    500


{-| How long players get to match each pose.
-}
roundMillis : Int
roundMillis =
    5000


type Phase
    = Countdown Int String
    | Playing Pose.Target Int
    | Celebrating Int (List Int)
    | Results Int


type alias Game =
    { phase : Phase
    , remaining : List Pose.Target
    , shown : Int
    , total : Int
    , players : List Player
    , poses : List RawPose
    , now : Int
    , sampling : Bool
    }


type Step
    = Continue Game
    | Restart


begin : Int -> List Pose.Target -> Game
begin now targets =
    { phase = Countdown (now + 3000) "Get ready"
    , remaining = targets
    , shown = 0
    , total = List.length targets
    , players = []
    , poses = []
    , now = now
    , sampling = False
    }


tick : Int -> Game -> Step
tick now game =
    let
        current =
            { game | now = now }
    in
    case current.phase of
        Countdown deadline _ ->
            if now >= deadline then
                Continue (startNext now current)

            else
                Continue current

        Playing _ deadline ->
            if now >= deadline && not current.sampling then
                Continue (afterMiss now current)

            else
                Continue current

        Celebrating deadline _ ->
            if now >= deadline then
                Continue (nextOrResults now "Next pose" current)

            else
                Continue current

        Results deadline ->
            if now >= deadline then
                Restart

            else
                Continue current


recordPoses : Int -> Maybe String -> List RawPose -> Game -> Game
recordPoses capturedAt requestedTarget rawPoses game =
    let
        ordered =
            rawPoses
                |> List.filter (\rawPose -> Pose.features rawPose /= Nothing)
                |> List.sortBy (\rawPose -> -(Pose.centerX rawPose))
                |> List.take 4

        slots =
            ensurePlayerCount (List.length ordered) game.players

        updatedPlayers =
            List.indexedMap (updatePlayer capturedAt requestedTarget game.phase ordered) slots

        scorers =
            updatedPlayers
                |> List.indexedMap
                    (\index player ->
                        if qualifies capturedAt player then
                            Just (index + 1)

                        else
                            Nothing
                    )
                |> List.filterMap identity

        updatedGame =
            { game | poses = ordered, players = updatedPlayers, sampling = False }
    in
    if List.isEmpty scorers then
        updatedGame

    else
        { updatedGame
            | phase = Celebrating (capturedAt + 1000) scorers
            , players = score scorers updatedPlayers
        }


updatePlayer : Int -> Maybe String -> Phase -> List RawPose -> Int -> Player -> Player
updatePlayer capturedAt requestedTarget phase ordered index player =
    case getAt index ordered of
        Nothing ->
            { player | active = False, matching = False, holdStarted = keepHold capturedAt player }

        Just rawPose ->
            let
                isMatching =
                    case phase of
                        Playing target deadline ->
                            requestedTarget
                                == Just target.id
                                && capturedAt
                                <= deadline
                                && Pose.matches target rawPose

                        _ ->
                            False
            in
            { player
                | active = True
                , matching = isMatching
                , holdStarted =
                    if isMatching then
                        Just (Maybe.withDefault capturedAt player.holdStarted)

                    else
                        keepHold capturedAt player
                , lastMatched =
                    if isMatching then
                        Just capturedAt

                    else
                        player.lastMatched
            }


keepHold : Int -> Player -> Maybe Int
keepHold capturedAt player =
    case player.lastMatched of
        Just matchedAt ->
            if capturedAt - matchedAt <= holdGraceMillis then
                player.holdStarted

            else
                Nothing

        Nothing ->
            Nothing


qualifies : Int -> Player -> Bool
qualifies capturedAt player =
    player.active
        && player.matching
        && (player.holdStarted
                |> Maybe.map (\started -> capturedAt - started >= holdMillis)
                |> Maybe.withDefault False
           )


score : List Int -> List Player -> List Player
score scorers players =
    List.indexedMap
        (\index player ->
            { player
                | score =
                    if List.member (index + 1) scorers then
                        player.score + 1

                    else
                        player.score
                , matching = False
                , holdStarted = Nothing
                , lastMatched = Nothing
            }
        )
        players


ensurePlayerCount : Int -> List Player -> List Player
ensurePlayerCount count players =
    players ++ List.repeat (max 0 (count - List.length players)) blankPlayer


blankPlayer : Player
blankPlayer =
    { score = 0, active = False, matching = False, holdStarted = Nothing, lastMatched = Nothing }


startNext : Int -> Game -> Game
startNext now game =
    case game.remaining of
        target :: rest ->
            { game
                | phase = Playing target (now + roundMillis)
                , remaining = rest
                , shown = game.shown + 1
                , players = resetHolds game.players
            }

        [] ->
            { game | phase = Results (now + 5000) }


afterMiss : Int -> Game -> Game
afterMiss now game =
    nextOrResults now "Time's up" { game | players = resetHolds game.players }


nextOrResults : Int -> String -> Game -> Game
nextOrResults now notice game =
    if List.isEmpty game.remaining then
        { game | phase = Results (now + 5000) }

    else
        { game | phase = Countdown (now + 3000) notice }


resetHolds : List Player -> List Player
resetHolds =
    List.map (\player -> { player | matching = False, holdStarted = Nothing, lastMatched = Nothing })


currentPose : Game -> Maybe Pose.Target
currentPose game =
    case game.phase of
        Playing target _ ->
            Just target

        _ ->
            Nothing


holdFraction : Int -> Player -> Float
holdFraction now player =
    player.holdStarted
        |> Maybe.map (\started -> min 1 (toFloat (now - started) / toFloat holdMillis))
        |> Maybe.withDefault 0


getAt : Int -> List value -> Maybe value
getAt index values =
    values |> List.drop index |> List.head
