module Sound exposing (Sound(..), play)

{-| Interaction sounds, synthesised by cuelume on the JavaScript side.
-}

import FFI
import Json.Encode as Encode
import Task exposing (Task)


type Sound
    = Tick
    | Success
    | Error
    | Ready
    | Bloom


play : Sound -> Task String ()
play sound =
    playRaw { name = name sound }
        |> Task.map (always ())
        |> Task.mapError FFI.errorToString


playRaw : { name : String } -> Task FFI.Error Encode.Value
playRaw =
    FFI.function
        [ ( "name", .name >> Encode.string ) ]
        """
        globalThis.__lusterkoSound.play(name)
        """


name : Sound -> String
name sound =
    case sound of
        Tick ->
            "tick"

        Success ->
            "success"

        Error ->
            "error"

        Ready ->
            "ready"

        Bloom ->
            "bloom"
