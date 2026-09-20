module Camera exposing (CameraError, Landmark, RawPose, errorToString, posesDecoder, samplePoses, startCamera)

import FFI
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import Time


type alias CameraError =
    String


{-| A landmark in "frame height" units: y runs 0..1 from the top of the frame,
x runs 0..aspect from the left, so a unit of x and a unit of y are the same
physical distance and angles measured between landmarks are undistorted.
-}
type alias Landmark =
    { x : Float, y : Float, z : Float, visibility : Float }


type alias RawPose =
    List Landmark


type alias CameraConfig =
    { maxPoses : Int }


startCamera : { maxPoses : Int } -> Task CameraError ()
startCamera config =
    startCameraRaw config
        |> FFI.decode (Decode.succeed ())
        |> Task.mapError FFI.errorToString


startCameraRaw : CameraConfig -> Task FFI.Error Decode.Value
startCameraRaw =
    FFI.function
        [ ( "maxPoses", .maxPoses >> Encode.int ) ]
        """
        const video = document.getElementById("camera")
        if (!video) throw new Error("The camera element is missing")
        if (!navigator.mediaDevices?.getUserMedia) {
          throw new Error("This browser does not support webcam access")
        }

        const previous = globalThis.__lusterkoPose
        previous?.stream?.getTracks().forEach(track => track.stop())
        previous?.landmarker?.close?.()

        const stream = await navigator.mediaDevices.getUserMedia({
          audio: false,
          video: {
            facingMode: "user",
            width: { ideal: 1280 },
            height: { ideal: 720 }
          }
        })

        try {
          video.srcObject = stream
          video.muted = true
          video.playsInline = true
          await video.play()

          const { FilesetResolver, PoseLandmarker } = globalThis.__lusterkoVision
          const vision = await FilesetResolver.forVisionTasks(
            "https://cdn.jsdelivr.net/npm/@mediapipe/tasks-vision@1.0.1/wasm"
          )
          const baseOptions = {
            modelAssetPath: "https://storage.googleapis.com/mediapipe-models/pose_landmarker/pose_landmarker_full/float16/1/pose_landmarker_full.task"
          }
          const options = {
            baseOptions: { ...baseOptions, delegate: "GPU" },
            runningMode: "VIDEO",
            numPoses: maxPoses,
            minPoseDetectionConfidence: 0.7,
            minPosePresenceConfidence: 0.7,
            minTrackingConfidence: 0.6
          }

          let landmarker
          try {
            landmarker = await PoseLandmarker.createFromOptions(vision, options)
          } catch (_) {
            landmarker = await PoseLandmarker.createFromOptions(vision, {
              ...options,
              baseOptions
            })
          }

          globalThis.__lusterkoPose = {
            landmarker,
            stream,
            video,
            lastVideoTime: -1,
            lastMediaTimestamp: -1,
            lastPoses: []
          }
          return true
        } catch (error) {
          stream.getTracks().forEach(track => track.stop())
          throw error
        }
        """


samplePoses : Time.Posix -> Task CameraError (List RawPose)
samplePoses capturedAt =
    samplePosesRaw { timestamp = Time.posixToMillis capturedAt }
        |> FFI.decode posesDecoder
        |> Task.mapError FFI.errorToString


samplePosesRaw : { timestamp : Int } -> Task FFI.Error Decode.Value
samplePosesRaw =
    FFI.function
        [ ( "timestamp", .timestamp >> Encode.int ) ]
        """
        const state = globalThis.__lusterkoPose
        if (!state) throw new Error("The pose detector is not ready")
        if (state.video.readyState < 2) return []
        if (state.video.currentTime === state.lastVideoTime) {
          // No new frame yet: repeat the last detection instead of reporting
          // an empty room, which would reset every player's hold.
          return state.lastPoses
        }

        state.lastVideoTime = state.video.currentTime
        const mediaTimestamp = Math.max(
          performance.now(),
          state.lastMediaTimestamp + 0.001
        )
        state.lastMediaTimestamp = mediaTimestamp
        const result = state.landmarker.detectForVideo(state.video, mediaTimestamp)
        // MediaPipe normalises x by frame width and y by frame height, which
        // squashes angles on non-square frames. Rescale x into height units so
        // both axes share one physical scale (see the Landmark docs).
        const aspect = state.video.videoWidth / state.video.videoHeight || 1
        state.lastPoses = result.landmarks.map(pose => pose.map(point => ({
          x: point.x * aspect,
          y: point.y,
          z: point.z ?? 0,
          visibility: point.visibility ?? 0
        })))
        return state.lastPoses
        """


poseDecoder : Decoder RawPose
poseDecoder =
    Decode.list landmarkDecoder
        |> Decode.andThen
            (\landmarks ->
                if List.length landmarks >= 29 then
                    Decode.succeed landmarks

                else
                    Decode.fail "Pose result did not contain the expected landmarks"
            )


posesDecoder : Decoder (List RawPose)
posesDecoder =
    Decode.list poseDecoder


landmarkDecoder : Decoder Landmark
landmarkDecoder =
    Decode.map4 Landmark
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.oneOf [ Decode.field "z" Decode.float, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "visibility" Decode.float, Decode.succeed 0 ])


errorToString : CameraError -> String
errorToString error =
    error
