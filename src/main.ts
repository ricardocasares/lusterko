import "./elm-ffi";
import { FilesetResolver, PoseLandmarker } from "@mediapipe/tasks-vision";
import { play } from "cuelume";
import { Elm } from "./Main.elm";
import "./style.css";

declare global {
  var __lusterkoVision: {
    FilesetResolver: typeof FilesetResolver;
    PoseLandmarker: typeof PoseLandmarker;
  };
  var __lusterkoSound: { play: typeof play };
}

globalThis.__lusterkoVision = { FilesetResolver, PoseLandmarker };
globalThis.__lusterkoSound = { play };
Elm.Main.init({ node: document.getElementById("app") });