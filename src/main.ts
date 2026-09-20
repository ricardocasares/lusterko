import "./elm-ffi";
import { FilesetResolver, PoseLandmarker } from "@mediapipe/tasks-vision";
import { Elm } from "./Main.elm";
import "./style.css";

declare global {
  var __lusterkoVision: {
    FilesetResolver: typeof FilesetResolver;
    PoseLandmarker: typeof PoseLandmarker;
  };
}

globalThis.__lusterkoVision = { FilesetResolver, PoseLandmarker };
Elm.Main.init({ node: document.getElementById("app") });
