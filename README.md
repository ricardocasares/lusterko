# Lusterko

A fullscreen local-multiplayer webcam party game: strike the pose shown on screen and hold it for half a second to score. Built in Elm with Tailwind CSS; TypeScript only bridges MediaPipe pose detection and configures Vite.

```sh
bun install
bun run dev
```

Webcam access requires localhost or HTTPS.

## How it plays

- Up to four players are tracked at once, each with their own colour, ordered left to right in the frame.
- Each game shuffles twelve target poses. Every pose gets a 3‑second "Get ready" countdown, then a 5‑second round.
- The target skeleton is drawn large on the right third of the screen so it reads from across a room; the round timer sits top-left, scores bottom-left.
- Holding a matching pose for 0.5 s scores a point; a single dropped detection is forgiven. Results show for five seconds, then a new game shuffles.
- Press `D` during a game to open the debug view and step through every pose with live match errors.

## Checks

```sh
bun run check
```

Runs `elm-test`, `tsc --noEmit` and a production build. Target poses are rendered directly from the same angle definitions used for matching.
