module Main

import Control.App
import Control.App.Console
import Data.Nat
import Data.Vect
import System.Clock
import System.FFI
import System.Random

import SDL
import SDL.Foreign

import LinGameOfLife

-- Press n to compute the next step in the game
-- Press s to start/stop the simulation
-- Press r to generate a new random game
-- Press +/- to speed up/down
-- Press arrow keys to pan

%auto_implicit_depth 256
black : SDLColor
black = RGBA 0 0 0 255

red : SDLColor
red = RGBA 255 0 0 255
%auto_implicit_depth 50

putError : Has [Console] e => SDLError -> App1 {u=Any} e ()
putError = app . putStrLn . show

data Life : Type where

record LifeState where
  constructor MkLifeState
  {radius : Nat}
  game : Game (S radius) Cell
  scale : Nat
  frequency : Nat
  elapsedTicks : Nat
  lastTick : Clock Monotonic
  isPlaying : Bool
  averageUpdateTime : (Integer, Integer)

drawBoard : Has [Console, SDLInterface] e => Nat -> List (Nat, Nat, Cell) -> (1 _ : SDL WithRenderer) -> App1 e (SDL WithRenderer)
drawBoard _ [] s = pure1 s
drawBoard scale ((_, _, Dead) :: cs) s = drawBoard scale cs s
drawBoard scale ((col, row, Alive) :: cs) s = do
  let rect = MkRect (cast $ row * scale) (cast $ col * scale) (cast scale) (cast scale)
  Success s <- fillRect rect s
    | Failure s err => do putError err; pure1 s
  drawBoard scale cs s

drawGame : Has [State Life LifeState, PrimIO, SDLInterface] e =>
           (1 _ : SDL WithRenderer) -> App1 e (SDL WithRenderer)
drawGame s = do
  Success s <- setColor black s
    | Failure s err => do putError err; pure1 s
  Success s <- clear s
    | Failure s err => do putError err; pure1 s
  Success s <- setColor red s
    | Failure s err => do putError err; pure1 s
  st <- app $ get Life
  let board = gameSquare st.game
  drawBoard st.scale board s

onKeyEvent : Has [State Life LifeState, PrimIO, SDLInterface] e => SDLKeyboardEvent -> App {l = NoThrow} e ()
onKeyEvent evt = case evt.keycode of
  0x72 => do
    rnd <- primIO $ randomGame {radius=14}
    modify Life (record { game = rnd, isPlaying = False })
  0x73 => modify Life (record { isPlaying $= not })
  0x6E => do
    st <- get Life
    if st.isPlaying
       then pure ()
       else modify Life (record { game $= nextStep })
  0x2D => do
    st <- get Life
    putStrLn $ "=> Updated frequency to " ++ show (st.frequency + 1)
    modify Life (record { frequency $= S })
  0x2B => do
    st <- get Life
    let newFreq = max 1 (st.frequency `minus` 1)
    putStrLn $ "=> Updated frequency to " ++ show newFreq
    modify Life (record { frequency = newFreq })
  0x4000004F => modify Life (record { game $= rightShift })
  0x40000050 => modify Life (record { game $= leftShift })
  0x40000051 => modify Life (record { game $= downShift })
  0x40000052 => modify Life (record { game $= upShift })
  _ => pure ()

eventLoop : Has [State Life LifeState, PrimIO, SDLInterface] e => (1 _ : SDL WithRenderer) -> App1 e (SDL WithRenderer)
eventLoop s = do
  st <- app $ get Life
  now <- app $ primIO $ clockTime Monotonic
  let delta = timeDifference now st.lastTick
  if nanoseconds delta < 16000
     then eventLoop s
     else do
       app $ modify Life (record { lastTick = now, elapsedTicks $= S })
       if st.isPlaying && st.elapsedTicks >= st.frequency
          then do
            let (n, avg) = st.averageUpdateTime
            let avg' = ((nanoseconds now) + n * avg) `div` (n + 1)
            app $ putStrLn $ "INFO: Average time " ++ show (avg' `div` 1000000) ++ " ms"
            app $ modify Life (record { game $= nextStep, elapsedTicks = 0, averageUpdateTime = (n + 1, avg') })
          else pure ()
       case !(app pollEvent) of
            Just (SDLQuit ** ()) => pure1 s
            Just (SDLKeyUp ** evt) => do
              app $ onKeyEvent evt
              s <- drawGame s
              s <- render s
              eventLoop s
            _ => do
              s <- drawGame s
              s <- render s
              eventLoop s

defaultWindowOpts : SDLWindowOptions
defaultWindowOpts = MkSDLWindowOptions "test" SDLWindowPosCentered SDLWindowPosCentered 500 500 []

test : Has [State Life LifeState, Console, PrimIO, SDLInterface] e => App e ()
test = initSDL [SDLInitVideo] (\err => putStrLn $ "Fatal error: " ++ show err) $ \s => app1 $ do
  -- s <- initSDL [SDLInitVideo]
  app $ primIO $ putStrLn "=> SDL Inited"

  st <- app $ get Life
  let size : Int = cast $ st.scale * (2 * (st.radius + 1) + 1)
  let winops : SDLWindowOptions = record { width = size, height = size } defaultWindowOpts
  Success s <- newWindow winops s
    | Failure s err => handleInitedError s (putError err)
  app $ primIO $ putStrLn "=> Window created"

  Success s <- newRenderer Nothing [SDLRendererSoftware] s
    | Failure s err => handleWindowedError s (putError err)
  app $ primIO $ putStrLn "=> Renderer operational"

  s <- drawGame s
  s <- render s
  s <- eventLoop s

  s <- closeRenderer s
  app $ primIO $ putStrLn "=> Renderer closed"
  s <- closeWindow s
  app $ primIO $ putStrLn "=> Window closed"
  app $ quitSDL s
  app $ primIO $ putStrLn "=> SDL quitted"

main : IO ()
main = do
  clock <- clockTime Monotonic
  rnd <- randomGame {radius=14} -- total dim is (14 + 1) * 2 + 1
  run $ new (MkLifeState rnd 10 25 0 clock False (0, 0)) test
