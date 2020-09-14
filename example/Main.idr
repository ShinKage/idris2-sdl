module Main

import Control.Linear.LIO
import Data.IORef
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

data Ref : (l : label) -> Type -> Type where
  [search l]
  MkRef : IORef a -> Ref x a

export
newRef : HasIO io => (x : label) -> t -> io (Ref x t)
newRef x val
    = do ref <- liftIO $ newIORef val
         pure (MkRef ref)

export %inline
get : HasIO io => (x : label) -> {auto ref : Ref x a} -> io a
get x {ref = MkRef io} = liftIO $ readIORef io

export %inline
put : HasIO io => (x : label) -> {auto ref : Ref x a} -> a -> io ()
put x {ref = MkRef io} val = liftIO $ writeIORef io val

export %inline
modify : HasIO io => (x : label) -> {auto ref : Ref x a} -> (a -> a) -> io ()
modify x f = do
  ref <- get x
  put x (f ref)

putError : LinearIO io => (err : SDLError) -> L io ()
putError = putStrLn . show

data Life : Type where -- Label for State

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

drawBoard : LinearIO io
         => (scale : Nat)
         -> (cells : List (Nat, Nat, Cell))
         -> (1 _ : SDL WithRenderer)
         -> L {use = 1} io (SDL WithRenderer)
drawBoard scale [] s = pure1 s
drawBoard scale ((_, _, Dead) :: cs) s = drawBoard scale cs s
drawBoard scale ((col, row, Alive) :: cs) s = do
  let rect = MkRect (cast $ row * scale) (cast $ col * scale) (cast scale) (cast scale)
  Success s <- fillRect rect s
    | Failure s err => do putError err; pure1 s
  drawBoard scale cs s

drawGame : (LinearIO io, Ref Life LifeState)
        => (1 _ : SDL WithRenderer)
        -> L {use = 1} io (SDL WithRenderer)
drawGame s = do
  Success s <- setColor black s
    | Failure s err => do putError err; pure1 s
  Success s <- clear s
    | Failure s err => do putError err; pure1 s
  Success s <- setColor red s
    | Failure s err => do putError err; pure1 s
  st <- get Life
  let board = gameSquare st.game
  drawBoard st.scale board s

onKeyEvent : (LinearIO io, Ref Life LifeState) => (event : SDLKeyboardEvent) -> L io ()
onKeyEvent evt = case evt.keycode of
  KeyR => do
    rnd <- liftIO $ randomGame {radius=14}
    modify Life (record { game = rnd, isPlaying = False })
  KeyS => modify Life (record { isPlaying $= not })
  KeyN => do
    st <- get Life
    when (not st.isPlaying) $
       modify Life (record { game $= nextStep })
  Minus => do
    st <- get Life
    putStrLn $ "=> Updated frequency to " ++ show (st.frequency + 1)
    modify Life (record { frequency $= S })
  Plus => do
    st <- get Life
    let newFreq = max 1 (st.frequency `minus` 1)
    putStrLn $ "=> Updated frequency to " ++ show newFreq
    modify Life (record { frequency = newFreq })
  Right => modify Life (record { game $= rightShift })
  Left => modify Life (record { game $= leftShift })
  Down => modify Life (record { game $= downShift })
  Up => modify Life (record { game $= upShift })
  _ => pure ()

eventLoop : (LinearIO io, Ref Life LifeState)
         => (1 _ : SDL WithRenderer)
         -> L {use = 1} io (SDL WithRenderer)
eventLoop s = do
  st <- get Life
  now <- liftIO $ clockTime Monotonic
  let delta = timeDifference now st.lastTick
  if nanoseconds delta < 16000
     then eventLoop s
     else do
       modify Life (record { lastTick = now, elapsedTicks $= S })
       when (st.isPlaying && st.elapsedTicks >= st.frequency) $ do
         let (n, avg) = st.averageUpdateTime
         let avg' = ((nanoseconds now) + n * avg) `div` (n + 1)
         putStrLn $ "INFO: Average time " ++ show (avg' `div` 1000000) ++ " ms"
         modify Life (record { game $= nextStep, elapsedTicks = 0, averageUpdateTime = (n + 1, avg') })
       the (L {use = 1} io (SDL WithRenderer)) $ case !pollEvent of
         Just (SDLQuit ** ()) => pure1 s
         Just (SDLKeyUp ** evt) => do
           onKeyEvent evt
           s <- drawGame s
           s <- render s
           eventLoop s
         _ => do
           s <- drawGame s
           s <- render s
           eventLoop s

defaultWindowOpts : SDLWindowOptions
defaultWindowOpts = MkSDLWindowOptions "Example" SDLWindowPosCentered SDLWindowPosCentered 500 500 []

win : (LinearIO io, Ref Life LifeState) => L io ()
win = initSDL [SDLInitVideo] (\err => putStrLn $ "Fatal error: " ++ show err) $ \s => do
  putStrLn "=> SDL Inited"

  st <- get Life
  let size : Int = cast $ st.scale * (2 * (st.radius + 1) + 1)
  let winops : SDLWindowOptions = record { width = size, height = size } defaultWindowOpts
  Success s <- newWindow winops s
    | Failure s err => handleInitedError s (putError err)
  putStrLn "=> Window created"

  Success s <- newRenderer Nothing [SDLRendererSoftware] s
    | Failure s err => handleWindowedError s (putError err)
  putStrLn "=> Renderer operational"

  s <- drawGame s
  s <- render s
  s <- eventLoop s

  s <- closeRenderer s
  putStrLn "=> Renderer closed"
  s <- closeWindow s
  putStrLn "=> Window closed"
  quitSDL s
  putStrLn "=> SDL quitted"

main : IO ()
main = do
  clock <- clockTime Monotonic
  rnd <- randomGame {radius=14} -- total dim is (14 + 1) * 2 + 1
  run $ do newRef Life (MkLifeState rnd 10 25 0 clock False (0, 0))
           win
