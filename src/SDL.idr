module SDL

import Control.App
import Control.App.Console
import public SDL.Types
import SDL.Foreign

%default total

public export
data SDLState : Type where
  Quitted : SDLState
  Inited : SDLState
  WithWindow : SDLState
  WithRenderer : SDLState

export
data SDL : SDLState -> Type where
  Initial : SDL Inited
  Windowed : SDLWindow -> SDL WithWindow
  Rendered : SDLWindow -> SDLRenderer -> SDL WithRenderer

public export
data SDLErrorPath : (ok : SDLState) -> (err : SDLState) -> Type where
  Success : (1 _ : SDL ok) -> SDLErrorPath ok err
  Failure : (1 _ : SDL err) -> SDLError -> SDLErrorPath ok err

public export
interface SDLInterface e where
  initSDL : List SDLInitFlags -> (SDLError -> App e ret) -> ((1 _ : SDL Inited) -> App e ret) -> App e ret
  quitSDL : (1 _ : SDL Inited) -> App {l} e ()
  newWindow : SDLWindowOptions -> (1 _ : SDL Inited) -> App1 e (SDLErrorPath WithWindow Inited)
  closeWindow : (1 _ : SDL WithWindow) -> App1 e (SDL Inited)
  newRenderer : Maybe Int -> List SDLRendererFlags -> (1 _ : SDL WithWindow) -> App1 e (SDLErrorPath WithRenderer WithWindow)
  closeRenderer : (1 _ : SDL WithRenderer) -> App1 e (SDL WithWindow)
  render : (1 _ : SDL WithRenderer) -> App1 e (SDL WithRenderer)
  clear : (1 _ : SDL WithRenderer) -> App1 e (SDLErrorPath WithRenderer WithRenderer)
  setColor : SDLColor -> (1 _ : SDL WithRenderer) -> App1 e (SDLErrorPath WithRenderer WithRenderer)
  drawPoint : SDLPoint -> (1 _ : SDL WithRenderer) -> App1 e (SDLErrorPath WithRenderer WithRenderer)
  drawLine : SDLPoint -> SDLPoint -> (1 _ : SDL WithRenderer) -> App1 e (SDLErrorPath WithRenderer WithRenderer)
  drawRect : SDLRect -> (1 _ : SDL WithRenderer) -> App1 e (SDLErrorPath WithRenderer WithRenderer)
  fillRect : SDLRect -> (1 _ : SDL WithRenderer) -> App1 e (SDLErrorPath WithRenderer WithRenderer)
  pollEvent : App {l = NoThrow} e (Maybe (t : SDLEventType ** RawEventStruct t))

export
Has [PrimIO] e => SDLInterface e where
  initSDL flags onerr onok = case !(primIO $ init flags) of
    Left err => onerr err
    Right () => onok Initial
  quitSDL Initial = primIO quit

  newWindow opts Initial = case !(app $ primIO $ createWindow opts) of
    Left err => pure1 $ Failure Initial err
    Right win => pure1 $ Success (Windowed win)
  closeWindow (Windowed win) = do
    app $ primIO $ destroyWindow win
    pure1 Initial

  newRenderer index flags (Windowed win) = case !(app $ primIO $ createRenderer win index flags) of
    Left err => pure1 $ Failure (Windowed win) err
    Right rnd => pure1 $ Success (Rendered win rnd)
  closeRenderer (Rendered win rnd) = do
    app $ primIO $ destroyRenderer rnd
    pure1 (Windowed win)

  render (Rendered win rnd) = do
    app $ primIO $ renderPresent rnd
    pure1 (Rendered win rnd)
  clear (Rendered win rnd) = case !(app $ primIO $ renderClear rnd) of
    Left err => pure1 $ Failure (Rendered win rnd) err
    Right () => pure1 $ Success (Rendered win rnd)

  setColor color (Rendered win rnd) = case !(app $ primIO $ setRenderDrawColor rnd color) of
    Left err => pure1 $ Failure (Rendered win rnd) err
    Right () => pure1 $ Success (Rendered win rnd)

  drawPoint p (Rendered win rnd) = case !(app $ primIO $ renderDrawPoint rnd p) of
    Left err => pure1 $ Failure (Rendered win rnd) err
    Right () => pure1 $ Success (Rendered win rnd)
  drawLine p1 p2 (Rendered win rnd) = case !(app $ primIO $ renderDrawLine rnd p1 p2) of
    Left err => pure1 $ Failure (Rendered win rnd) err
    Right () => pure1 $ Success (Rendered win rnd)
  drawRect r (Rendered win rnd) = case !(app $ primIO $ renderDrawRect rnd r) of
    Left err => pure1 $ Failure (Rendered win rnd) err
    Right () => pure1 $ Success (Rendered win rnd)
  fillRect r (Rendered win rnd) = case !(app $ primIO $ renderFillRect rnd r) of
    Left err => pure1 $ Failure (Rendered win rnd) err
    Right () => pure1 $ Success (Rendered win rnd)

  pollEvent = case !(primIO pollEvent) of
    Nothing => pure Nothing
    Just raw => pure $ Just !(primIO $ getEvent raw)

export
delaySDL : Has [PrimIO] e => Nat -> App1 {u=Any} e ()
delaySDL = app . primIO . delay

export
handleInitedError : Has [SDLInterface] e => (1 _ : SDL Inited) -> (1 fn : App1 {u = Any} e a) -> App1 {u = Any} e a
handleInitedError s fn = do
  app $ quitSDL s
  fn

export
handleWindowedError : Has [SDLInterface] e => (1 _ : SDL WithWindow) -> (1 fn : App1 {u = Any} e a) -> App1 {u = Any} e a
handleWindowedError s fn = do
  s <- closeWindow s
  app $ quitSDL s
  fn

export
handleRenderedError : Has [SDLInterface] e => (1 _ : SDL WithRenderer) -> (1 fn : App1 {u = Any} e a) -> App1 {u = Any} e a
handleRenderedError s fn = do
  s <- closeRenderer s
  s <- closeWindow s
  app $ quitSDL s
  fn
