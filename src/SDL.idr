module SDL

import Control.Linear.LIO

import public SDL.Types
import public SDL.Keysym
import SDL.Foreign

%default total

public export
data SDLState : Type where
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

export
initSDL : LinearIO io => (flags : List SDLInitFlags) -> (onerr : SDLError -> L io ret) -> (onok : (1 _ : SDL Inited) -> L io ret) -> L io ret
initSDL flags onerr onok = case !(init flags) of
  Left err => onerr err
  Right () => onok Initial

export
quitSDL : LinearIO io => (1 _ : SDL Inited) -> L io ()
quitSDL Initial = quit

export
newWindow : LinearIO io => (opts : SDLWindowOptions) -> (1 _ : SDL Inited) -> L {use = 1} io (SDLErrorPath WithWindow Inited)
newWindow opts Initial = case !(createWindow opts) of
  Left err => pure1 $ Failure Initial err
  Right win => pure1 $ Success (Windowed win)

export
closeWindow : LinearIO io => (1 _ : SDL WithWindow) -> L {use = 1} io (SDL Inited)
closeWindow (Windowed win) = do
  destroyWindow win
  pure1 Initial

export
newRenderer : LinearIO io => (index : Maybe Int) -> (flags : List SDLRendererFlags) -> (1 _ : SDL WithWindow) -> L {use = 1} io (SDLErrorPath WithRenderer WithWindow)
newRenderer index flags (Windowed win) = case !(createRenderer win index flags) of
  Left err => pure1 $ Failure (Windowed win) err
  Right rnd => pure1 $ Success (Rendered win rnd)

export
closeRenderer : LinearIO io => (1 _ : SDL WithRenderer) -> L {use = 1} io (SDL WithWindow)
closeRenderer (Rendered win rnd) = do
  destroyRenderer rnd
  pure1 (Windowed win)

export
render : LinearIO io => (1 _ : SDL WithRenderer) -> L {use = 1} io (SDL WithRenderer)
render (Rendered win rnd) = do
  renderPresent rnd
  pure1 (Rendered win rnd)

export
clear : LinearIO io => (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
clear (Rendered win rnd) = case !(renderClear rnd) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
setColor : LinearIO io => (color : SDLColor) -> (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
setColor color (Rendered win rnd) = case !(setRenderDrawColor rnd color) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
drawPoint : LinearIO io => (point : SDLPoint) -> (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
drawPoint p (Rendered win rnd) = case !(renderDrawPoint rnd p) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
drawLine : LinearIO io => (point1 : SDLPoint) -> (point2 : SDLPoint) -> (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
drawLine p1 p2 (Rendered win rnd) = case !(renderDrawLine rnd p1 p2) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
drawRect : LinearIO io => (rect : SDLRect) -> (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
drawRect r (Rendered win rnd) = case !(renderDrawRect rnd r) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
fillRect : LinearIO io => (rect : SDLRect) -> (1 _ : SDL WithRenderer) -> L {use = 1} io (SDLErrorPath WithRenderer WithRenderer)
fillRect r (Rendered win rnd) = case !(renderFillRect rnd r) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
pollEvent : LinearIO io => L io (Maybe (t : SDLEventType ** RawEventStruct t))
pollEvent = case !(Foreign.pollEvent) of
  Nothing => pure Nothing
  Just raw => getEvent raw

export
copySurface : LinearIO io => (texture : SDLTexture) -> (src : SDLRect) -> (dst : SDLRect) -> (1 _ : SDL WithRenderer) -> L io {use = 1} (SDLErrorPath WithRenderer WithRenderer)
copySurface txt src dst (Rendered win rnd) = case !(renderCopy rnd txt src dst) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right () => pure1 $ Success (Rendered win rnd)

export
withImage : LinearIO io
         => (path : String)
         -> (handler : (1 _ : SDLTexture) -> (1 _ : SDL WithRenderer) -> L io {use = 1} (SDLErrorPath WithRenderer WithRenderer))
         -> (1 _ : SDL WithRenderer)
         -> L io {use = 1} (SDLErrorPath WithRenderer WithRenderer)
withImage path handler (Rendered win rnd) = case !(imgLoad path) of
  Left err => pure1 $ Failure (Rendered win rnd) err
  Right srf => case !(createTextureFromSurface rnd srf) of
                    Left err => do freeSurface srf
                                   pure1 $ Failure (Rendered win rnd) err
                    Right txt => do freeSurface srf
                                    ret <- handler txt (Rendered win rnd)
                                    destroyTexture txt
                                    pure1 ret

export
delaySDL : LinearIO io => (time : Nat) -> L io ()
delaySDL = delay

export
handleInitedError : LinearIO io => (1 _ : SDL Inited) -> (1 fn : L io a) -> L io a
handleInitedError s fn = do quitSDL s; fn

export
handleWindowedError : LinearIO io => (1 _ : SDL WithWindow) -> (1 fn : L io a) -> L io a
handleWindowedError s fn = do
  s <- closeWindow s
  quitSDL s
  fn

export
handleRenderedError : LinearIO io => (1 _ : SDL WithRenderer) -> (1 fn : L io a) -> L io a
handleRenderedError s fn = do
  s <- closeRenderer s
  s <- closeWindow s
  quitSDL s
  fn
