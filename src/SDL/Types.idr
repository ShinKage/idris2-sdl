module SDL.Types

import Data.Nat
import System.FFI

%default total

public export
data SDLError : Type where
  GenericError : String -> SDLError

export
Show SDLError where
  show (GenericError err) = err

public export
data SDLInitFlags : Type where
  SDLInitTimer : SDLInitFlags
  SDLInitAudio : SDLInitFlags
  SDLInitVideo : SDLInitFlags
  SDLInitJoystick : SDLInitFlags
  SDLInitHaptic : SDLInitFlags
  SDLInitGameController : SDLInitFlags
  SDLInitEvents : SDLInitFlags

export
initFlagsToInt : SDLInitFlags -> Int
initFlagsToInt SDLInitTimer = 0x00000001
initFlagsToInt SDLInitAudio = 0x00000010
initFlagsToInt SDLInitVideo = 0x00000020
initFlagsToInt SDLInitJoystick = 0x00000200
initFlagsToInt SDLInitHaptic = 0x00001000
initFlagsToInt SDLInitGameController = 0x00002000
initFlagsToInt SDLInitEvents = 0x00004000

export
sdlInitEverything : List SDLInitFlags
sdlInitEverything = [SDLInitTimer, SDLInitAudio, SDLInitVideo, SDLInitJoystick, SDLInitHaptic, SDLInitGameController, SDLInitEvents]

public export
data SDLWindow : Type where
  Window : AnyPtr -> SDLWindow

public export
data SDLWindowPos : Type where
  SDLWindowPosCentered : SDLWindowPos
  SDLWindowPosUndefined : SDLWindowPos
  SDLWindowPosCustom : Int -> SDLWindowPos

export
windowPosToInt : SDLWindowPos -> Int
windowPosToInt SDLWindowPosCentered = 0x2FFF0000
windowPosToInt SDLWindowPosUndefined = 0x1FFF0000
windowPosToInt (SDLWindowPosCustom x) = x

public export
data SDLWindowFlags : Type where
  SDLWindowFullscreen : SDLWindowFlags
  SDLWindowFullscreenDesktop : SDLWindowFlags
  SDLWindowOpenGL : SDLWindowFlags
  SDLWindowVulkan : SDLWindowFlags
  SDLWindowHidden : SDLWindowFlags
  SDLWindowBorderless : SDLWindowFlags
  SDLWindowResizable : SDLWindowFlags
  SDLWindowMinimized : SDLWindowFlags
  SDLWindowMaximized : SDLWindowFlags
  SDLWindowInputGrabbed : SDLWindowFlags
  SDLWindowAllowHighDPI : SDLWindowFlags

export
windowFlagsToInt : SDLWindowFlags -> Int
windowFlagsToInt SDLWindowFullscreen = 0x00000001
windowFlagsToInt SDLWindowFullscreenDesktop = 0x00001001
windowFlagsToInt SDLWindowOpenGL = 0x00000002
windowFlagsToInt SDLWindowVulkan = 0x10000000
windowFlagsToInt SDLWindowHidden = 0x00000008
windowFlagsToInt SDLWindowBorderless = 0x00000010
windowFlagsToInt SDLWindowResizable = 0x00000020
windowFlagsToInt SDLWindowMinimized = 0x00000040
windowFlagsToInt SDLWindowMaximized = 0x00000080
windowFlagsToInt SDLWindowInputGrabbed = 0x00000100
windowFlagsToInt SDLWindowAllowHighDPI = 0x00002000

public export
record SDLWindowOptions where
  constructor MkSDLWindowOptions
  name : String
  x : SDLWindowPos
  y : SDLWindowPos
  width : Int
  height : Int
  flags : List SDLWindowFlags

public export
data SDLSurface : Type where
  Surface : AnyPtr -> SDLSurface

public export
data SDLRenderer : Type where
  Renderer : AnyPtr -> SDLRenderer

public export
data SDLRendererFlags : Type where
  SDLRendererSoftware : SDLRendererFlags
  SDLRendererAccelerated : SDLRendererFlags
  SDLRendererPresentVSync : SDLRendererFlags
  SDLRendererTargetTexture : SDLRendererFlags

export
rendererFlagsToInt : SDLRendererFlags -> Int
rendererFlagsToInt SDLRendererSoftware      = 0x00000001
rendererFlagsToInt SDLRendererAccelerated   = 0x00000002
rendererFlagsToInt SDLRendererPresentVSync  = 0x00000004
rendererFlagsToInt SDLRendererTargetTexture = 0x00000008

public export
data SDLRawEvent : Type where
  RawEvent : GCAnyPtr -> SDLRawEvent

public export
data SDLEventType : Type where
  SDLQuit : SDLEventType
  SDLWindowEvent : SDLEventType
  SDLKeyDown : SDLEventType
  SDLKeyUp : SDLEventType
  SDLMouseMotion : SDLEventType
  SDLMouseButtonDown : SDLEventType
  SDLMouseButtonUp : SDLEventType
  SDLGenericEvent : SDLEventType

export
Show SDLEventType where
  show SDLQuit = "SDLQuit"
  show SDLWindowEvent = "SDLWindowEvent"
  show SDLKeyDown = "SDLKeyDown"
  show SDLKeyUp = "SDLKeyUp"
  show SDLMouseMotion = "SDLMouseMotion"
  show SDLMouseButtonDown = "SDLMouseButtonDown"
  show SDLMouseButtonUp = "SDLMouseButtonUp"
  show SDLGenericEvent = "SDLGenericEvent"

export total
eventFromInt : Int -> SDLEventType
eventFromInt 0x100 = SDLQuit
eventFromInt 0x200 = SDLWindowEvent
eventFromInt 0x300 = SDLKeyDown
eventFromInt 0x301 = SDLKeyUp
eventFromInt 0x400 = SDLMouseMotion
eventFromInt 0x401 = SDLMouseButtonDown
eventFromInt 0x402 = SDLMouseButtonUp
eventFromInt x = SDLGenericEvent

public export
record SDLMouseEvent where
  constructor MkMouseEvent
  button : Int
  state : Int
  clicks : Int
  x : Int
  y : Int

public export
record SDLKeyboardEvent where
  constructor MkKeyboardEvent
  type : Int
  state : Int
  repeat : Int
  scancode : Int
  keycode : Int
  mod : Int

public export
record SDLPoint where
  constructor MkPoint
  x : Int
  y : Int

public export
record SDLRect where
  constructor MkRect
  x : Int
  y : Int
  width : Int
  height : Int

public export
data SDLColor : Type where
  RGBA : (r : Nat) -> LTE r 255 =>
         (g : Nat) -> LTE g 255 =>
         (b : Nat) -> LTE b 255 =>
         (a : Nat) -> LTE a 255 => SDLColor
