module SDL.Types

import Data.Nat
import System.FFI

import SDL.Keysym

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
Eq SDLInitFlags where
  SDLInitTimer == SDLInitTimer = True
  SDLInitAudio == SDLInitAudio = True
  SDLInitVideo == SDLInitVideo = True
  SDLInitJoystick == SDLInitJoystick = True
  SDLInitHaptic == SDLInitHaptic = True
  SDLInitGameController == SDLInitGameController = True
  SDLInitEvents == SDLInitEvents = True
  _ == _ = False

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
Eq SDLWindowPos where
  SDLWindowPosCentered == SDLWindowPosCentered = True
  SDLWindowPosUndefined == SDLWindowPosUndefined = True
  (SDLWindowPosCustom p) == (SDLWindowPosCustom p') = p == p'
  _ == _ = False

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
Eq SDLWindowFlags where
  SDLWindowFullscreen == SDLWindowFullscreen = True
  SDLWindowFullscreenDesktop == SDLWindowFullscreenDesktop = True
  SDLWindowOpenGL == SDLWindowOpenGL = True
  SDLWindowVulkan == SDLWindowVulkan = True
  SDLWindowHidden == SDLWindowHidden = True
  SDLWindowBorderless == SDLWindowBorderless = True
  SDLWindowResizable == SDLWindowResizable = True
  SDLWindowMinimized == SDLWindowMinimized = True
  SDLWindowMaximized == SDLWindowMaximized = True
  SDLWindowInputGrabbed == SDLWindowInputGrabbed = True
  SDLWindowAllowHighDPI == SDLWindowAllowHighDPI = True
  _ == _ = False

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
Eq SDLRendererFlags where
  SDLRendererSoftware == SDLRendererSoftware = True
  SDLRendererAccelerated == SDLRendererAccelerated = True
  SDLRendererPresentVSync == SDLRendererPresentVSync = True
  SDLRendererTargetTexture == SDLRendererTargetTexture = True
  _ == _ = False

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
Eq SDLEventType where
  SDLQuit == SDLQuit = True
  SDLWindowEvent == SDLWindowEvent = True
  SDLKeyDown == SDLKeyDown = True
  SDLKeyUp == SDLKeyUp = True
  SDLMouseMotion == SDLMouseMotion = True
  SDLMouseButtonDown == SDLMouseButtonDown = True
  SDLMouseButtonUp == SDLMouseButtonUp = True
  SDLGenericEvent == SDLGenericEvent = True
  _ == _ = False

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

export
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
data MouseButton = Left | Middle | Right | X1 | X2

export
Eq MouseButton where
  Left == Left = True
  Middle == Middle = True
  Right == Right = True
  X1 == X1 = True
  X2 == X2 = True
  _ == _ = False

export
mouseButtonToBits : MouseButton -> Bits8
mouseButtonToBits Left = 0x01
mouseButtonToBits Middle = 0x02
mouseButtonToBits Right = 0x03
mouseButtonToBits X1 = 0x04
mouseButtonToBits X2 = 0x05

export
mouseButtonFromBits : Bits8 -> Maybe MouseButton
mouseButtonFromBits 0x01 = Just Left
mouseButtonFromBits 0x02 = Just Middle
mouseButtonFromBits 0x03 = Just Right
mouseButtonFromBits 0x04 = Just X1
mouseButtonFromBits 0x05 = Just X2
mouseButtonFromBits _ = Nothing

public export
data ButtonState = Pressed | Released

export
Eq ButtonState where
  Pressed == Pressed = True
  Released == Released = True
  _ == _ = False

export
buttonStateToBits : ButtonState -> Bits8
buttonStateToBits Pressed = 0x01
buttonStateToBits Released = 0x00

export
buttonStateFromBits : Bits8 -> Maybe ButtonState
buttonStateFromBits 0x01 = Just Pressed
buttonStateFromBits 0x00 = Just Released
buttonStateFromBits _ = Nothing

public export
record SDLMouseButtonEvent where
  constructor MkMouseButtonEvent
  timestamp : Bits32
  windowID : Bits32
  which : Bits32
  button : MouseButton
  state : ButtonState
  clicks : Bits8
  x : Int
  y : Int

public export
record SDLMouseMotionEvent where
  constructor MkMouseMotionEvent
  timestamp : Bits32
  windowID : Bits32
  which : Bits32
  state : Bits32
  x : Int
  y : Int
  xrel : Int
  yrel : Int

public export
record SDLKeyboardEvent where
  constructor MkKeyboardEvent
  timestamp : Bits32
  windowID : Bits32
  state : ButtonState
  repeat : Bits8
  scancode : Int
  keycode : Keycode
  mod : List Keymod

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
