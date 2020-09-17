module SDL.Types

import Data.Nat
import System.FFI

import SDL.Keysym
import SDL.Elab

%default total
%language ElabReflection

public export
data SDLError : Type where
  GenericError : String -> SDLError
  ImageError : String -> SDLError

export
Show SDLError where
  show (GenericError err) = err
  show (ImageError err) = err

public export
data SDLInitFlags : Type where
  SDLInitTimer : SDLInitFlags
  SDLInitAudio : SDLInitFlags
  SDLInitVideo : SDLInitFlags
  SDLInitJoystick : SDLInitFlags
  SDLInitHaptic : SDLInitFlags
  SDLInitGameController : SDLInitFlags
  SDLInitEvents : SDLInitFlags

%runElab deriveUnitSumEq Export `{{SDLInitFlags}}
%runElab deriveUnitSumShow Export `{{SDLInitFlags}}

export
initFlagsToBits : SDLInitFlags -> Bits32
initFlagsToBits SDLInitTimer = 0x00000001
initFlagsToBits SDLInitAudio = 0x00000010
initFlagsToBits SDLInitVideo = 0x00000020
initFlagsToBits SDLInitJoystick = 0x00000200
initFlagsToBits SDLInitHaptic = 0x00001000
initFlagsToBits SDLInitGameController = 0x00002000
initFlagsToBits SDLInitEvents = 0x00004000

export
sdlInitEverything : List SDLInitFlags
sdlInitEverything = [SDLInitTimer, SDLInitAudio, SDLInitVideo, SDLInitJoystick, SDLInitHaptic, SDLInitGameController, SDLInitEvents]

public export
data SDLWindow : Type where
  Window : Bits32 -> AnyPtr -> SDLWindow

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

%runElab deriveUnitSumEq Export `{{SDLWindowFlags}}
%runElab deriveUnitSumShow Export `{{SDLWindowFlags}}

export
windowFlagsToBits : SDLWindowFlags -> Bits32
windowFlagsToBits SDLWindowFullscreen = 0x00000001
windowFlagsToBits SDLWindowFullscreenDesktop = 0x00001001
windowFlagsToBits SDLWindowOpenGL = 0x00000002
windowFlagsToBits SDLWindowVulkan = 0x10000000
windowFlagsToBits SDLWindowHidden = 0x00000008
windowFlagsToBits SDLWindowBorderless = 0x00000010
windowFlagsToBits SDLWindowResizable = 0x00000020
windowFlagsToBits SDLWindowMinimized = 0x00000040
windowFlagsToBits SDLWindowMaximized = 0x00000080
windowFlagsToBits SDLWindowInputGrabbed = 0x00000100
windowFlagsToBits SDLWindowAllowHighDPI = 0x00002000

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
data SDLTexture : Type where
  Texture : AnyPtr -> SDLTexture

public export
data SDLRenderer : Type where
  Renderer : AnyPtr -> SDLRenderer

public export
data SDLRendererFlags : Type where
  SDLRendererSoftware : SDLRendererFlags
  SDLRendererAccelerated : SDLRendererFlags
  SDLRendererPresentVSync : SDLRendererFlags
  SDLRendererTargetTexture : SDLRendererFlags

%runElab deriveUnitSumEq Export `{{SDLRendererFlags}}
%runElab deriveUnitSumShow Export `{{SDLRendererFlags}}

export
rendererFlagsToBits : SDLRendererFlags -> Bits32
rendererFlagsToBits SDLRendererSoftware      = 0x00000001
rendererFlagsToBits SDLRendererAccelerated   = 0x00000002
rendererFlagsToBits SDLRendererPresentVSync  = 0x00000004
rendererFlagsToBits SDLRendererTargetTexture = 0x00000008

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

%runElab deriveUnitSumEq Export `{{SDLEventType}}
%runElab deriveUnitSumShow Export `{{SDLEventType}}

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

%runElab deriveUnitSumEq Export `{{MouseButton}}
%runElab deriveUnitSumShow Export `{{MouseButton}}

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

%runElab deriveUnitSumEq Export `{{ButtonState}}
%runElab deriveUnitSumShow Export `{{ButtonState}}

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
