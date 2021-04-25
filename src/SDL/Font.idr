module SDL.Font

import Control.Linear.LIO

import public SDL.Types
import public SDL.Font.Types
import SDL.Font.Foreign
import SDL.Foreign
import SDL
import Data.Vect
import Data.Vect.Elem

%default total

public export
data SDLTTFState : Type where
  Inited : SDLTTFState
  WithRenderer : SDLTTFState

public export
data SDLTTF : SDLTTFState -> Type where
  Initial : SDLTTF Inited
  Rendered : SDL WithRenderer -> SDLTTF WithRenderer

public export
data ErrorPath : ( a : Type ) -> ( b : a -> Type ) -> ( e : Type ) -> (ok : a) -> (err : a) -> Type where
  Success : (1 _ : b ok) -> ErrorPath a b e ok err
  Failure : (1 _ : b err) -> e -> ErrorPath a b e ok err

public export
data CoHVect : Vect n Type -> Type where
       CoElem : Elem r rs => r -> CoHVect rs

{--
public export
data SDLTTFErrorPath : (ok : SDLTTFState) -> (err : SDLTTFState) -> Type where
  Success : (1 _ : SDLTTF ok) -> SDLTTFErrorPath ok err
  Failure : (1 _ : SDLTTF err) -> SDLFontError -> SDLTTFErrorPath ok err
  --}

public export
SDLTTFErrorPath : SDLTTFState -> SDLTTFState -> Type
SDLTTFErrorPath = ErrorPath SDLTTFState SDLTTF SDLFontError

public export
SDLTTFErrorPath' : SDLTTFState -> SDLTTFState -> Type
SDLTTFErrorPath' = ErrorPath SDLTTFState SDLTTF (CoHVect [SDLError, SDLFontError])

export
initSDLTTF : LinearIO io => (onerr : SDLFontError -> L io ret) -> (onok : (1 _ : SDLTTF Inited) -> L io ret) -> L io ret
initSDLTTF onerr onok = case !(SDL.Font.Foreign.init) of
  Left err => onerr err
  Right () => onok Initial

export
withFont : LinearIO io
        => (path : String)
        -> (pt   : Int)
        -> (handler : (1 _ : SDLFont) -> (1 _ : SDLTTF x) -> L io { use = 1 } (SDLTTFErrorPath y x))
        -> (1 _ : SDLTTF x)
        -> L io {use = 1} (SDLTTFErrorPath y x)
withFont path pt handler x = case !(openFont path pt) of
  Left err => pure1 $ Failure x err
  Right fnt => do
    ret <- handler fnt x
    closeFont fnt
    pure1 ret

export
withSolidSurfaceRender : LinearIO io
                      => (1 _ : SDLFont)
                      -> String
                      -> SDLColor
                      -> (handler : (1 _ : SDLTexture) -> (1 _ : SDL WithRenderer) -> L io { use = 1 } (SDLErrorPath WithRenderer WithRenderer))
                      -> (1 _ : SDLTTF WithRenderer)
                      -> L io { use = 1 } (SDLTTFErrorPath' WithRenderer WithRenderer)
withSolidSurfaceRender (Font fnt) text color handler (Rendered (Rendered win rnd)) =
  case !(renderTextSolid (Font fnt) text color) of
       Left err => pure1 $ (Success (Rendered (Rendered win rnd)))
       Right srf => case !(createTextureFromSurface rnd srf) of
                           Left err => do freeSurface srf
                                          pure1 $ Failure (Rendered (Rendered win rnd)) (CoElem err)
                           Right txt => do freeSurface srf
                                           ret <- handler txt (Rendered win rnd)
                                           destroyTexture txt
                                           case ret of
                                                Success (Rendered win rnd) => pure1 (Success (Rendered (Rendered win rnd)))
                                                Failure (Rendered win rnd) err => pure1 (Failure (Rendered (Rendered win rnd)) (CoElem err))

export
quitSDLTTF : LinearIO io => (1 _ : SDLTTF Inited) -> L io ()
quitSDLTTF Initial = SDL.Font.Foreign.quit

export
handleInitedError : LinearIO io => (1 _ : SDLTTF Inited) -> (1 fn : L io a) -> L io a
handleInitedError s fn = do quitSDLTTF s
                            fn
