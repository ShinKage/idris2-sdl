module SDL.Font

import Control.Linear.LIO

import public SDL.Types
import public SDL.Font.Types
import SDL.Font.Foreign
import SDL.Foreign
import SDL

%default total

public export
data SDLTTFState : Type where
  Inited : SDLTTFState

public export
data SDLTTF : SDLTTFState -> Type where
  Initial : SDLTTF Inited

public export
data SDLTTFErrorPath : (ok : SDLTTFState) -> (err : SDLTTFState) -> Type where
  Success : (1 _ : SDLTTF ok) -> SDLTTFErrorPath ok err
  Failure : (1 _ : SDLTTF err) -> SDLFontError -> SDLTTFErrorPath ok err

export
initSDLTTF : LinearIO io => (onerr : SDLFontError -> L io ret) -> (onok : (1 _ : SDLTTF Inited) -> L io ret) -> L io ret
initSDLTTF onerr onok = case !(SDL.Font.Foreign.init) of
  Left err => onerr err
  Right () => onok Initial

export
withFont : LinearIO io
        => (path : String)
        -> (pt   : Int)
        -> (handler : (1 _ : SDLFont) -> (1 _ : SDLTTF Inited) -> L io { use = 1 } (SDLTTFErrorPath Inited Inited))
        -> (1 _ : SDLTTF Inited)
        -> L io {use = 1} (SDLTTFErrorPath Inited Inited)
withFont path pt handler Initial = case !(openFont path pt) of
  Left err => pure1 $ Failure Initial err
  Right fnt => do
    ret <- handler fnt Initial
    closeFont fnt
    pure1 ret

export
withSolidSurfaceRender : LinearIO io
                      => (1 _ : SDLFont)
                      -> String
                      -> SDLColor
                      -> (handler : (1 _ : SDLTexture) -> (1 _ : SDL WithRenderer) -> L io { use = 1 } (SDLErrorPath WithRenderer WithRenderer))
                      -> (1 _ : SDL WithRenderer)
                      -> (1 _ : SDLTTF Inited)
                      -> L io { use = 1 } (SDLErrorPath WithRenderer WithRenderer, SDLTTFErrorPath Inited Inited)
withSolidSurfaceRender (Font fnt) text color handler (Rendered win rnd) Initial =
  case !(renderTextSolid (Font fnt) text color) of
       Left err => pure1 $ (Success (Rendered win rnd), Failure Initial err)
       Right srf => case !(createTextureFromSurface rnd srf) of
                           Left err => do freeSurface srf
                                          pure1 $ (Failure (Rendered win rnd) err, Success Initial)
                           Right txt => do freeSurface srf
                                           ret <- handler txt (Rendered win rnd)
                                           destroyTexture txt
                                           case ret of
                                                Success (Rendered win rnd) => pure1 (Success (Rendered win rnd), Success Initial)
                                                Failure (Rendered win rnd) err => pure1 (Failure (Rendered win rnd) err, Success Initial)

export
quitSDLTTF : LinearIO io => (1 _ : SDLTTF Inited) -> L io ()
quitSDLTTF Initial = SDL.Font.Foreign.quit

export
handleInitedError : LinearIO io => (1 _ : SDLTTF Inited) -> (1 fn : L io a) -> L io a
handleInitedError s fn = do quitSDLTTF s
                            fn
