module SDL.Font.Types

import System.FFI

public export
data SDLFontError : Type where
  FontError : String -> SDLFontError

export
Show SDLFontError where
  show (FontError err) = err

public export
data SDLFont : Type where
  Font : AnyPtr -> SDLFont
