module SDL.Font.Foreign

import Data.Bits
import Data.Maybe
import System.FFI
import SDL.Font.Types
import SDL.Keysym
import SDL.Types
import SDL.Foreign

libsdl : String -> String
libsdl fn = "C:" ++ fn ++ ",libidrissdl"

%foreign libsdl "sdl_ttf_get_error"
prim__sdlGetError : PrimIO String

export
getFontError : HasIO io => io SDLFontError
getFontError = FontError <$> primIO prim__sdlGetError

checkRetPtr : HasIO io => AnyPtr -> {default getFontError handler : io SDLFontError} -> io (Either SDLFontError ())
checkRetPtr ptr =
  if prim__nullAnyPtr ptr /= 0
     then Left <$> handler
     else pure $ Right ()

checkNonZeroRet : (Num n, Eq n, HasIO io) => n -> {default getFontError handler : io SDLFontError} -> io (Either SDLFontError ())
checkNonZeroRet ret =
  if ret == 0
     then Left <$> handler
     else pure $ Right ()

checkZeroRet : (Num n, Eq n, HasIO io) => n -> {default getFontError handler : io SDLFontError} -> io (Either SDLFontError ())
checkZeroRet ret =
  if ret /= 0
     then Left <$> handler
     else pure $ Right ()

%foreign libsdl "sdl_ttf_init"
prim__sdlTtfInit : PrimIO Int

export
init : HasIO io => io (Either SDLFontError ())
init = do
  res <- primIO prim__sdlTtfInit
  checkZeroRet res

%foreign libsdl "sdl_ttf_quit"
prim__sdlTtfQuit : PrimIO ()

export
quit : HasIO io => io ()
quit = primIO prim__sdlTtfQuit

%foreign libsdl "sdl_ttf_open_font"
prim__sdlTtfOpenFont : String -> Int -> PrimIO AnyPtr

export
openFont : HasIO io => String -> Int -> io (Either SDLFontError SDLFont)
openFont str pt = do
      ptr <- primIO $ prim__sdlTtfOpenFont str pt
      Right () <- checkRetPtr ptr
        | Left err => pure $ Left err
      pure $ Right (Font ptr)

%foreign libsdl "sdl_ttf_close_font"
prim__sdlTtfCloseFont : AnyPtr -> PrimIO ()

export
closeFont : HasIO io => SDLFont -> io ()
closeFont (Font raw) = primIO $ prim__sdlTtfCloseFont raw

%foreign libsdl "sdl_ttf_render_text_solid"
prim__sdlTtfRenderTextSolid : AnyPtr -> String -> Bits8 -> Bits8 -> Bits8 -> Bits8 -> PrimIO AnyPtr

export
renderTextSolid : HasIO io => SDLFont -> String -> SDLColor -> io (Either SDLFontError SDLSurface)
renderTextSolid (Font fnt) x (RGBA r g b a) = do
  let r' = cast $ natToInteger r
  let g' = cast $ natToInteger g
  let b' = cast $ natToInteger b
  let a' = cast $ natToInteger a
  ptr <- primIO $ prim__sdlTtfRenderTextSolid fnt x r' g' b' a'
  Right () <- checkRetPtr ptr
    | Left err => pure $ Left err
  pure $ Right (Surface ptr)
