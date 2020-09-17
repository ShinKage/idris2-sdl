module SDL.Foreign

import Data.Maybe
import System.FFI
import SDL.Types
import SDL.Keysym

%default total

libsdl : String -> String
libsdl fn = "C:" ++ fn ++ ",libidrissdl"

%foreign libsdl "sdl_free"
prim__sdlFree : AnyPtr -> PrimIO ()

%foreign libsdl "sdl_get_error"
prim__sdlGetError : PrimIO String

%foreign libsdl "sdl_img_get_error"
prim__sdlImgGetError : PrimIO String

export
getError : HasIO io => io SDLError
getError = GenericError <$> primIO prim__sdlGetError

export
getImageError : HasIO io => io SDLError
getImageError = ImageError <$> primIO prim__sdlGetError

checkRetPtr : HasIO io => AnyPtr -> {default getError handler : io SDLError} -> io (Either SDLError ())
checkRetPtr ptr {handler} =
  if prim__nullAnyPtr ptr == 1
     then Left <$> handler
     else pure $ Right ()

checkRetNum : (Num n, Eq n, HasIO io) => n -> {default getError handler : io SDLError} -> io (Either SDLError ())
checkRetNum ret {handler} =
  if ret /= 0
     then Left <$> handler
     else pure $ Right ()

%foreign libsdl "sdl_init"
prim__sdlInit : Bits32 -> PrimIO Int

export
init : HasIO io => List SDLInitFlags -> io (Either SDLError ())
init flags = do
  let flags' = foldl prim__or_Bits32 0 (initFlagsToBits <$> flags)
  res <- primIO $ prim__sdlInit flags'
  checkRetNum res

%foreign libsdl "sdl_quit"
prim__sdlQuit : PrimIO ()

export
quit : HasIO io => io ()
quit = primIO prim__sdlQuit

%foreign libsdl "sdl_get_window_id"
prim__sdlGetWindowID : AnyPtr -> PrimIO Bits32

%foreign libsdl "sdl_create_window"
prim__sdlCreateWindow : String -> Int -> Int -> Int -> Int -> Bits32 -> PrimIO AnyPtr

export
createWindow : HasIO io => SDLWindowOptions -> io (Either SDLError SDLWindow)
createWindow opts = do
  let flags = foldl prim__or_Bits32 0 (windowFlagsToBits <$> opts.flags)
  let x = windowPosToInt opts.x
  let y = windowPosToInt opts.y
  win <- primIO $ prim__sdlCreateWindow opts.name x y opts.width opts.height flags
  checkRetPtr win
  idx <- primIO $ prim__sdlGetWindowID win
  checkRetNum idx
  pure $ Right (Window idx win)

%foreign libsdl "sdl_destroy_window"
prim__sdlDestroyWindow : AnyPtr -> PrimIO ()

export
destroyWindow : HasIO io => SDLWindow -> io ()
destroyWindow (Window _ win) = primIO $ prim__sdlDestroyWindow win

%foreign libsdl "sdl_create_renderer"
prim__sdlCreateRenderer : AnyPtr -> Int -> Bits32 -> PrimIO AnyPtr

export
createRenderer : HasIO io => SDLWindow -> Maybe Int -> List SDLRendererFlags -> io (Either SDLError SDLRenderer)
createRenderer (Window _ win) index flags = do
  let index' = fromMaybe (-1) index
  let flags' = foldl prim__or_Bits32 0 (rendererFlagsToBits <$> flags)
  rnd <- primIO $ prim__sdlCreateRenderer win index' flags'
  checkRetPtr rnd
  pure $ Right (Renderer rnd)

%foreign libsdl "sdl_destroy_renderer"
prim__sdlDestroyRenderer : AnyPtr -> PrimIO ()

export
destroyRenderer : HasIO io => SDLRenderer -> io ()
destroyRenderer (Renderer rnd) = primIO $ prim__sdlDestroyRenderer rnd

%foreign libsdl "sdl_render_clear"
prim__sdlRenderClear : AnyPtr -> PrimIO Int

export
renderClear : HasIO io => SDLRenderer -> io (Either SDLError ())
renderClear (Renderer rnd) = do
  res <- primIO $ prim__sdlRenderClear rnd
  checkRetNum res

%foreign libsdl "sdl_render_present"
prim__sdlRenderPresent : AnyPtr -> PrimIO ()

export
renderPresent : HasIO io => SDLRenderer -> io ()
renderPresent (Renderer rnd) = primIO $ prim__sdlRenderPresent rnd

%foreign libsdl "sdl_render_set_draw_color"
prim__sdlSetRenderDrawColor : AnyPtr -> Bits8 -> Bits8 -> Bits8 -> Bits8 -> PrimIO Int

export
setRenderDrawColor : HasIO io => SDLRenderer -> SDLColor -> io (Either SDLError ())
setRenderDrawColor (Renderer rnd) (RGBA r g b a) = do
  let r' = cast $ natToInteger r
  let g' = cast $ natToInteger g
  let b' = cast $ natToInteger b
  let a' = cast $ natToInteger a
  res <- primIO $ prim__sdlSetRenderDrawColor rnd r' g' b' a'
  checkRetNum res

%foreign libsdl "sdl_render_draw_point"
prim__sdlRenderDrawPoint : AnyPtr -> Int -> Int -> PrimIO Int

export
renderDrawPoint : HasIO io => SDLRenderer -> SDLPoint -> io (Either SDLError ())
renderDrawPoint (Renderer rnd) (MkPoint x y) = do
  res <- primIO $ prim__sdlRenderDrawPoint rnd x y
  checkRetNum res

%foreign libsdl "sdl_render_draw_line"
prim__sdlRenderDrawLine : AnyPtr -> Int -> Int -> Int -> Int -> PrimIO Int

export
renderDrawLine : HasIO io => SDLRenderer -> SDLPoint -> SDLPoint -> io (Either SDLError ())
renderDrawLine (Renderer rnd) p1 p2 = do
  res <- primIO $ prim__sdlRenderDrawLine rnd p1.x p1.y p2.x p2.y
  checkRetNum res

%foreign libsdl "sdl_render_draw_rect"
prim__sdlRenderDrawRect : AnyPtr -> Int -> Int -> Int -> Int -> PrimIO Int

export
renderDrawRect : HasIO io => SDLRenderer -> SDLRect -> io (Either SDLError ())
renderDrawRect (Renderer rnd) rect = do
  res <- primIO $ prim__sdlRenderDrawRect rnd rect.x rect.y rect.width rect.height
  checkRetNum res

%foreign libsdl "sdl_render_fill_rect"
prim__sdlRenderFillRect : AnyPtr -> Int -> Int -> Int -> Int -> PrimIO Int

export
renderFillRect : HasIO io => SDLRenderer -> SDLRect -> io (Either SDLError ())
renderFillRect (Renderer rnd) rect = do
  res <- primIO $ prim__sdlRenderFillRect rnd rect.x rect.y rect.width rect.height
  checkRetNum res

%foreign libsdl "sdl_render_copy"
prim__sdlRenderCopy : AnyPtr -> AnyPtr -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

export
renderCopy : HasIO io => SDLRenderer -> SDLTexture -> SDLRect -> SDLRect -> io (Either SDLError ())
renderCopy (Renderer rnd) (Texture txt) src dst = do
  ret <- primIO $ prim__sdlRenderCopy rnd txt src.x src.y src.width src.height
                                              dst.x dst.y dst.width dst.height
  checkRetNum ret

%foreign libsdl "sdl_poll_event"
prim__sdlPollEvent : PrimIO AnyPtr

export
pollEvent : HasIO io => io (Maybe SDLRawEvent)
pollEvent = do
  evt <- primIO prim__sdlPollEvent
  if prim__nullAnyPtr evt /= 1
     then do
       gcEvt <- liftIO $ onCollectAny evt (\ptr => primIO $ prim__sdlFree ptr)
       pure $ Just (RawEvent gcEvt)
     else do
       primIO $ prim__sdlFree evt
       pure Nothing

%foreign libsdl "sdl_get_event_type"
prim__sdlGetEventType : GCAnyPtr -> PrimIO Int

export
eventType : HasIO io => SDLRawEvent -> io SDLEventType
eventType (RawEvent evt) = eventFromInt <$> primIO (prim__sdlGetEventType evt)

public export
SDLRawMouseButtonEvent : Type
SDLRawMouseButtonEvent =
  Struct "sdl_raw_mousebuttonevent" [ ("type", Bits32)
                                    , ("timestamp", Bits32)
                                    , ("windowID", Bits32)
                                    , ("which", Bits32)
                                    , ("button", Bits8)
                                    , ("state", Bits8)
                                    , ("clicks", Bits8)
                                    , ("x", Int)
                                    , ("y", Int)
                                    ]

public export
SDLRawMouseMotionEvent : Type
SDLRawMouseMotionEvent =
  Struct "sdl_raw_mousemotionevent" [ ("type", Bits32)
                                    , ("timestamp", Bits32)
                                    , ("windowID", Bits32)
                                    , ("which", Bits32)
                                    , ("state", Bits32)
                                    , ("x", Int)
                                    , ("y", Int)
                                    , ("xrel", Int)
                                    , ("yrel", Int)
                                    ]

public export
SDLRawKeyboardEvent : Type
SDLRawKeyboardEvent =
  Struct "sdl_raw_keyboardevent" [ ("type", Bits32)
                                 , ("timestamp", Bits32)
                                 , ("windowID", Bits32)
                                 , ("state", Bits8)
                                 , ("repeat", Bits8)
                                 , ("scancode", Int)
                                 , ("keycode", Int)
                                 , ("mod", Bits16)
                                 ]

public export
RawEventStruct : SDLEventType -> Type
RawEventStruct SDLQuit = ()
RawEventStruct SDLWindowEvent = ()
RawEventStruct SDLKeyDown = SDLKeyboardEvent
RawEventStruct SDLKeyUp = SDLKeyboardEvent
RawEventStruct SDLMouseMotion = SDLMouseMotionEvent
RawEventStruct SDLMouseButtonDown = SDLMouseButtonEvent
RawEventStruct SDLMouseButtonUp = SDLMouseButtonEvent
RawEventStruct SDLGenericEvent = ()

%foreign libsdl "sdl_get_mouse_button_event"
prim__sdlGetMouseButtonEvent : GCAnyPtr -> SDLRawMouseButtonEvent

%foreign libsdl "sdl_free_raw_mousebuttonevent"
prim__sdlFreeRawMouseButtonEvent : SDLRawMouseButtonEvent -> PrimIO ()

freeMouseButtonEvent : HasIO io => SDLRawMouseButtonEvent -> io ()
freeMouseButtonEvent evt = primIO $ prim__sdlFreeRawMouseButtonEvent evt

getMouseButtonEvent : SDLRawEvent -> SDLRawMouseButtonEvent
getMouseButtonEvent (RawEvent evt) = prim__sdlGetMouseButtonEvent evt

fromRawMouseButtonEvent : SDLRawMouseButtonEvent -> Maybe SDLMouseButtonEvent
fromRawMouseButtonEvent evt =
  Just $ MkMouseButtonEvent (getField evt "timestamp") (getField evt "windowID") (getField evt "which")
                            !(mouseButtonFromBits $ getField evt "button")
                            !(buttonStateFromBits $ getField evt "state")
                            (getField evt "clicks") (getField evt "x") (getField evt "y")

%foreign libsdl "sdl_get_mouse_motion_event"
prim__sdlGetMouseMotionEvent : GCAnyPtr -> SDLRawMouseMotionEvent

%foreign libsdl "sdl_free_raw_mousemotionevent"
prim__sdlFreeRawMouseMotionEvent : SDLRawMouseMotionEvent -> PrimIO ()

freeMouseMotionEvent : HasIO io => SDLRawMouseMotionEvent -> io ()
freeMouseMotionEvent evt = primIO $ prim__sdlFreeRawMouseMotionEvent evt

getMouseMotionEvent : SDLRawEvent -> SDLRawMouseMotionEvent
getMouseMotionEvent (RawEvent evt) = prim__sdlGetMouseMotionEvent evt

fromRawMouseMotionEvent : SDLRawMouseMotionEvent -> SDLMouseMotionEvent
fromRawMouseMotionEvent evt =
  MkMouseMotionEvent (getField evt "timestamp") (getField evt "windowID") (getField evt "which")
                     (getField evt "state") (getField evt "x") (getField evt "y")
                     (getField evt "xrel") (getField evt "yrel")

%foreign libsdl "sdl_get_keyboard_event"
prim__sdlGetKeyboardEvent : GCAnyPtr -> SDLRawKeyboardEvent

%foreign libsdl "sdl_free_raw_keyboardevent"
prim__sdlFreeRawKeyboardEvent : SDLRawKeyboardEvent -> PrimIO ()

freeKeyboardEvent : HasIO io => SDLRawKeyboardEvent -> io ()
freeKeyboardEvent evt = primIO $ prim__sdlFreeRawKeyboardEvent evt

getKeyboardEvent : SDLRawEvent -> SDLRawKeyboardEvent
getKeyboardEvent (RawEvent evt) = prim__sdlGetKeyboardEvent evt

fromRawKeyboardEvent : SDLRawKeyboardEvent -> Maybe SDLKeyboardEvent
fromRawKeyboardEvent evt =
  Just $ MkKeyboardEvent (getField evt "timestamp") (getField evt "windowID")
                         !(buttonStateFromBits $ getField evt "state")
                         (getField evt "repeat") (getField evt "scancode")
                         (keycodeFromInt $ getField evt "keycode") (keymodsFromBits $ getField evt "mod")

export
getEvent : HasIO io => SDLRawEvent -> io (Maybe (t : SDLEventType ** RawEventStruct t))
getEvent evt = case !(eventType evt) of
  SDLMouseMotion => do
    let raw = getMouseMotionEvent evt
    let evt' = fromRawMouseMotionEvent raw
    freeMouseMotionEvent raw
    pure $ Just (SDLMouseMotion ** evt')
  SDLMouseButtonDown => do
    let raw = getMouseButtonEvent evt
    let evt' = fromRawMouseButtonEvent raw
    freeMouseButtonEvent raw
    pure $ MkDPair SDLMouseButtonDown <$> evt'
  SDLMouseButtonUp => do
    let raw = getMouseButtonEvent evt
    let evt' = fromRawMouseButtonEvent raw
    freeMouseButtonEvent raw
    pure $ MkDPair SDLMouseButtonUp <$> evt'
  SDLKeyDown => do
    let raw = getKeyboardEvent evt
    let evt' = fromRawKeyboardEvent raw
    freeKeyboardEvent raw
    pure $ MkDPair SDLKeyDown <$> evt'
  SDLKeyUp => do
    let raw = getKeyboardEvent evt
    let evt' = fromRawKeyboardEvent raw
    freeKeyboardEvent raw
    pure $ MkDPair SDLKeyUp <$> evt'
  SDLQuit => pure $ Just (SDLQuit ** ())
  SDLWindowEvent => pure $ Just (SDLWindowEvent ** ())
  SDLGenericEvent => pure $ Just (SDLGenericEvent ** ())

%foreign libsdl "sdl_free_raw_surface"
prim__sdlFreeRawSurface : AnyPtr -> PrimIO ()

export
freeSurface : HasIO io => SDLSurface -> io ()
freeSurface (Surface raw) = primIO $ prim__sdlFreeRawSurface raw

%foreign libsdl "sdl_img_load"
prim__sdlImgLoad : String -> PrimIO AnyPtr

export
imgLoad : HasIO io => String -> io (Either SDLError SDLSurface)
imgLoad str = do
  ptr <- primIO $ prim__sdlImgLoad str
  checkRetPtr ptr {handler = getImageError}
  pure $ Right (Surface ptr)

%foreign libsdl "sdl_create_texture_from_surface"
prim__sdlCreateTextureFromSurface : AnyPtr -> AnyPtr -> PrimIO AnyPtr

export
createTextureFromSurface : HasIO io => SDLRenderer -> SDLSurface -> io (Either SDLError SDLTexture)
createTextureFromSurface (Renderer rnd) (Surface srf) = do
  ptr <- primIO $ prim__sdlCreateTextureFromSurface rnd srf
  checkRetPtr ptr
  pure $ Right (Texture ptr)

%foreign libsdl "sdl_destroy_texture"
prim__sdlDestroyTexture : AnyPtr -> PrimIO ()

export
destroyTexture : HasIO io => SDLTexture -> io ()
destroyTexture (Texture raw) = primIO $ prim__sdlDestroyTexture raw

%foreign libsdl "sdl_delay"
prim__sdlDelay : Bits32 -> PrimIO ()

export
delay : HasIO io => Nat -> io ()
delay ms = primIO $ prim__sdlDelay (cast $ natToInteger ms)
