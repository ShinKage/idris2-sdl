#include "idris_sdl.h"

void sdl_free(void* ptr) {
    free(ptr);
}

const char* sdl_get_error() {
    return SDL_GetError();
}

const char* sdl_img_get_error() {
    return IMG_GetError();
}

int sdl_init(uint32_t flags) {
    return SDL_Init(flags);
}

void sdl_quit() {
    SDL_Quit();
}

void* sdl_create_window(const char* title, int x, int y, int w, int h, uint32_t flags) {
    SDL_Window* window = SDL_CreateWindow(title, x, y, w, h, flags);
    return (void*) window;
}

uint32_t sdl_get_window_id(void* window) {
    return SDL_GetWindowID((SDL_Window*) window);
}

void sdl_destroy_window(void* window) {
    SDL_DestroyWindow((SDL_Window*) window);
}

void* sdl_create_renderer(void* win, int index, uint32_t flags) {
    SDL_Window* window = (SDL_Window*) win;
    SDL_Renderer* renderer = SDL_CreateRenderer(window, index, flags);
    return (void*) renderer;
}

void sdl_destroy_renderer(void* rnd) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    SDL_DestroyRenderer(renderer);
}

int sdl_render_clear(void* rnd) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    return SDL_RenderClear(renderer);
}

void sdl_render_present(void* rnd) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    SDL_RenderPresent(renderer);
}

int sdl_render_set_draw_color(void* rnd, uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    return SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

int sdl_render_draw_point(void* rnd, int x, int y) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    return SDL_RenderDrawPoint(renderer, x, y);
}

int sdl_render_draw_line(void* rnd, int x1, int y1, int x2, int y2) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    return SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
}

int sdl_render_draw_rect(void* rnd, int x, int y, int w, int h) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    SDL_Rect rect = {x, y, w, h};
    return SDL_RenderDrawRect(renderer, &rect);
}

int sdl_render_fill_rect(void* rnd, int x, int y, int w, int h) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    SDL_Rect rect = {x, y, w, h};
    return SDL_RenderFillRect(renderer, &rect);
}

void* sdl_poll_event() {
    SDL_Event* evt = malloc(sizeof(SDL_Event));
    int ret = SDL_PollEvent(evt);
    if (!ret)
        evt = NULL;
    return (void*) evt;
}

int sdl_get_event_type(void* evt) {
    SDL_Event* event = evt;
    return (int) event->type;
}

sdl_raw_mousebuttonevent* sdl_get_mouse_button_event(void* evt) {
    SDL_Event* event = (SDL_Event*) evt;
    sdl_raw_mousebuttonevent* mevt = malloc(sizeof(sdl_raw_mousebuttonevent));
    mevt->type = event->button.type;
    mevt->timestamp = event->button.timestamp;
    mevt->windowID = event->button.windowID;
    mevt->which = event->button.which;
    mevt->button = event->button.button;
    mevt->state = event->button.state;
    mevt->clicks = event->button.clicks;
    mevt->x = event->button.x;
    mevt->y = event->button.y;
    return mevt;
}

void sdl_free_raw_mousebuttonevent(sdl_raw_mousebuttonevent* evt) {
    free(evt);
}

sdl_raw_mousemotionevent* sdl_get_mouse_motion_event(void* evt) {
    SDL_Event* event = (SDL_Event*) evt;
    sdl_raw_mousemotionevent* mevt = malloc(sizeof(sdl_raw_mousemotionevent));
    mevt->type = event->motion.type;
    mevt->timestamp = event->motion.timestamp;
    mevt->windowID = event->motion.windowID;
    mevt->which = event->motion.which;
    mevt->state = event->motion.state;
    mevt->x = event->motion.x;
    mevt->y = event->motion.y;
    mevt->xrel = event->motion.xrel;
    mevt->yrel = event->motion.yrel;
    return mevt;
}

void sdl_free_raw_mousemotionevent(sdl_raw_mousemotionevent* evt) {
    free(evt);
}

sdl_raw_keyboardevent* sdl_get_keyboard_event(void* evt) {
    SDL_Event* event = (SDL_Event*) evt;
    sdl_raw_keyboardevent* kevt = malloc(sizeof(sdl_raw_keyboardevent));
    kevt->type = event->key.type;
    kevt->timestamp = event->key.timestamp;
    kevt->windowID = event->key.windowID;
    kevt->state = event->key.state;
    kevt->repeat = event->key.repeat;
    kevt->scancode = event->key.keysym.scancode;
    kevt->keycode = event->key.keysym.sym;
    kevt->mod = event->key.keysym.mod;
    return kevt;
}

void sdl_free_raw_keyboardevent(sdl_raw_mousebuttonevent* evt) {
    free(evt);
}

void sdl_free_raw_surface(void* srf) {
    SDL_Surface* surface = (SDL_Surface*) srf;
    SDL_FreeSurface(surface);
}

void* sdl_img_load(const char* path) {
    SDL_Surface* surface = IMG_Load(path);
    return (void*) surface;
}

void* sdl_create_texture_from_surface(void* rnd, void* srf) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    SDL_Surface* surface = (SDL_Surface*) srf;
    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    return (void*) texture;
}

void sdl_destroy_texture(void* txt) {
    SDL_Texture* texture = (SDL_Texture*) txt;
    SDL_DestroyTexture(texture);
}

int sdl_render_copy(void* rnd, void* txt, int srcx, int srcy, int srcw, int srch, int dstx, int dsty, int dstw,
                    int dsth) {
    SDL_Renderer* renderer = (SDL_Renderer*) rnd;
    SDL_Texture* texture = (SDL_Texture*) txt;
    SDL_Rect src = {srcx, srcy, srcw, srch};
    SDL_Rect dst = {dstx, dsty, dstw, dsth};
    return SDL_RenderCopy(renderer, texture, &src, &dst);
}

void sdl_delay(uint32_t ms) {
    SDL_Delay(ms);
}

const char* sdl_ttf_get_error() {
    return TTF_GetError();
}

int sdl_ttf_init() {
    return TTF_Init();
}

void sdl_ttf_quit() {
    TTF_Quit();
}

void* sdl_ttf_open_font(const char* path, int ps) {
    TTF_Font* font = TTF_OpenFont(path, ps);
    return (void*) font;
}

void sdl_ttf_close_font(void* font) {
    TTF_CloseFont(font);
}

void* sdl_ttf_render_text_solid(void* font, const char* text, uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    SDL_Color color = {r,g,b,a};
    TTF_RenderText_Solid(font, text, color);
} 
