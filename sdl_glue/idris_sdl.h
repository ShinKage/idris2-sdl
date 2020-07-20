#ifndef _IDRIS_SDL_H
#define _IDRIS_SDL_H

#include <stdlib.h>

#include <SDL2/SDL.h>

void sdl_free(void *ptr);
void sdl_delay(int ms);
const char* sdl_get_error();

int sdl_init(int flags);
void sdl_quit();

void* sdl_create_window(const char* title, int x, int y, int w, int h, int flags);
void sdl_destroy_window(void* window);

void* sdl_create_renderer(void* win, int index, int flags);
void sdl_destroy_renderer(void* rnd);
int sdl_render_clear(void* rnd);
void sdl_render_present(void* rnd);
int sdl_render_set_draw_color(void *rnd, int r, int g, int b, int a);
int sdl_render_draw_point(void *rnd, int x, int y);
int sdl_render_draw_line(void *rnd, int x1, int y1, int x2, int y2);
int sdl_render_draw_rect(void *rnd, int x, int y, int w, int h);
int sdl_render_fill_rect(void *rnd, int x, int y, int w, int h);

void* sdl_poll_event();
int sdl_get_event_type(void* evt);

typedef struct {
    int button;
    int state;
    int clicks;
    int x;
    int y;
} sdl_raw_mousebuttonevent;
sdl_raw_mousebuttonevent* sdl_get_mouse_button_event(void* evt);
void sdl_free_raw_mousebuttonevent(sdl_raw_mousebuttonevent* evt);

typedef struct {
    int type;
    int state;
    int repeat;
    int scancode;
    int keycode;
    int mod;
} sdl_raw_keyboardevent;
sdl_raw_keyboardevent* sdl_get_keyboard_event(void* evt);
void sdl_free_raw_keyboardevent(sdl_raw_mousebuttonevent* evt);

#endif
