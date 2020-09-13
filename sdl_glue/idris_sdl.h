#ifndef _IDRIS_SDL_H
#define _IDRIS_SDL_H

#include <stdlib.h>
#include <stdint.h>

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
    uint32_t type;
    uint32_t timestamp;
    uint32_t windowID;
    uint32_t which;
    uint8_t button;
    uint8_t state;
    uint8_t clicks;
    int32_t x;
    int32_t y;
} sdl_raw_mousebuttonevent;
sdl_raw_mousebuttonevent* sdl_get_mouse_button_event(void* evt);
void sdl_free_raw_mousebuttonevent(sdl_raw_mousebuttonevent* evt);

typedef struct {
    uint32_t type;
    uint32_t timestamp;
    uint32_t windowID;
    uint32_t which;
    uint32_t state;
    int32_t x;
    int32_t y;
    int32_t xrel;
    int32_t yrel;
} sdl_raw_mousemotionevent;
sdl_raw_mousemotionevent* sdl_get_mouse_motion_event(void* evt);
void sdl_free_raw_mousemotionevent(sdl_raw_mousemotionevent* evt);

typedef struct {
    uint32_t type;
    uint32_t timestamp;
    uint32_t windowID;
    uint8_t state;
    uint8_t repeat;
    int scancode;
    int keycode;
    uint16_t mod;
} sdl_raw_keyboardevent;
sdl_raw_keyboardevent* sdl_get_keyboard_event(void* evt);
void sdl_free_raw_keyboardevent(sdl_raw_mousebuttonevent* evt);

#endif
