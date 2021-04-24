#ifndef _IDRIS_SDL_H
#define _IDRIS_SDL_H

#include <stdint.h>
#include <stdlib.h>

#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>

void sdl_free(void* ptr);
void sdl_delay(uint32_t ms);
const char* sdl_get_error();
const char* sdl_img_get_error();

int sdl_init(uint32_t flags);
void sdl_quit();

void* sdl_create_window(const char* title, int x, int y, int w, int h, uint32_t flags);
uint32_t sdl_get_window_id(void* window);
void sdl_destroy_window(void* window);

void* sdl_create_renderer(void* win, int index, uint32_t flags);
void sdl_destroy_renderer(void* rnd);
int sdl_render_clear(void* rnd);
void sdl_render_present(void* rnd);
int sdl_render_set_draw_color(void* rnd, uint8_t r, uint8_t g, uint8_t b, uint8_t a);
int sdl_render_draw_point(void* rnd, int x, int y);
int sdl_render_draw_line(void* rnd, int x1, int y1, int x2, int y2);
int sdl_render_draw_rect(void* rnd, int x, int y, int w, int h);
int sdl_render_fill_rect(void* rnd, int x, int y, int w, int h);

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

void sdl_free_raw_surface(void* srf);

void* sdl_create_texture_from_surface(void* rnd, void* srf);
void sdl_destroy_texture(void* txt);

int sdl_render_copy(void* rnd, void* txt, int srcx, int srcy, int srcw, int srch, int dstx, int dsty, int dstw,
                    int dsth);

void* sdl_img_load(const char* path);

const char* sdl_ttf_get_error();

int sdl_ttf_init();
void sdl_ttf_quit();

void* sdl_ttf_open_font(const char* path, int pt);

void sdl_ttf_close_font(void* font);

#endif
