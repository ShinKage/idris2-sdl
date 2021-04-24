#ifndef _IDRIS_SDL_TTF_H
#define _IDRIS_SDL_TTF_H

#include <stdint.h>
#include <stdlib.h>

#include <SDL_ttf.h>

const char* sdl_ttf_get_error();

int sdl_ttf_init();
void sdl_ttf_quit();

void* sdl_ttf_open_font(const char* path, int pt);

void sdl_ttf_close_font(void* font);

#endif
