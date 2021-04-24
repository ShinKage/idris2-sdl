#include "idris_sdl_ttf.h"

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
