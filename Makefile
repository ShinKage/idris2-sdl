# .PHONY: run build-support
# 
# libsdl_support: sdl_support.c
# 	clang -shared $< -o $@.dylib -lSDL2
# 
# build-support: libsdl_support
# 	cp $<.dylib build/exec/_tmpchez_app
# 
# run: build-support
# 	idris2 -p contrib --package idris-sdl.ipkg src/Main.idr --exec main

.PHONY: build install clean

build:
	idris2 --build sdl.ipkg

install:
	idris2 --install sdl.ipkg

clean:
	idris2 --clean sdl.ipkg
