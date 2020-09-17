.PHONY: build install clean

build:
	idris2 --build sdl.ipkg

install:
	idris2 --install sdl.ipkg

clean:
	idris2 --clean sdl.ipkg
