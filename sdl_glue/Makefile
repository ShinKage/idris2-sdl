IDRIS		:= idris2
MACHINE		:= $(shell $(CC) -dumpmachine)

ifneq (, $(findstring darwin, $(MACHINE)))
	OS	:= darwin
else ifneq (, $(findstring cygwin, $(MACHINE)))
	OS	:= windows
else ifneq (, $(findstring mingw, $(MACHINE)))
	OS	:= windows
else ifneq (, $(findstring windows, $(MACHINE)))
	OS	:= windows
else
	OS	:= unix
endif

ifeq ($(OS), darwin)
	SHLIB_SUFFIX	:= dylib
else ifeq ($(OS), windows)
	SHLIB_SUFFIX	:= dll
else
	SHLIB_SUFFIX	:= so
endif

LIBTARGET = libidrissdl.$(SHLIB_SUFFIX)
VERSION = 0.1.0
INSTALLDIR = `${IDRIS} --libdir`/sdl-$(VERSION)

HDRS = idris_sdl.h 
OBJS = idris_sdl.o

SDL_LIBS := -lSDL2 $(shell sdl2-config --libs) -lSDL2_image -lSDL2_ttf
SDL_FLAGS := $(shell sdl2-config --cflags)

CFLAGS = $(SDL_FLAGS)

$(LIBTARGET): $(OBJS)
	$(CC) -o $@ -shared $< $(SDL_LIBS) $(SDL_FLAGS)

clean:
	rm -f $(OBJS) $(LIBTARGET)

install:
	mkdir -p $(INSTALLDIR)/lib
	mkdir -p $(INSTALLDIR)/include
	install -m 755 $(LIBTARGET) $(INSTALLDIR)/lib
	install -m 644 $(HDRS) $(INSTALLDIR)/include

.PHONY: install clean
