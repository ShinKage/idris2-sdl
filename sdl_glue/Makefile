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
INSTALLDIR = `${IDRIS} --libdir`/sdl/lib

HDRS = idris_sdl.h
OBJS = idris_sdl.o

SDL_LIBS := -lSDL2
SDL_FLAGS :=

CFLAGS = $(SDL_FLAGS)

$(LIBTARGET): $(OBJS)
	$(CC) -o $@ -shared $< $(SDL_LIBS) $(SDL_FLAGS)

clean:
	rm -f $(OBJS) $(LIBTARGET)

install:
	@if ! [ -d $(INSTALLDIR) ]; then mkdir -p $(INSTALLDIR); fi
	install $(LIBTARGET) $(HDRS) $(INSTALLDIR)

.PHONY: install clean
