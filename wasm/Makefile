CC ?= emcc
ifeq ($(origin CC),default)
CC = emcc
endif
export CC

CXX ?= em++
ifeq ($(origin CXX),default)
CXX = em++
endif
export CXX

AR ?= emar
ifeq ($(origin AR),default)
AR = emar
endif
export AR

DEBUG_CCFLAGS ?= -O2 -g -fno-rtti -Wall
DEBUG_CXXFLAGS ?= -O2 -g -fno-rtti -Wall -std=gnu++23
RELEASE_CCFLAGS ?= -O2 -fno-rtti -Wall
RELEASE_CXXFLAGS ?= -O2 -fno-rtti -Wall -std=gnu++23

all: release

.PHONY: clean
clean: 
	+$(MAKE) -C SQUIRREL2 clean
	rm -f api.js api.wasm dragonbox.js dragonbox.wasm

sq:
	+$(MAKE) -C SQUIRREL2

api.js api.wasm: api.c
	$(CC) $(CCFLAGS) -s WASM=1 -s MODULARIZE=1 -s EXPORT_ES6=1 -s EXPORTED_FUNCTIONS=_sq_open,_sq_close,_compile_and_serialize_buffer,_out_buffer_init,_out_buffer_close,_out_buffer_content,_free -s EXPORTED_RUNTIME_METHODS=stringToNewUTF8,lengthBytesUTF8,UTF8ToString,HEAPU8 -ISQUIRREL2/include -LSQUIRREL2/lib -lsquirrel api.c -o api.js

dragonbox.js dragonbox.wasm: dragonbox.cpp
	$(CXX) $(CXXFLAGS) -s WASM=1 -s MODULARIZE=1 -s EXPORT_ES6=1 -s EXPORTED_FUNCTIONS=_realloc,_free,_bulk_dragonbox -s EXPORTED_RUNTIME_METHODS=HEAP32,HEAPU32,HEAPF32 dragonbox.cpp -o dragonbox.js

debug: CCFLAGS += $(DEBUG_CCFLAGS)
debug: CXXFLAGS += $(DEBUG_CXXFLAGS)
debug: sq api.js api.wasm dragonbox.js dragonbox.wasm

release: export CCFLAGS += $(RELEASE_CCFLAGS)
release: export CXXFLAGS += $(RELEASE_CXXFLAGS)
release: sq api.js api.wasm dragonbox.js dragonbox.wasm
