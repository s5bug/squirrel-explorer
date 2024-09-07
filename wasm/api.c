#include <emscripten.h>
#include <squirrel.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct out_buffer {
    char* content;
    size_t size;
} out_buffer_t;

EMSCRIPTEN_KEEPALIVE
out_buffer_t* out_buffer_init(void) {
    out_buffer_t* ptr = malloc(sizeof(*ptr));
    
    size_t sz = 1024;
    char* content = (char*) malloc(sz);
    
    ptr->size = sz;
    ptr->content = content;
    
    return ptr;
}

EMSCRIPTEN_KEEPALIVE
void out_buffer_close(out_buffer_t* buf) {
    free(buf->content);
    free(buf);
}

void out_buffer_guarantee(out_buffer_t* self, size_t size) {
    if(self->size >= size) return;
    
    while(self->size < size) {
        self->size *= 2;
    }
    // assume this won't fail
    self->content = realloc(self->content, self->size); 
}

EMSCRIPTEN_KEEPALIVE
char* out_buffer_content(out_buffer_t* self) {
    return self->content;
}

typedef struct cursor {
    out_buffer_t* out;
    size_t length;
} cursor_t;

void cursor_write(cursor_t* self, char* buffer, size_t bufferLength) {
    out_buffer_guarantee(self->out, self->length + bufferLength);
    memcpy(self->out->content + self->length, buffer, bufferLength);
    self->length += bufferLength;
}

SQInteger write_to_out_buffer(void* cursor_v, void* buffer_v, SQInteger bufferLength) {
    cursor_t* cursor = cursor_v;
    char* buffer = buffer_v;
    
    cursor_write(cursor, buffer, (size_t) bufferLength);
    return bufferLength;
}

EMSCRIPTEN_KEEPALIVE
size_t compile_and_serialize_buffer(HSQUIRRELVM vm, char* buffer, int bufferSize, char* sourceName, out_buffer_t* pOutBuffer) {
    sq_compilebuffer(vm, buffer, bufferSize, sourceName, false);
    
    cursor_t curs;
    curs.out = pOutBuffer;
    curs.length = 0;
    
    sq_writeclosure(vm, write_to_out_buffer, &curs);
    
    return curs.length;
}
