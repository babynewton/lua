/*
** $Id: lzio.h,v 1.26.1.1 2013/04/12 18:48:47 roberto Exp $
** Buffered streams
** See Copyright Notice in lua.h
*/


#ifndef lzio_h
#define lzio_h

#include "lua.h"

#include "lmem.h"


#define EOZ	(-1)			/* end of stream */


typedef struct Mbuffer {
 private:
  char *m_buffer;
  size_t m_n;
  size_t m_buffsize;
 public:
  Mbuffer():m_buffer(NULL), m_buffsize(0) {}
  inline char *buffer(void) { return m_buffer; }
  inline size_t size(void){ return m_buffsize; }
  inline size_t bufflen(void) { return m_n; }
  inline void reset(void) { m_n = 0; }
  inline void resize(lua_State *L, size_t size);
  inline void free(lua_State *L) { resize(L, 0); }
  inline void add(char c) { m_buffer[m_n++] = c; }
} Mbuffer;

void Mbuffer::resize(lua_State *L, size_t size) {
  luaM_reallocvector(L, m_buffer, m_buffsize, size, char);
  m_buffsize = size;
}

LUAI_FUNC char *luaZ_openspace (lua_State *L, Mbuffer *buff, size_t n);



/* --------- Private Part ------------------ */

class ZIO {
 private:
  size_t m_n;			/* bytes still unread */
  const char *m_p;		/* current position in buffer */
  lua_Reader* m_reader;		/* reader function */
  lua_State *m_L;			/* Lua state (for reader) */
  int fill(void);
 public:
  ZIO(lua_State *L, lua_Reader *reader):m_n(0), m_p(NULL), m_reader(reader), m_L(L) {}
  size_t read(void* b, size_t n);
  inline int getc(void) { return (m_n--)>0 ?  cast_uchar(*m_p++) : fill(); }
};


#endif
