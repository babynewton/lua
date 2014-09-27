/*
** $Id: lzio.c,v 1.35.1.1 2013/04/12 18:48:47 roberto Exp $
** Buffered streams
** See Copyright Notice in lua.h
*/


#include <string.h>

#define lzio_c
#define LUA_CORE

#include "lua.h"

#include "llimits.h"
#include "lmem.h"
#include "lstate.h"
#include "lzio.h"


int ZIO::fill (void) {
  size_t size;
  lua_State *L = m_L;
  const char *buff;
  lua_unlock(L);
  buff = m_reader->read(L, &size);
  lua_lock(L);
  if (buff == NULL || size == 0)
    return EOZ;
  m_n = size - 1;  /* discount char being returned */
  m_p = buff;
  return cast_uchar(*(m_p++));
}


/* --------------------------------------------------------------- read --- */
size_t ZIO::read (void *b, size_t n) {
  while (n) {
    size_t m;
    if (m_n == 0) {  /* no bytes in buffer? */
      if (fill() == EOZ)  /* try to read more */
        return n;  /* no more input; return number of missing bytes */
      else {
        m_n++;  /* luaZ_fill consumed first byte; put it back */
        m_p--;
      }
    }
    m = (n <= m_n) ? n : m_n;  /* min. between n and z->n */
    memcpy(b, m_p, m);
    m_n -= m;
    m_p += m;
    b = (char *)b + m;
    n -= m;
  }
  return 0;
}

/* ------------------------------------------------------------------------ */
char *luaZ_openspace (lua_State *L, Mbuffer *buff, size_t n) {
  if (n > buff->size()) {
    if (n < LUA_MINBUFFER) n = LUA_MINBUFFER;
    buff->resize(L, n);
  }
  return buff->buffer();
}


