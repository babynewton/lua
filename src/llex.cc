/*
** $Id: llex.c,v 2.63.1.2 2013/08/30 15:49:41 roberto Exp $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/


#include <locale.h>
#include <string.h>

#define llex_c
#define LUA_CORE

#include "lua.h"

#include "lctype.h"
#include "ldo.h"
#include "llex.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lzio.h"








/* ORDER RESERVED */
static const char *const luaX_tokens [] = {
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
    "..", "...", "==", ">=", "<=", "~=", "::", "<eof>",
    "<number>", "<name>", "<string>"
};






void LexState::save (int c) {
  Mbuffer *b = buff;
  if (b->bufflen() + 1 > b->size()) {
    size_t newsize;
    if (b->size() >= MAX_SIZET/2)
      lexerror("lexical element too long", 0);
    newsize = b->size() * 2;
    b->resize(m_L, newsize);
  }
  b->add(cast(char, c));
}


void luaX_init (lua_State *L) {
  int i;
  for (i=0; i<NUM_RESERVED; i++) {
    TString *ts = luaS_new(L, luaX_tokens[i]);
    luaS_fix(ts);  /* reserved words are never collected */
    ts->extra = cast_byte(i+1);  /* reserved word */
  }
}


const char *LexState::token2str (int token) {
  if (token < FIRST_RESERVED) {  /* single-byte symbols? */
    lua_assert(token == cast(unsigned char, token));
    return (lisprint(token)) ? luaO_pushfstring(m_L, LUA_QL("%c"), token) :
                              luaO_pushfstring(m_L, "char(%d)", token);
  }
  else {
    const char *s = luaX_tokens[token - FIRST_RESERVED];
    if (token < TK_EOS)  /* fixed format (symbols and reserved words)? */
      return luaO_pushfstring(m_L, LUA_QS, s);
    else  /* names, strings, and numerals */
      return s;
  }
}


const char *LexState::txtToken (int token) {
  switch (token) {
    case TK_NAME:
    case TK_STRING:
    case TK_NUMBER:
      save('\0');
      return luaO_pushfstring(m_L, LUA_QS, buff->buffer());
    default:
      return token2str(token);
  }
}


l_noret LexState::lexerror (const char *msg, int token) {
  char buff[LUA_IDSIZE];
  luaO_chunkid(buff, getstr(m_source), LUA_IDSIZE);
  msg = luaO_pushfstring(m_L, "%s:%d: %s", buff, m_linenumber, msg);
  if (token)
    luaO_pushfstring(m_L, "%s near %s", msg, txtToken(token));
  luaD_throw(m_L, LUA_ERRSYNTAX);
}


l_noret LexState::syntax_error (const char *msg) {
  lexerror(msg, m_t.token);
}


/*
** creates a new string and anchors it in function's table so that
** it will not be collected until the end of the function's compilation
** (by that time it should be anchored in function's prototype)
*/
TString *LexState::new_string (const char *str, size_t l) {
  TValue *o;  /* entry for `str' */
  TString *ts = luaS_newlstr(m_L, str, l);  /* create new string */
  (m_L->top++)->set_value(m_L, ts);  /* temporarily anchor it in stack */
  o = luaH_set(m_L, fs->h, m_L->top - 1);
  if (o->is_nil()) {  /* not in use yet? (see 'addK') */
    /* boolean value does not need GC barrier;
       table has no metatable, so it does not need to invalidate cache */
    o->set_value(true);  /* t[string] = true */
    luaC_checkGC(m_L);
  }
  else {  /* string already present */
    ts = (keyfromval(o))->to_string();  /* re-use value previously stored */
  }
  m_L->top--;  /* remove string from stack */
  return ts;
}


/*
** increment line number and skips newline sequence (any of
** \n, \r, \n\r, or \r\n)
*/
void LexState::incline_number (void) {
  int old = current;
  lua_assert(currIsNewline());
  next();  /* skip `\n' or `\r' */
  if (currIsNewline() && current != old)
    next();  /* skip `\n\r' or `\r\n' */
  if (++m_linenumber >= MAX_INT)
    syntax_error("chunk has too many lines");
}


void LexState::set_input (lua_State *L, ZIO *zio, TString *source,
                    int firstchar) {
  decpoint = '.';
  m_L = L;
  current = firstchar;
  lookahead.token = TK_EOS;  /* no look-ahead token */
  z = zio;
  fs = NULL;
  m_linenumber = 1;
  m_lastline = 1;
  m_source = source;
  m_envn = luaS_new(L, LUA_ENV);  /* create env name */
  luaS_fix(m_envn);  /* never collect this name */
  buff->resize(m_L, LUA_MINBUFFER);  /* initialize buffer */
}



/*
** =======================================================
** LEXICAL ANALYZER
** =======================================================
*/



int LexState::check_next (const char *set) {
  if (current == '\0' || !strchr(set, current))
    return 0;
  save_and_next();
  return 1;
}


/*
** change all characters 'from' in buffer to 'to'
*/
void LexState::buffreplace (char from, char to) {
  size_t n = buff->bufflen();
  char *p = buff->buffer();
  while (n--)
    if (p[n] == from) p[n] = to;
}


#if !defined(getlocaledecpoint)
#define getlocaledecpoint()	(localeconv()->decimal_point[0])
#endif


#define buff2d(b,e)	luaO_str2d(b->buffer(), b->bufflen() - 1, e)

/*
** in case of format error, try to change decimal point separator to
** the one defined in the current locale and check again
*/
void LexState::trydecpoint (SemInfo *seminfo) {
  char old = decpoint;
  decpoint = getlocaledecpoint();
  buffreplace(old, decpoint);  /* try new decimal separator */
  if (!buff2d(buff, &seminfo->r)) {
    /* format error with correct decimal point: no more options */
    buffreplace(decpoint, '.');  /* undo change (for error message) */
    lexerror("malformed number", TK_NUMBER);
  }
}


/* LUA_NUMBER */
/*
** this function is quite liberal in what it accepts, as 'luaO_str2d'
** will reject ill-formed numerals.
*/
void LexState::read_numeral (SemInfo *seminfo) {
  const char *expo = "Ee";
  int first = current;
  lua_assert(lisdigit(current));
  save_and_next();
  if (first == '0' && check_next("Xx"))  /* hexadecimal? */
    expo = "Pp";
  for (;;) {
    if (check_next(expo))  /* exponent part? */
      check_next("+-");  /* optional exponent sign */
    if (lisxdigit(current) || current == '.')
      save_and_next();
    else  break;
  }
  save('\0');
  buffreplace('.', decpoint);  /* follow locale for decimal point */
  if (!buff2d(buff, &seminfo->r))  /* format error? */
    trydecpoint(seminfo); /* try to update decimal point separator */
}


/*
** skip a sequence '[=*[' or ']=*]' and return its number of '='s or
** -1 if sequence is malformed
*/
int LexState::skip_sep (void) {
  int count = 0;
  int s = current;
  lua_assert(s == '[' || s == ']');
  save_and_next();
  while (current == '=') {
    save_and_next();
    count++;
  }
  return (current == s) ? count : (-count) - 1;
}


void LexState::read_long_string (SemInfo *seminfo, int sep) {
  save_and_next();  /* skip 2nd `[' */
  if (currIsNewline())  /* string starts with a newline? */
    incline_number();  /* skip it */
  for (;;) {
    switch (current) {
      case EOZ:
        lexerror((seminfo) ? "unfinished long string" :
                                 "unfinished long comment", TK_EOS);
        break;  /* to avoid warnings */
      case ']': {
        if (skip_sep() == sep) {
          save_and_next();  /* skip 2nd `]' */
          goto endloop;
        }
        break;
      }
      case '\n': case '\r': {
        save('\n');
        incline_number();
        if (!seminfo) buff->reset();  /* avoid wasting space */
        break;
      }
      default: {
        if (seminfo) save_and_next();
        else next();
      }
    }
  } endloop:
  if (seminfo)
    seminfo->ts = new_string(buff->buffer() + (2 + sep),
                                     buff->bufflen() - 2*(2 + sep));
}


void LexState::escError (int *c, int n, const char *msg) {
  int i;
  buff->reset();  /* prepare error message */
  save('\\');
  for (i = 0; i < n && c[i] != EOZ; i++)
    save(c[i]);
  lexerror(msg, TK_STRING);
}


int LexState::readhexaesc (void) {
  int c[3], i;  /* keep input for error message */
  int r = 0;  /* result accumulator */
  c[0] = 'x';  /* for error message */
  for (i = 1; i < 3; i++) {  /* read two hexadecimal digits */
    c[i] = next();
    if (!lisxdigit(c[i]))
      escError(c, i + 1, "hexadecimal digit expected");
    r = (r << 4) + luaO_hexavalue(c[i]);
  }
  return r;
}


int LexState::readdecesc (void) {
  int c[3], i;
  int r = 0;  /* result accumulator */
  for (i = 0; i < 3 && lisdigit(current); i++) {  /* read up to 3 digits */
    c[i] = current;
    r = 10*r + c[i] - '0';
    next();
  }
  if (r > UCHAR_MAX)
    escError(c, i, "decimal escape too large");
  return r;
}


void LexState::read_string (int del, SemInfo *seminfo) {
  save_and_next();  /* keep delimiter (for error messages) */
  while (current != del) {
    switch (current) {
      case EOZ:
        lexerror("unfinished string", TK_EOS);
        break;  /* to avoid warnings */
      case '\n':
      case '\r':
        lexerror("unfinished string", TK_STRING);
        break;  /* to avoid warnings */
      case '\\': {  /* escape sequences */
        int c;  /* final character to be saved */
        next();  /* do not save the `\' */
        switch (current) {
          case 'a': c = '\a'; goto read_save;
          case 'b': c = '\b'; goto read_save;
          case 'f': c = '\f'; goto read_save;
          case 'n': c = '\n'; goto read_save;
          case 'r': c = '\r'; goto read_save;
          case 't': c = '\t'; goto read_save;
          case 'v': c = '\v'; goto read_save;
          case 'x': c = readhexaesc(); goto read_save;
          case '\n': case '\r':
            incline_number(); c = '\n'; goto only_save;
          case '\\': case '\"': case '\'':
            c = current; goto read_save;
          case EOZ: goto no_save;  /* will raise an error next loop */
          case 'z': {  /* zap following span of spaces */
            next();  /* skip the 'z' */
            while (lisspace(current)) {
              if (currIsNewline()) incline_number();
              else next();
            }
            goto no_save;
          }
          default: {
            if (!lisdigit(current))
              escError(&current, 1, "invalid escape sequence");
            /* digital escape \ddd */
            c = readdecesc();
            goto only_save;
          }
        }
       read_save: next();  /* read next character */
       only_save: save(c);  /* save 'c' */
       no_save: break;
      }
      default:
        save_and_next();
    }
  }
  save_and_next();  /* skip delimiter */
  seminfo->ts = new_string(buff->buffer() + 1,
                                   buff->bufflen() - 2);
}


int LexState::llex (SemInfo *seminfo) {
  buff->reset();
  for (;;) {
    switch (current) {
      case '\n': case '\r': {  /* line breaks */
        incline_number();
        break;
      }
      case ' ': case '\f': case '\t': case '\v': {  /* spaces */
        next();
        break;
      }
      case '-': {  /* '-' or '--' (comment) */
        next();
        if (current != '-') return '-';
        /* else is a comment */
        next();
        if (current == '[') {  /* long comment? */
          int sep = skip_sep();
          buff->reset();  /* `skip_sep' may dirty the buffer */
          if (sep >= 0) {
            read_long_string(NULL, sep);  /* skip long comment */
            buff->reset();  /* previous call may dirty the buff. */
            break;
          }
        }
        /* else short comment */
        while (!currIsNewline() && current != EOZ)
          next();  /* skip until end of line (or end of file) */
        break;
      }
      case '[': {  /* long string or simply '[' */
        int sep = skip_sep();
        if (sep >= 0) {
          read_long_string(seminfo, sep);
          return TK_STRING;
        }
        else if (sep == -1) return '[';
        else lexerror("invalid long string delimiter", TK_STRING);
      }
      case '=': {
        next();
        if (current != '=') return '=';
        else { next(); return TK_EQ; }
      }
      case '<': {
        next();
        if (current != '=') return '<';
        else { next(); return TK_LE; }
      }
      case '>': {
        next();
        if (current != '=') return '>';
        else { next(); return TK_GE; }
      }
      case '~': {
        next();
        if (current != '=') return '~';
        else { next(); return TK_NE; }
      }
      case ':': {
        next();
        if (current != ':') return ':';
        else { next(); return TK_DBCOLON; }
      }
      case '"': case '\'': {  /* short literal strings */
        read_string(current, seminfo);
        return TK_STRING;
      }
      case '.': {  /* '.', '..', '...', or number */
        save_and_next();
        if (check_next(".")) {
          if (check_next("."))
            return TK_DOTS;   /* '...' */
          else return TK_CONCAT;   /* '..' */
        }
        else if (!lisdigit(current)) return '.';
        /* else go through */
      }
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9': {
        read_numeral(seminfo);
        return TK_NUMBER;
      }
      case EOZ: {
        return TK_EOS;
      }
      default: {
        if (lislalpha(current)) {  /* identifier or reserved word? */
          TString *ts;
          do {
            save_and_next();
          } while (lislalnum(current));
          ts = new_string(buff->buffer(),
                                  buff->bufflen());
          seminfo->ts = ts;
          if (isreserved(ts))  /* reserved word? */
            return ts->extra - 1 + FIRST_RESERVED;
          else {
            return TK_NAME;
          }
        }
        else {  /* single-char tokens (+ - / ...) */
          int c = current;
          next();
          return c;
        }
      }
    }
  }
}


void LexState::next_token(void) {
  m_lastline = m_linenumber;
  if (lookahead.token != TK_EOS) {  /* is there a look-ahead token? */
    m_t = lookahead;  /* use this one */
    lookahead.token = TK_EOS;  /* and discharge it */
  }
  else
    m_t.token = llex(&m_t.seminfo);  /* read next token */
}


int LexState::look_ahead (void) {
  lua_assert(lookahead.token == TK_EOS);
  lookahead.token = llex(&lookahead.seminfo);
  return lookahead.token;
}

