/*
** $Id: llex.h,v 1.72.1.1 2013/04/12 18:48:47 roberto Exp $
** Lexical Analyzer
** See Copyright Notice in lua.h
*/

#ifndef llex_h
#define llex_h

#include "lobject.h"
#include "lzio.h"
#include "lstate.h"


#define FIRST_RESERVED	257



/*
* WARNING: if you change the order of this enumeration,
* grep "ORDER RESERVED"
*/
enum RESERVED {
  /* terminal symbols denoted by reserved words */
  TK_AND = FIRST_RESERVED, TK_BREAK,
  TK_DO, TK_ELSE, TK_ELSEIF, TK_END, TK_FALSE, TK_FOR, TK_FUNCTION,
  TK_GOTO, TK_IF, TK_IN, TK_LOCAL, TK_NIL, TK_NOT, TK_OR, TK_REPEAT,
  TK_RETURN, TK_THEN, TK_TRUE, TK_UNTIL, TK_WHILE,
  /* other terminal symbols */
  TK_CONCAT, TK_DOTS, TK_EQ, TK_GE, TK_LE, TK_NE, TK_DBCOLON, TK_EOS,
  TK_NUMBER, TK_NAME, TK_STRING
};

/* number of reserved words */
#define NUM_RESERVED	(cast(int, TK_WHILE-FIRST_RESERVED+1))


typedef union {
  lua_Number r;
  TString *ts;
} SemInfo;  /* semantics information */


typedef struct Token {
  int token;
  SemInfo seminfo;
} Token;


/* state of the lexer plus state of the parser when shared by all
   functions */
class LexState {
 private:
  int current;  /* current character (charint) */
  int m_linenumber;  /* input line counter */
  int m_lastline;  /* line of last token `consumed' */
  Token m_t;  /* current token */
  Token lookahead;  /* look ahead token */
  struct lua_State *m_L;
  ZIO *z;  /* input stream */
  Mbuffer *buff;  /* buffer for tokens */
  struct Dyndata *m_dyd;  /* dynamic structures used by the parser */
  TString *m_source;  /* current source name */
  TString *m_envn;  /* environment variable name */
  char decpoint;  /* locale decimal point */
  int llex (SemInfo *seminfo);
  inline int next(void) { current = z->getc(); return current; }
  void incline_number(void);
  inline bool currIsNewline(void) { return (current == '\n' || current == '\r'); }
  inline void save_and_next(void) { save(current), next(); }
  int skip_sep (void);
  void save (int c);
  void read_long_string (SemInfo *seminfo, int sep);
  l_noret lexerror(const char *msg, int token);
  const char *txtToken (int token);
  int check_next (const char *set);
  void buffreplace (char from, char to);
  void read_numeral (SemInfo *seminfo);
  void trydecpoint (SemInfo *seminfo);
  void escError (int *c, int n, const char *msg);
  int readhexaesc (void);
  int readdecesc (void);
  void read_string (int del, SemInfo *seminfo);
 public:
  void check_condition(lu_byte c,const char *msg) { if (!(c)) syntax_error(msg); }
  struct FuncState *fs;  /* current function (parser) */
  inline lua_State* L(void) { return m_L; }
  inline Token &t(void) { return m_t; }
  inline Dyndata* dyd(void) { return m_dyd; }
  inline int lastline(void) { return m_lastline; }
  inline int linenumber(void) { return m_linenumber; }
  inline TString *envn(void) { return m_envn; }
  inline TString *source(void) { return m_source; }
  int look_ahead(void);
  void next_token(void);
  const char *token2str (int token);
  l_noret syntax_error (const char *msg);
  TString *new_string (const char *str, size_t l);
  void set_input (lua_State *L, ZIO *z, TString *source, int firstchar);
  inline void leave_level(void) { m_L->nCcalls--; }
  inline void set_data(Mbuffer *buffer, Dyndata *dyd) { buff = buffer; m_dyd = dyd; }
 };


LUAI_FUNC void luaX_init (lua_State *L);


#endif
