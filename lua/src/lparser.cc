/*
** $Id: lparser.c,v 2.130.1.1 2013/04/12 18:48:47 roberto Exp $
** Lua Parser
** See Copyright Notice in lua.h
*/


#include <string.h>

#define lparser_c
#define LUA_CORE

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"



/* maximum number of local variables per function (must be smaller
   than 250, due to the bytecode format) */
#define MAXVARS		200


#define hasmultret(k)		((k) == VCALL || (k) == VVARARG)



/*
** nodes for block list (list of active blocks)
*/
typedef struct BlockCnt {
  struct BlockCnt *previous;  /* chain */
  short firstlabel;  /* index of first label in this block */
  short firstgoto;  /* index of first pending goto in this block */
  lu_byte nactvar;  /* # active locals outside the block */
  lu_byte upval;  /* true if some variable in the block is an upvalue */
  lu_byte isloop;  /* true if `block' is a loop */
} BlockCnt;


class lua_Parser {
 private:
  LexState *ls;
  struct Dyndata *dyd;  /* dynamic structures used by the parser */
  TString *envn;  /* environment variable name */
  void open_func(FuncState *fs, BlockCnt *bl);
  void statlist (void);
  int testnext (int c);
  void close_func (void);
  void check_match (int what, int who, int where);
  void codeclosure (expdesc *v);
  void constructor (expdesc *t);
  void body (expdesc *e, int ismethod, int line);
  void parlist (void);
  int explist (expdesc *v);
  void funcargs (expdesc *f, int line);
  void primaryexp (expdesc *v);
  void suffixedexp (expdesc *v);
  void simpleexp (expdesc *v);
  BinOpr subexpr (expdesc *v, int limit);
  void expr (expdesc *v);
  void yindex (expdesc *v);
  void recfield (struct ConsControl *cc);
  void listfield (struct ConsControl *cc);
  void field (struct ConsControl *cc);
  void block (void);
  void statement (void);
  void assignment (struct LHS_assign *lh, int nvars);
  int cond (void);
  void gotostat (int pc);
  void skipnoopstat (void);
  void labelstat (TString *label, int line);
  void whilestat (int line);
  void repeatstat (int line);
  int exp1 (void);
  void forbody (int base, int line, int nvars, int isnum);
  void fornum (TString *varname, int line);
  void forlist (TString *indexname);
  void forstat (int line);
  void test_then_block (int *escapelist);
  void ifstat (int line);
  void localfunc (void);
  void localstat (void);
  void funcstat (int line);
  void exprstat (void);
  void retstat (void);
  void check (int c);
  void checknext (int c);
  TString *str_checkname (void);
  void checkname (expdesc *e);
  void singlevar (expdesc *var);
  void fieldsel (expdesc *v);
  int funcname (expdesc *v);
  void new_localvar (TString *name);
  void new_localvarliteral (const char *name);
  LocVar *getlocvar (FuncState *fs, int i);
  void adjustlocalvars (int nvars);
  void removevars (FuncState *fs, int tolevel);
  int searchvar (FuncState *fs, TString *n);
  int singlevaraux (FuncState *fs, TString *n, expdesc *var, int base);
  void closegoto (int g, Labeldesc *label);
  int findlabel (int g);
  void findgotos (Labeldesc *lb);
  void movegotosout (FuncState *fs, BlockCnt *bl);
  void enterblock (FuncState *fs, BlockCnt *bl, lu_byte isloop);
  void breaklabel (void);
  void leaveblock (FuncState *fs);
 public:
  lua_Parser(LexState *lexstate, Dyndata *dydata):ls(lexstate), dyd(dydata)	{
    envn = luaS_new(ls->L(), LUA_ENV);  /* create env name */
    luaS_fix(envn);  /* never collect this name */
 }
  void mainfunc(FuncState *fs);
};

/*
** prototypes for recursive non-terminal functions
*/


static void anchor_token (LexState *ls) {
  /* last token from outer function must be EOS */
  lua_assert(ls->fs != NULL || ls->t().token == TK_EOS);
  if (ls->t().token == TK_NAME || ls->t().token == TK_STRING) {
    TString *ts = ls->t().seminfo.ts;
    ls->new_string(getstr(ts), ts->len);
  }
}


/* semantic error */
static l_noret semerror (LexState *ls, const char *msg) {
  ls->t().token = 0;  /* remove 'near to' from final message */
  ls->syntax_error(msg);
}


static l_noret error_expected (LexState *ls, int token) {
  ls->syntax_error(
      luaO_pushfstring(ls->L(), "%s expected", ls->token2str(token)));
}


static l_noret errorlimit (FuncState *fs, int limit, const char *what) {
  lua_State *L = fs->ls->L();
  const char *msg;
  int line = fs->f->linedefined;
  const char *where = (line == 0)
                      ? "main function"
                      : luaO_pushfstring(L, "function at line %d", line);
  msg = luaO_pushfstring(L, "too many %s (limit is %d) in %s",
                             what, limit, where);
  fs->ls->syntax_error(msg);
}


static void checklimit (FuncState *fs, int v, int l, const char *what) {
  if (v > l) errorlimit(fs, l, what);
}


int lua_Parser::testnext (int c) {
  if (ls->t().token == c) {
    ls->next_token();
    return 1;
  }
  else return 0;
}


void lua_Parser::check (int c) {
  if (ls->t().token != c)
    error_expected(ls, c);
}


void lua_Parser::checknext (int c) {
  check(c);
  ls->next_token();
}





void lua_Parser::check_match (int what, int who, int where) {
  if (!testnext(what)) {
    if (where == ls->linenumber())
      error_expected(ls, what);
    else {
      ls->syntax_error(luaO_pushfstring(ls->L(),
             "%s expected (to close %s at line %d)",
              ls->token2str(what), ls->token2str(who), where));
    }
  }
}


TString *lua_Parser::str_checkname (void) {
  TString *ts;
  check(TK_NAME);
  ts = ls->t().seminfo.ts;
  ls->next_token();
  return ts;
}


static void init_exp (expdesc *e, expkind k, int i) {
  e->f = e->t = NO_JUMP;
  e->k = k;
  e->u.info = i;
}


static void codestring (LexState *ls, expdesc *e, TString *s) {
  init_exp(e, VK, luaK_stringK(ls->fs, s));
}


void lua_Parser::checkname (expdesc *e) {
  codestring(ls, e, str_checkname());
}


static int registerlocalvar (LexState *ls, TString *varname) {
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int oldsize = f->sizelocvars;
  luaM_growvector(ls->L(), f->locvars, fs->nlocvars, f->sizelocvars,
                  LocVar, SHRT_MAX, "local variables");
  while (oldsize < f->sizelocvars) f->locvars[oldsize++].varname = NULL;
  f->locvars[fs->nlocvars].varname = varname;
  luaC_objbarrier(ls->L(), f, varname);
  return fs->nlocvars++;
}


void lua_Parser::new_localvar (TString *name) {
  FuncState *fs = ls->fs;
  int reg = registerlocalvar(ls, name);
  checklimit(fs, dyd->actvar.n + 1 - fs->firstlocal,
                  MAXVARS, "local variables");
  luaM_growvector(ls->L(), dyd->actvar.arr, dyd->actvar.n + 1,
                  dyd->actvar.size, Vardesc, MAX_INT, "local variables");
  dyd->actvar.arr[dyd->actvar.n++].idx = cast(short, reg);
}


void lua_Parser::new_localvarliteral (const char *name) {
  //TODO:sizeof(name) -> strlen(name)
  new_localvar(ls->new_string(name, (sizeof(name)/sizeof(char))-1));
}

LocVar *lua_Parser::getlocvar (FuncState *fs, int i) {
  int idx = dyd->actvar.arr[fs->firstlocal + i].idx;
  lua_assert(idx < fs->nlocvars);
  return &fs->f->locvars[idx];
}


void lua_Parser::adjustlocalvars (int nvars) {
  FuncState *fs = ls->fs;
  fs->nactvar = cast_byte(fs->nactvar + nvars);
  for (; nvars; nvars--) {
    getlocvar(fs, fs->nactvar - nvars)->startpc = fs->pc;
  }
}


void lua_Parser::removevars (FuncState *fs, int tolevel) {
  dyd->actvar.n -= (fs->nactvar - tolevel);
  while (fs->nactvar > tolevel)
    getlocvar(fs, --fs->nactvar)->endpc = fs->pc;
}


static int searchupvalue (FuncState *fs, TString *name) {
  int i;
  Upvaldesc *up = fs->f->upvalues;
  for (i = 0; i < fs->nups; i++) {
    if (luaS_eqstr(up[i].name, name)) return i;
  }
  return -1;  /* not found */
}


static int newupvalue (FuncState *fs, TString *name, expdesc *v) {
  Proto *f = fs->f;
  int oldsize = f->sizeupvalues;
  checklimit(fs, fs->nups + 1, MAXUPVAL, "upvalues");
  luaM_growvector(fs->ls->L(), f->upvalues, fs->nups, f->sizeupvalues,
                  Upvaldesc, MAXUPVAL, "upvalues");
  while (oldsize < f->sizeupvalues) f->upvalues[oldsize++].name = NULL;
  f->upvalues[fs->nups].instack = (v->k == VLOCAL);
  f->upvalues[fs->nups].idx = cast_byte(v->u.info);
  f->upvalues[fs->nups].name = name;
  luaC_objbarrier(fs->ls->L(), f, name);
  return fs->nups++;
}


int lua_Parser::searchvar (FuncState *fs, TString *n) {
  int i;
  for (i = cast_int(fs->nactvar) - 1; i >= 0; i--) {
    if (luaS_eqstr(n, getlocvar(fs, i)->varname))
      return i;
  }
  return -1;  /* not found */
}


/*
  Mark block where variable at given level was defined
  (to emit close instructions later).
*/
static void markupval (FuncState *fs, int level) {
  BlockCnt *bl = fs->bl;
  while (bl->nactvar > level) bl = bl->previous;
  bl->upval = 1;
}


/*
  Find variable with given name 'n'. If it is an upvalue, add this
  upvalue into all intermediate functions.
*/
int lua_Parser::singlevaraux (FuncState *fs, TString *n, expdesc *var, int base) {
  if (fs == NULL)  /* no more levels? */
    return VVOID;  /* default is global */
  else {
    int v = searchvar(fs, n);  /* look up locals at current level */
    if (v >= 0) {  /* found? */
      init_exp(var, VLOCAL, v);  /* variable is local */
      if (!base)
        markupval(fs, v);  /* local will be used as an upval */
      return VLOCAL;
    }
    else {  /* not found as local at current level; try upvalues */
      int idx = searchupvalue(fs, n);  /* try existing upvalues */
      if (idx < 0) {  /* not found? */
        if (singlevaraux(fs->prev, n, var, 0) == VVOID) /* try upper levels */
          return VVOID;  /* not found; is a global */
        /* else was LOCAL or UPVAL */
        idx  = newupvalue(fs, n, var);  /* will be a new upvalue */
      }
      init_exp(var, VUPVAL, idx);
      return VUPVAL;
    }
  }
}


void lua_Parser::singlevar (expdesc *var) {
  TString *varname = str_checkname();
  FuncState *fs = ls->fs;
  if (singlevaraux(fs, varname, var, 1) == VVOID) {  /* global name? */
    expdesc key;
    singlevaraux(fs, envn, var, 1);  /* get environment variable */
    lua_assert(var->k == VLOCAL || var->k == VUPVAL);
    codestring(ls, &key, varname);  /* key is variable name */
    luaK_indexed(fs, var, &key);  /* env[varname] */
  }
}


static void adjust_assign (LexState *ls, int nvars, int nexps, expdesc *e) {
  FuncState *fs = ls->fs;
  int extra = nvars - nexps;
  if (hasmultret(e->k)) {
    extra++;  /* includes call itself */
    if (extra < 0) extra = 0;
    luaK_setreturns(fs, e, extra);  /* last exp. provides the difference */
    if (extra > 1) luaK_reserveregs(fs, extra-1);
  }
  else {
    if (e->k != VVOID) luaK_exp2nextreg(fs, e);  /* close last expression */
    if (extra > 0) {
      int reg = fs->freereg;
      luaK_reserveregs(fs, extra);
      luaK_nil(fs, reg, extra);
    }
  }
}


static void enterlevel (LexState *ls) {
  lua_State *L = ls->L();
  ++L->nCcalls;
  checklimit(ls->fs, L->nCcalls, LUAI_MAXCCALLS, "C levels");
}




void lua_Parser::closegoto (int g, Labeldesc *label) {
  int i;
  FuncState *fs = ls->fs;
  Labellist *gl = &dyd->gt;
  Labeldesc *gt = &gl->arr[g];
  lua_assert(luaS_eqstr(gt->name, label->name));
  if (gt->nactvar < label->nactvar) {
    TString *vname = getlocvar(fs, gt->nactvar)->varname;
    const char *msg = luaO_pushfstring(ls->L(),
      "<goto %s> at line %d jumps into the scope of local " LUA_QS,
      getstr(gt->name), gt->line, getstr(vname));
    semerror(ls, msg);
  }
  luaK_patchlist(fs, gt->pc, label->pc);
  /* remove goto from pending list */
  for (i = g; i < gl->n - 1; i++)
    gl->arr[i] = gl->arr[i + 1];
  gl->n--;
}


/*
** try to close a goto with existing labels; this solves backward jumps
*/
int lua_Parser::findlabel (int g) {
  int i;
  BlockCnt *bl = ls->fs->bl;
  Labeldesc *gt = &dyd->gt.arr[g];
  /* check labels in current block for a match */
  for (i = bl->firstlabel; i < dyd->label.n; i++) {
    Labeldesc *lb = &dyd->label.arr[i];
    if (luaS_eqstr(lb->name, gt->name)) {  /* correct label? */
      if (gt->nactvar > lb->nactvar &&
          (bl->upval || dyd->label.n > bl->firstlabel))
        luaK_patchclose(ls->fs, gt->pc, lb->nactvar);
      closegoto(g, lb);  /* close it */
      return 1;
    }
  }
  return 0;  /* label not found; cannot close goto */
}


static int newlabelentry (LexState *ls, Labellist *l, TString *name,
                          int line, int pc) {
  int n = l->n;
  luaM_growvector(ls->L(), l->arr, n, l->size,
                  Labeldesc, SHRT_MAX, "labels/gotos");
  l->arr[n].name = name;
  l->arr[n].line = line;
  l->arr[n].nactvar = ls->fs->nactvar;
  l->arr[n].pc = pc;
  l->n++;
  return n;
}


/*
** check whether new label 'lb' matches any pending gotos in current
** block; solves forward jumps
*/
void lua_Parser::findgotos (Labeldesc *lb) {
  Labellist *gl = &dyd->gt;
  int i = ls->fs->bl->firstgoto;
  while (i < gl->n) {
    if (luaS_eqstr(gl->arr[i].name, lb->name))
      closegoto(i, lb);
    else
      i++;
  }
}


/*
** "export" pending gotos to outer level, to check them against
** outer labels; if the block being exited has upvalues, and
** the goto exits the scope of any variable (which can be the
** upvalue), close those variables being exited.
*/
void lua_Parser::movegotosout (FuncState *fs, BlockCnt *bl) {
  int i = bl->firstgoto;
  Labellist *gl = &dyd->gt;
  /* correct pending gotos to current block and try to close it
     with visible labels */
  while (i < gl->n) {
    Labeldesc *gt = &gl->arr[i];
    if (gt->nactvar > bl->nactvar) {
      if (bl->upval)
        luaK_patchclose(fs, gt->pc, bl->nactvar);
      gt->nactvar = bl->nactvar;
    }
    if (!findlabel(i))
      i++;  /* move to next one */
  }
}


void lua_Parser::enterblock (FuncState *fs, BlockCnt *bl, lu_byte isloop) {
  bl->isloop = isloop;
  bl->nactvar = fs->nactvar;
  bl->firstlabel = dyd->label.n;
  bl->firstgoto = dyd->gt.n;
  bl->upval = 0;
  bl->previous = fs->bl;
  fs->bl = bl;
  lua_assert(fs->freereg == fs->nactvar);
}


/*
** create a label named "break" to resolve break statements
*/
void lua_Parser::breaklabel (void) {
  TString *n = luaS_new(ls->L(), "break");
  int l = newlabelentry(ls, &dyd->label, n, 0, ls->fs->pc);
  findgotos(&dyd->label.arr[l]);
}

/*
** generates an error for an undefined 'goto'; choose appropriate
** message when label name is a reserved word (which can only be 'break')
*/
static l_noret undefgoto (LexState *ls, Labeldesc *gt) {
  const char *msg = isreserved(gt->name)
                    ? "<%s> at line %d not inside a loop"
                    : "no visible label " LUA_QS " for <goto> at line %d";
  msg = luaO_pushfstring(ls->L(), msg, getstr(gt->name), gt->line);
  semerror(ls, msg);
}


void lua_Parser::leaveblock (FuncState *fs) {
  BlockCnt *bl = fs->bl;
  LexState *ls = fs->ls;
  if (bl->previous && bl->upval) {
    /* create a 'jump to here' to close upvalues */
    int j = luaK_jump(fs);
    luaK_patchclose(fs, j, bl->nactvar);
    luaK_patchtohere(fs, j);
  }
  if (bl->isloop)
    breaklabel();  /* close pending breaks */
  fs->bl = bl->previous;
  removevars(fs, bl->nactvar);
  lua_assert(bl->nactvar == fs->nactvar);
  fs->freereg = fs->nactvar;  /* free registers */
  dyd->label.n = bl->firstlabel;  /* remove local labels */
  if (bl->previous)  /* inner block? */
    movegotosout(fs, bl);  /* update pending gotos to outer block */
  else if (bl->firstgoto < dyd->gt.n)  /* pending gotos in outer block? */
    undefgoto(ls, &dyd->gt.arr[bl->firstgoto]);  /* error */
}


/*
** adds a new prototype into list of prototypes
*/
static Proto *addprototype (LexState *ls) {
  Proto *clp;
  lua_State *L = ls->L();
  FuncState *fs = ls->fs;
  Proto *f = fs->f;  /* prototype of current function */
  if (fs->np >= f->sizep) {
    int oldsize = f->sizep;
    luaM_growvector(L, f->p, fs->np, f->sizep, Proto *, MAXARG_Bx, "functions");
    while (oldsize < f->sizep) f->p[oldsize++] = NULL;
  }
  f->p[fs->np++] = clp = luaF_newproto(L);
  luaC_objbarrier(L, f, clp);
  return clp;
}


/*
** codes instruction to create new closure in parent function.
** The OP_CLOSURE instruction must use the last available register,
** so that, if it invokes the GC, the GC knows which registers
** are in use at that time.
*/
void lua_Parser::codeclosure (expdesc *v) {
  FuncState *fs = ls->fs->prev;
  init_exp(v, VRELOCABLE, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np - 1));
  luaK_exp2nextreg(fs, v);  /* fix it at the last register */
}


void lua_Parser::open_func (FuncState *fs, BlockCnt *bl) {
  lua_State *L = ls->L();
  Proto *f;
  fs->prev = ls->fs;  /* linked list of funcstates */
  fs->ls = ls;
  ls->fs = fs;
  fs->pc = 0;
  fs->lasttarget = 0;
  fs->jpc = NO_JUMP;
  fs->freereg = 0;
  fs->nk = 0;
  fs->np = 0;
  fs->nups = 0;
  fs->nlocvars = 0;
  fs->nactvar = 0;
  fs->firstlocal = dyd->actvar.n;
  fs->bl = NULL;
  f = fs->f;
  f->source = ls->source();
  f->maxstacksize = 2;  /* registers 0/1 are always valid */
  fs->h = luaH_new(L);
  /* anchor table of constants (to avoid being collected) */
  L->top->set_value(L, fs->h);
  incr_top(L);
  enterblock(fs, bl, 0);
}


void lua_Parser::close_func (void) {
  lua_State *L = ls->L();
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  luaK_ret(fs, 0, 0);  /* final return */
  leaveblock(fs);
  luaM_reallocvector(L, f->code, f->sizecode, fs->pc, Instruction);
  f->sizecode = fs->pc;
  luaM_reallocvector(L, f->lineinfo, f->sizelineinfo, fs->pc, int);
  f->sizelineinfo = fs->pc;
  luaM_reallocvector(L, f->k, f->sizek, fs->nk, TValue);
  f->sizek = fs->nk;
  luaM_reallocvector(L, f->p, f->sizep, fs->np, Proto *);
  f->sizep = fs->np;
  luaM_reallocvector(L, f->locvars, f->sizelocvars, fs->nlocvars, LocVar);
  f->sizelocvars = fs->nlocvars;
  luaM_reallocvector(L, f->upvalues, f->sizeupvalues, fs->nups, Upvaldesc);
  f->sizeupvalues = fs->nups;
  lua_assert(fs->bl == NULL);
  ls->fs = fs->prev;
  /* last token read was anchored in defunct function; must re-anchor it */
  anchor_token(ls);
  L->top--;  /* pop table of constants */
  luaC_checkGC(L);
}



/*============================================================*/
/* GRAMMAR RULES */
/*============================================================*/


/*
** check whether current token is in the follow set of a block.
** 'until' closes syntactical blocks, but do not close scope,
** so it handled in separate.
*/
static int block_follow (LexState *ls, int withuntil) {
  switch (ls->t().token) {
    case TK_ELSE: case TK_ELSEIF:
    case TK_END: case TK_EOS:
      return 1;
    case TK_UNTIL: return withuntil;
    default: return 0;
  }
}


void lua_Parser::statlist (void) {
  /* statlist -> { stat [`;'] } */
  while (!block_follow(ls, 1)) {
    if (ls->t().token == TK_RETURN) {
      statement();
      return;  /* 'return' must be last statement */
    }
    statement();
  }
}


void lua_Parser::fieldsel (expdesc *v) {
  /* fieldsel -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  expdesc key;
  luaK_exp2anyregup(fs, v);
  ls->next_token();  /* skip the dot or colon */
  checkname(&key);
  luaK_indexed(fs, v, &key);
}


void lua_Parser::yindex (expdesc *v) {
  /* index -> '[' expr ']' */
  ls->next_token();  /* skip the '[' */
  expr(v);
  luaK_exp2val(ls->fs, v);
  checknext(']');
}


/*
** {======================================================================
** Rules for Constructors
** =======================================================================
*/


struct ConsControl {
  expdesc v;  /* last list item read */
  expdesc *t;  /* table descriptor */
  int nh;  /* total number of `record' elements */
  int na;  /* total number of array elements */
  int tostore;  /* number of array elements pending to be stored */
};


void lua_Parser::recfield (struct ConsControl *cc) {
  /* recfield -> (NAME | `['exp1`]') = exp1 */
  FuncState *fs = ls->fs;
  int reg = ls->fs->freereg;
  expdesc key, val;
  int rkkey;
  if (ls->t().token == TK_NAME) {
    checklimit(fs, cc->nh, MAX_INT, "items in a constructor");
    checkname(&key);
  }
  else  /* ls->t().token == '[' */
    yindex(&key);
  cc->nh++;
  checknext('=');
  rkkey = luaK_exp2RK(fs, &key);
  expr(&val);
  luaK_codeABC(fs, OP_SETTABLE, cc->t->u.info, rkkey, luaK_exp2RK(fs, &val));
  fs->freereg = reg;  /* free registers */
}


static void closelistfield (FuncState *fs, struct ConsControl *cc) {
  if (cc->v.k == VVOID) return;  /* there is no list item */
  luaK_exp2nextreg(fs, &cc->v);
  cc->v.k = VVOID;
  if (cc->tostore == LFIELDS_PER_FLUSH) {
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);  /* flush */
    cc->tostore = 0;  /* no more items pending */
  }
}


static void lastlistfield (FuncState *fs, struct ConsControl *cc) {
  if (cc->tostore == 0) return;
  if (hasmultret(cc->v.k)) {
    luaK_setmultret(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.info, cc->na, LUA_MULTRET);
    cc->na--;  /* do not count last expression (unknown number of elements) */
  }
  else {
    if (cc->v.k != VVOID)
      luaK_exp2nextreg(fs, &cc->v);
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);
  }
}


void lua_Parser::listfield (struct ConsControl *cc) {
  /* listfield -> exp */
  expr(&cc->v);
  checklimit(ls->fs, cc->na, MAX_INT, "items in a constructor");
  cc->na++;
  cc->tostore++;
}


void lua_Parser::field (struct ConsControl *cc) {
  /* field -> listfield | recfield */
  switch(ls->t().token) {
    case TK_NAME: {  /* may be 'listfield' or 'recfield' */
      if (ls->look_ahead() != '=')  /* expression? */
        listfield(cc);
      else
        recfield(cc);
      break;
    }
    case '[': {
      recfield(cc);
      break;
    }
    default: {
      listfield(cc);
      break;
    }
  }
}


void lua_Parser::constructor (expdesc *t) {
  /* constructor -> '{' [ field { sep field } [sep] ] '}'
     sep -> ',' | ';' */
  FuncState *fs = ls->fs;
  int line = ls->linenumber();
  int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);
  struct ConsControl cc;
  cc.na = cc.nh = cc.tostore = 0;
  cc.t = t;
  init_exp(t, VRELOCABLE, pc);
  init_exp(&cc.v, VVOID, 0);  /* no value (yet) */
  luaK_exp2nextreg(ls->fs, t);  /* fix it at stack top */
  checknext('{');
  do {
    lua_assert(cc.v.k == VVOID || cc.tostore > 0);
    if (ls->t().token == '}') break;
    closelistfield(fs, &cc);
    field(&cc);
  } while (testnext(',') || testnext(';'));
  check_match('}', '{', line);
  lastlistfield(fs, &cc);
  SETARG_B(fs->f->code[pc], luaO_int2fb(cc.na)); /* set initial array size */
  SETARG_C(fs->f->code[pc], luaO_int2fb(cc.nh));  /* set initial table size */
}

/* }====================================================================== */



void lua_Parser::parlist (void) {
  /* parlist -> [ param { `,' param } ] */
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int nparams = 0;
  f->is_vararg = 0;
  if (ls->t().token != ')') {  /* is `parlist' not empty? */
    do {
      switch (ls->t().token) {
        case TK_NAME: {  /* param -> NAME */
          new_localvar(str_checkname());
          nparams++;
          break;
        }
        case TK_DOTS: {  /* param -> `...' */
          ls->next_token();
          f->is_vararg = 1;
          break;
        }
        default: ls->syntax_error("<name> or " LUA_QL("...") " expected");
      }
    } while (!f->is_vararg && testnext(','));
  }
  adjustlocalvars(nparams);
  f->numparams = cast_byte(fs->nactvar);
  luaK_reserveregs(fs, fs->nactvar);  /* reserve register for parameters */
}


void lua_Parser::body (expdesc *e, int ismethod, int line) {
  /* body ->  `(' parlist `)' block END */
  FuncState new_fs;
  BlockCnt bl;
  new_fs.f = addprototype(ls);
  new_fs.f->linedefined = line;
  open_func(&new_fs, &bl);
  checknext('(');
  if (ismethod) {
    new_localvarliteral("self");  /* create 'self' parameter */
    adjustlocalvars(1);
  }
  parlist();
  checknext(')');
  statlist();
  new_fs.f->lastlinedefined = ls->linenumber();
  check_match(TK_END, TK_FUNCTION, line);
  codeclosure(e);
  close_func();
}


int lua_Parser::explist (expdesc *v) {
  /* explist -> expr { `,' expr } */
  int n = 1;  /* at least one expression */
  expr(v);
  while (testnext(',')) {
    luaK_exp2nextreg(ls->fs, v);
    expr(v);
    n++;
  }
  return n;
}


void lua_Parser::funcargs (expdesc *f, int line) {
  FuncState *fs = ls->fs;
  expdesc args;
  int base, nparams;
  switch (ls->t().token) {
    case '(': {  /* funcargs -> `(' [ explist ] `)' */
      ls->next_token();
      if (ls->t().token == ')')  /* arg list is empty? */
        args.k = VVOID;
      else {
        explist(&args);
        luaK_setmultret(fs, &args);
      }
      check_match(')', '(', line);
      break;
    }
    case '{': {  /* funcargs -> constructor */
      constructor(&args);
      break;
    }
    case TK_STRING: {  /* funcargs -> STRING */
      codestring(ls, &args, ls->t().seminfo.ts);
      ls->next_token();  /* must use `seminfo' before `next' */
      break;
    }
    default: {
      ls->syntax_error("function arguments expected");
    }
  }
  lua_assert(f->k == VNONRELOC);
  base = f->u.info;  /* base register for call */
  if (hasmultret(args.k))
    nparams = LUA_MULTRET;  /* open call */
  else {
    if (args.k != VVOID)
      luaK_exp2nextreg(fs, &args);  /* close last argument */
    nparams = fs->freereg - (base+1);
  }
  init_exp(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams+1, 2));
  luaK_fixline(fs, line);
  fs->freereg = base+1;  /* call remove function and arguments and leaves
                            (unless changed) one result */
}




/*
** {======================================================================
** Expression parsing
** =======================================================================
*/


void lua_Parser::primaryexp (expdesc *v) {
  /* primaryexp -> NAME | '(' expr ')' */
  switch (ls->t().token) {
    case '(': {
      int line = ls->linenumber();
      ls->next_token();
      expr(v);
      check_match(')', '(', line);
      luaK_dischargevars(ls->fs, v);
      return;
    }
    case TK_NAME: {
      singlevar(v);
      return;
    }
    default: {
      ls->syntax_error("unexpected symbol");
    }
  }
}


void lua_Parser::suffixedexp (expdesc *v) {
  /* suffixedexp ->
       primaryexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs } */
  FuncState *fs = ls->fs;
  int line = ls->linenumber();
  primaryexp(v);
  for (;;) {
    switch (ls->t().token) {
      case '.': {  /* fieldsel */
        fieldsel(v);
        break;
      }
      case '[': {  /* `[' exp1 `]' */
        expdesc key;
        luaK_exp2anyregup(fs, v);
        yindex(&key);
        luaK_indexed(fs, v, &key);
        break;
      }
      case ':': {  /* `:' NAME funcargs */
        expdesc key;
        ls->next_token();
        checkname(&key);
        luaK_self(fs, v, &key);
        funcargs(v, line);
        break;
      }
      case '(': case TK_STRING: case '{': {  /* funcargs */
        luaK_exp2nextreg(fs, v);
        funcargs(v, line);
        break;
      }
      default: return;
    }
  }
}


void lua_Parser::simpleexp (expdesc *v) {
  /* simpleexp -> NUMBER | STRING | NIL | TRUE | FALSE | ... |
                  constructor | FUNCTION body | suffixedexp */
  switch (ls->t().token) {
    case TK_NUMBER: {
      init_exp(v, VKNUM, 0);
      v->u.nval = ls->t().seminfo.r;
      break;
    }
    case TK_STRING: {
      codestring(ls, v, ls->t().seminfo.ts);
      break;
    }
    case TK_NIL: {
      init_exp(v, VNIL, 0);
      break;
    }
    case TK_TRUE: {
      init_exp(v, VTRUE, 0);
      break;
    }
    case TK_FALSE: {
      init_exp(v, VFALSE, 0);
      break;
    }
    case TK_DOTS: {  /* vararg */
      FuncState *fs = ls->fs;
      ls->check_condition(fs->f->is_vararg,
                      "cannot use " LUA_QL("...") " outside a vararg function");
      init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 1, 0));
      break;
    }
    case '{': {  /* constructor */
      constructor(v);
      return;
    }
    case TK_FUNCTION: {
      ls->next_token();
      body(v, 0, ls->linenumber());
      return;
    }
    default: {
      suffixedexp(v);
      return;
    }
  }
  ls->next_token();
}


static UnOpr getunopr (int op) {
  switch (op) {
    case TK_NOT: return OPR_NOT;
    case '-': return OPR_MINUS;
    case '#': return OPR_LEN;
    default: return OPR_NOUNOPR;
  }
}


static BinOpr getbinopr (int op) {
  switch (op) {
    case '+': return OPR_ADD;
    case '-': return OPR_SUB;
    case '*': return OPR_MUL;
    case '/': return OPR_DIV;
    case '%': return OPR_MOD;
    case '^': return OPR_POW;
    case TK_CONCAT: return OPR_CONCAT;
    case TK_NE: return OPR_NE;
    case TK_EQ: return OPR_EQ;
    case '<': return OPR_LT;
    case TK_LE: return OPR_LE;
    case '>': return OPR_GT;
    case TK_GE: return OPR_GE;
    case TK_AND: return OPR_AND;
    case TK_OR: return OPR_OR;
    default: return OPR_NOBINOPR;
  }
}


static const struct {
  lu_byte left;  /* left priority for each binary operator */
  lu_byte right; /* right priority */
} priority[] = {  /* ORDER OPR */
   {6, 6}, {6, 6}, {7, 7}, {7, 7}, {7, 7},  /* `+' `-' `*' `/' `%' */
   {10, 9}, {5, 4},                 /* ^, .. (right associative) */
   {3, 3}, {3, 3}, {3, 3},          /* ==, <, <= */
   {3, 3}, {3, 3}, {3, 3},          /* ~=, >, >= */
   {2, 2}, {1, 1}                   /* and, or */
};

#define UNARY_PRIORITY	8  /* priority for unary operators */


/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where `binop' is any binary operator with a priority higher than `limit'
*/
BinOpr lua_Parser::subexpr (expdesc *v, int limit) {
  BinOpr op;
  UnOpr uop;
  enterlevel(ls);
  uop = getunopr(ls->t().token);
  if (uop != OPR_NOUNOPR) {
    int line = ls->linenumber();
    ls->next_token();
    subexpr(v, UNARY_PRIORITY);
    luaK_prefix(ls->fs, uop, v, line);
  }
  else simpleexp(v);
  /* expand while operators have priorities higher than `limit' */
  op = getbinopr(ls->t().token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    expdesc v2;
    BinOpr nextop;
    int line = ls->linenumber();
    ls->next_token();
    luaK_infix(ls->fs, op, v);
    /* read sub-expression with higher priority */
    nextop = subexpr(&v2, priority[op].right);
    luaK_posfix(ls->fs, op, v, &v2, line);
    op = nextop;
  }
  ls->leave_level();
  return op;  /* return first untreated operator */
}


void lua_Parser::expr (expdesc *v) {
  subexpr(v, 0);
}

/* }==================================================================== */



/*
** {======================================================================
** Rules for Statements
** =======================================================================
*/


void lua_Parser::block (void) {
  /* block -> statlist */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  enterblock(fs, &bl, 0);
  statlist();
  leaveblock(fs);
}


/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
struct LHS_assign {
  struct LHS_assign *prev;
  expdesc v;  /* variable (global, local, upvalue, or indexed) */
};


/*
** check whether, in an assignment to an upvalue/local variable, the
** upvalue/local variable is begin used in a previous assignment to a
** table. If so, save original upvalue/local value in a safe place and
** use this safe copy in the previous assignment.
*/
static void check_conflict (LexState *ls, struct LHS_assign *lh, expdesc *v) {
  FuncState *fs = ls->fs;
  int extra = fs->freereg;  /* eventual position to save local variable */
  int conflict = 0;
  for (; lh; lh = lh->prev) {  /* check all previous assignments */
    if (lh->v.k == VINDEXED) {  /* assigning to a table? */
      /* table is the upvalue/local being assigned now? */
      if (lh->v.u.ind.vt == v->k && lh->v.u.ind.t == v->u.info) {
        conflict = 1;
        lh->v.u.ind.vt = VLOCAL;
        lh->v.u.ind.t = extra;  /* previous assignment will use safe copy */
      }
      /* index is the local being assigned? (index cannot be upvalue) */
      if (v->k == VLOCAL && lh->v.u.ind.idx == v->u.info) {
        conflict = 1;
        lh->v.u.ind.idx = extra;  /* previous assignment will use safe copy */
      }
    }
  }
  if (conflict) {
    /* copy upvalue/local value to a temporary (in position 'extra') */
    OpCode op = (v->k == VLOCAL) ? OP_MOVE : OP_GETUPVAL;
    luaK_codeABC(fs, op, extra, v->u.info, 0);
    luaK_reserveregs(fs, 1);
  }
}


void lua_Parser::assignment (struct LHS_assign *lh, int nvars) {
  expdesc e;
  ls->check_condition(vkisvar(lh->v.k), "syntax error");
  if (testnext(',')) {  /* assignment -> ',' suffixedexp assignment */
    struct LHS_assign nv;
    nv.prev = lh;
    suffixedexp(&nv.v);
    if (nv.v.k != VINDEXED)
      check_conflict(ls, lh, &nv.v);
    checklimit(ls->fs, nvars + ls->L()->nCcalls, LUAI_MAXCCALLS,
                    "C levels");
    assignment(&nv, nvars+1);
  }
  else {  /* assignment -> `=' explist */
    int nexps;
    checknext('=');
    nexps = explist(&e);
    if (nexps != nvars) {
      adjust_assign(ls, nvars, nexps, &e);
      if (nexps > nvars)
        ls->fs->freereg -= nexps - nvars;  /* remove extra values */
    }
    else {
      luaK_setoneret(ls->fs, &e);  /* close last expression */
      luaK_storevar(ls->fs, &lh->v, &e);
      return;  /* avoid default */
    }
  }
  init_exp(&e, VNONRELOC, ls->fs->freereg-1);  /* default assignment */
  luaK_storevar(ls->fs, &lh->v, &e);
}


int lua_Parser::cond (void) {
  /* cond -> exp */
  expdesc v;
  expr(&v);  /* read condition */
  if (v.k == VNIL) v.k = VFALSE;  /* `falses' are all equal here */
  luaK_goiftrue(ls->fs, &v);
  return v.f;
}


void lua_Parser::gotostat (int pc) {
  int line = ls->linenumber();
  TString *label;
  int g;
  if (testnext(TK_GOTO))
    label = str_checkname();
  else {
    ls->next_token();  /* skip break */
    label = luaS_new(ls->L(), "break");
  }
  g = newlabelentry(ls, &dyd->gt, label, line, pc);
  findlabel(g);  /* close it if label already defined */
}


/* check for repeated labels on the same block */
static void checkrepeated (FuncState *fs, Labellist *ll, TString *label) {
  int i;
  for (i = fs->bl->firstlabel; i < ll->n; i++) {
    if (luaS_eqstr(label, ll->arr[i].name)) {
      const char *msg = luaO_pushfstring(fs->ls->L(),
                          "label " LUA_QS " already defined on line %d",
                          getstr(label), ll->arr[i].line);
      semerror(fs->ls, msg);
    }
  }
}


/* skip no-op statements */
void lua_Parser::skipnoopstat (void) {
  while (ls->t().token == ';' || ls->t().token == TK_DBCOLON)
    statement();
}


void lua_Parser::labelstat (TString *label, int line) {
  /* label -> '::' NAME '::' */
  FuncState *fs = ls->fs;
  Labellist *ll = &dyd->label;
  int l;  /* index of new label being created */
  checkrepeated(fs, ll, label);  /* check for repeated labels */
  checknext(TK_DBCOLON);  /* skip double colon */
  /* create new entry for this label */
  l = newlabelentry(ls, ll, label, line, fs->pc);
  skipnoopstat();  /* skip other no-op statements */
  if (block_follow(ls, 0)) {  /* label is last no-op statement in the block? */
    /* assume that locals are already out of scope */
    ll->arr[l].nactvar = fs->bl->nactvar;
  }
  findgotos(&ll->arr[l]);
}


void lua_Parser::whilestat (int line) {
  /* whilestat -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  BlockCnt bl;
  ls->next_token();  /* skip WHILE */
  whileinit = luaK_getlabel(fs);
  condexit = cond();
  enterblock(fs, &bl, 1);
  checknext(TK_DO);
  block();
  luaK_jumpto(fs, whileinit);
  check_match(TK_END, TK_WHILE, line);
  leaveblock(fs);
  luaK_patchtohere(fs, condexit);  /* false conditions finish the loop */
}


void lua_Parser::repeatstat (int line) {
  /* repeatstat -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = luaK_getlabel(fs);
  BlockCnt bl1, bl2;
  enterblock(fs, &bl1, 1);  /* loop block */
  enterblock(fs, &bl2, 0);  /* scope block */
  ls->next_token();  /* skip REPEAT */
  statlist();
  check_match(TK_UNTIL, TK_REPEAT, line);
  condexit = cond();  /* read condition (inside scope block) */
  if (bl2.upval)  /* upvalues? */
    luaK_patchclose(fs, condexit, bl2.nactvar);
  leaveblock(fs);  /* finish scope */
  luaK_patchlist(fs, condexit, repeat_init);  /* close the loop */
  leaveblock(fs);  /* finish loop */
}


int lua_Parser::exp1 (void) {
  expdesc e;
  int reg;
  expr(&e);
  luaK_exp2nextreg(ls->fs, &e);
  lua_assert(e.k == VNONRELOC);
  reg = e.u.info;
  return reg;
}


void lua_Parser::forbody (int base, int line, int nvars, int isnum) {
  /* forbody -> DO block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  adjustlocalvars(3);  /* control variables */
  checknext(TK_DO);
  prep = isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : luaK_jump(fs);
  enterblock(fs, &bl, 0);  /* scope for declared variables */
  adjustlocalvars(nvars);
  luaK_reserveregs(fs, nvars);
  block();
  leaveblock(fs);  /* end of scope for declared variables */
  luaK_patchtohere(fs, prep);
  if (isnum)  /* numeric for? */
    endfor = luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP);
  else {  /* generic for */
    luaK_codeABC(fs, OP_TFORCALL, base, 0, nvars);
    luaK_fixline(fs, line);
    endfor = luaK_codeAsBx(fs, OP_TFORLOOP, base + 2, NO_JUMP);
  }
  luaK_patchlist(fs, endfor, prep + 1);
  luaK_fixline(fs, line);
}


void lua_Parser::fornum (TString *varname, int line) {
  /* fornum -> NAME = exp1,exp1[,exp1] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  new_localvarliteral("(for index)");
  new_localvarliteral("(for limit)");
  new_localvarliteral("(for step)");
  new_localvar(varname);
  checknext('=');
  exp1();  /* initial value */
  checknext(',');
  exp1();  /* limit */
  if (testnext(','))
    exp1();  /* optional step */
  else {  /* default step = 1 */
    luaK_codek(fs, fs->freereg, luaK_numberK(fs, 1));
    luaK_reserveregs(fs, 1);
  }
  forbody(base, line, 1, 1);
}


void lua_Parser::forlist (TString *indexname) {
  /* forlist -> NAME {,NAME} IN explist forbody */
  FuncState *fs = ls->fs;
  expdesc e;
  int nvars = 4;  /* gen, state, control, plus at least one declared var */
  int line;
  int base = fs->freereg;
  /* create control variables */
  new_localvarliteral("(for generator)");
  new_localvarliteral("(for state)");
  new_localvarliteral("(for control)");
  /* create declared variables */
  new_localvar(indexname);
  while (testnext(',')) {
    new_localvar(str_checkname());
    nvars++;
  }
  checknext(TK_IN);
  line = ls->linenumber();
  adjust_assign(ls, 3, explist(&e), &e);
  luaK_checkstack(fs, 3);  /* extra space to call generator */
  forbody(base, line, nvars - 3, 0);
}


void lua_Parser::forstat (int line) {
  /* forstat -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  TString *varname;
  BlockCnt bl;
  enterblock(fs, &bl, 1);  /* scope for loop and control variables */
  ls->next_token();  /* skip `for' */
  varname = str_checkname();  /* first variable name */
  switch (ls->t().token) {
    case '=': fornum(varname, line); break;
    case ',': case TK_IN: forlist(varname); break;
    default: ls->syntax_error(LUA_QL("=") " or " LUA_QL("in") " expected");
  }
  check_match(TK_END, TK_FOR, line);
  leaveblock(fs);  /* loop scope (`break' jumps to this point) */
}


void lua_Parser::test_then_block (int *escapelist) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  expdesc v;
  int jf;  /* instruction to skip 'then' code (if condition is false) */
  ls->next_token();  /* skip IF or ELSEIF */
  expr(&v);  /* read condition */
  checknext(TK_THEN);
  if (ls->t().token == TK_GOTO || ls->t().token == TK_BREAK) {
    luaK_goiffalse(ls->fs, &v);  /* will jump to label if condition is true */
    enterblock(fs, &bl, 0);  /* must enter block before 'goto' */
    gotostat(v.t);  /* handle goto/break */
    skipnoopstat();  /* skip other no-op statements */
    if (block_follow(ls, 0)) {  /* 'goto' is the entire block? */
      leaveblock(fs);
      return;  /* and that is it */
    }
    else  /* must skip over 'then' part if condition is false */
      jf = luaK_jump(fs);
  }
  else {  /* regular case (not goto/break) */
    luaK_goiftrue(ls->fs, &v);  /* skip over block if condition is false */
    enterblock(fs, &bl, 0);
    jf = v.f;
  }
  statlist();  /* `then' part */
  leaveblock(fs);
  if (ls->t().token == TK_ELSE ||
      ls->t().token == TK_ELSEIF)  /* followed by 'else'/'elseif'? */
    luaK_concat(fs, escapelist, luaK_jump(fs));  /* must jump over it */
  luaK_patchtohere(fs, jf);
}


void lua_Parser::ifstat (int line) {
  /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
  FuncState *fs = ls->fs;
  int escapelist = NO_JUMP;  /* exit list for finished parts */
  test_then_block(&escapelist);  /* IF cond THEN block */
  while (ls->t().token == TK_ELSEIF)
    test_then_block(&escapelist);  /* ELSEIF cond THEN block */
  if (testnext(TK_ELSE))
    block();  /* `else' part */
  check_match(TK_END, TK_IF, line);
  luaK_patchtohere(fs, escapelist);  /* patch escape list to 'if' end */
}


void lua_Parser::localfunc (void) {
  expdesc b;
  FuncState *fs = ls->fs;
  new_localvar(str_checkname());  /* new local variable */
  adjustlocalvars(1);  /* enter its scope */
  body(&b, 0, ls->linenumber());  /* function created in next register */
  /* debug information will only see the variable after this point! */
  getlocvar(fs, b.u.info)->startpc = fs->pc;
}


void lua_Parser::localstat (void) {
  /* stat -> LOCAL NAME {`,' NAME} [`=' explist] */
  int nvars = 0;
  int nexps;
  expdesc e;
  do {
    new_localvar(str_checkname());
    nvars++;
  } while (testnext(','));
  if (testnext('='))
    nexps = explist(&e);
  else {
    e.k = VVOID;
    nexps = 0;
  }
  adjust_assign(ls, nvars, nexps, &e);
  adjustlocalvars(nvars);
}


int lua_Parser::funcname (expdesc *v) {
  /* funcname -> NAME {fieldsel} [`:' NAME] */
  int ismethod = 0;
  singlevar(v);
  while (ls->t().token == '.')
    fieldsel(v);
  if (ls->t().token == ':') {
    ismethod = 1;
    fieldsel(v);
  }
  return ismethod;
}


void lua_Parser::funcstat (int line) {
  /* funcstat -> FUNCTION funcname body */
  int ismethod;
  expdesc v, b;
  ls->next_token();  /* skip FUNCTION */
  ismethod = funcname(&v);
  body(&b, ismethod, line);
  luaK_storevar(ls->fs, &v, &b);
  luaK_fixline(ls->fs, line);  /* definition `happens' in the first line */
}


void lua_Parser::exprstat (void) {
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  suffixedexp(&v.v);
  if (ls->t().token == '=' || ls->t().token == ',') { /* stat -> assignment ? */
    v.prev = NULL;
    assignment(&v, 1);
  }
  else {  /* stat -> func */
    ls->check_condition(v.v.k == VCALL, "syntax error");
    SETARG_C(getcode(fs, &v.v), 1);  /* call statement uses no results */
  }
}


void lua_Parser::retstat (void) {
  /* stat -> RETURN [explist] [';'] */
  FuncState *fs = ls->fs;
  expdesc e;
  int first, nret;  /* registers with returned values */
  if (block_follow(ls, 1) || ls->t().token == ';')
    first = nret = 0;  /* return no values */
  else {
    nret = explist(&e);  /* optional return values */
    if (hasmultret(e.k)) {
      luaK_setmultret(fs, &e);
      if (e.k == VCALL && nret == 1) {  /* tail call? */
        SET_OPCODE(getcode(fs,&e), OP_TAILCALL);
        lua_assert(GETARG_A(getcode(fs,&e)) == fs->nactvar);
      }
      first = fs->nactvar;
      nret = LUA_MULTRET;  /* return all values */
    }
    else {
      if (nret == 1)  /* only one single value? */
        first = luaK_exp2anyreg(fs, &e);
      else {
        luaK_exp2nextreg(fs, &e);  /* values must go to the `stack' */
        first = fs->nactvar;  /* return all `active' values */
        lua_assert(nret == fs->freereg - first);
      }
    }
  }
  luaK_ret(fs, first, nret);
  testnext(';');  /* skip optional semicolon */
}


void lua_Parser::statement (void) {
  int line = ls->linenumber();  /* may be needed for error messages */
  enterlevel(ls);
  switch (ls->t().token) {
    case ';': {  /* stat -> ';' (empty statement) */
      ls->next_token();  /* skip ';' */
      break;
    }
    case TK_IF: {  /* stat -> ifstat */
      ifstat(line);
      break;
    }
    case TK_WHILE: {  /* stat -> whilestat */
      whilestat(line);
      break;
    }
    case TK_DO: {  /* stat -> DO block END */
      ls->next_token();  /* skip DO */
      block();
      check_match(TK_END, TK_DO, line);
      break;
    }
    case TK_FOR: {  /* stat -> forstat */
      forstat(line);
      break;
    }
    case TK_REPEAT: {  /* stat -> repeatstat */
      repeatstat(line);
      break;
    }
    case TK_FUNCTION: {  /* stat -> funcstat */
      funcstat(line);
      break;
    }
    case TK_LOCAL: {  /* stat -> localstat */
      ls->next_token();  /* skip LOCAL */
      if (testnext(TK_FUNCTION))  /* local function? */
        localfunc();
      else
        localstat();
      break;
    }
    case TK_DBCOLON: {  /* stat -> label */
      ls->next_token();  /* skip double colon */
      labelstat(str_checkname(), line);
      break;
    }
    case TK_RETURN: {  /* stat -> retstat */
      ls->next_token();  /* skip RETURN */
      retstat();
      break;
    }
    case TK_BREAK:   /* stat -> breakstat */
    case TK_GOTO: {  /* stat -> 'goto' NAME */
      gotostat(luaK_jump(ls->fs));
      break;
    }
    default: {  /* stat -> func | assignment */
      exprstat();
      break;
    }
  }
  lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
             ls->fs->freereg >= ls->fs->nactvar);
  ls->fs->freereg = ls->fs->nactvar;  /* free registers */
  ls->leave_level();
}

/* }====================================================================== */


/*
** compiles the main function, which is a regular vararg function with an
** upvalue named LUA_ENV
*/
void lua_Parser::mainfunc (FuncState *fs) {
  BlockCnt bl;
  expdesc v;
  open_func(fs, &bl);
  fs->f->is_vararg = 1;  /* main function is always vararg */
  init_exp(&v, VLOCAL, 0);  /* create and... */
  newupvalue(fs, envn, &v);  /* ...set environment upvalue */
  ls->next_token();  /* read first token */
  statlist();  /* parse main body */
  check(TK_EOS);
  close_func();
}


Closure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
                      Dyndata *dyd, const char *name, int firstchar) {
  LexState lexstate;
  FuncState funcstate;
  LClosure *cl = luaF_newLclosure(L, 1);  /* create main closure */
  /* anchor closure (to avoid being collected) */
  L->top->set_value(L, cl);
  incr_top(L);
  funcstate.f = cl->p = luaF_newproto(L);
  funcstate.f->source = luaS_new(L, name);  /* create and anchor TString */
  lexstate.set_data(buff);
  dyd->actvar.n = dyd->gt.n = dyd->label.n = 0;
  lexstate.set_input(L, z, funcstate.f->source, firstchar);
  lua_Parser parser(&lexstate, dyd);
  parser.mainfunc(&funcstate);
  lua_assert(!funcstate.prev && funcstate.nups == 1 && !lexstate.fs);
  /* all scopes should be correctly finished */
  lua_assert(dyd->actvar.n == 0 && dyd->gt.n == 0 && dyd->label.n == 0);
  return cl;  /* it's on the stack too */
}

