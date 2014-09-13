/*
** $Id: lapi.c,v 2.171.1.1 2013/04/12 18:48:47 roberto Exp $
** Lua API
** See Copyright Notice in lua.h
*/


#include <stdarg.h>
#include <string.h>

#define lapi_c
#define LUA_CORE

#include "lua.h"

#include "lapi.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lundump.h"
#include "lvm.h"



const char lua_ident[] =
  "$LuaVersion: " LUA_COPYRIGHT " $"
  "$LuaAuthors: " LUA_AUTHORS " $";


/* value at a non-valid index */
#define NONVALIDVALUE		cast(TValue *, luaO_nilobject)

/* corresponding test */
#define isvalid(o)	((o) != luaO_nilobject)

/* test for pseudo index */
#define ispseudo(i)		((i) <= LUA_REGISTRYINDEX)

/* test for valid but not pseudo index */
#define isstackindex(i, o)	(isvalid(o) && !ispseudo(i))

#define api_checkvalidindex(L, o)  api_check(L, isvalid(o), "invalid index")

#define api_checkstackindex(L, i, o)  \
	api_check(L, isstackindex(i, o), "index not in the stack")


static TValue *index2addr (lua_State *L, int idx) {
  CallInfo *ci = L->ci;
  if (idx > 0) {
    TValue *o = ci->func + idx;
    api_check(L, idx <= ci->top - (ci->func + 1), "unacceptable index");
    if (o >= L->top) return NONVALIDVALUE;
    else return o;
  }
  else if (!ispseudo(idx)) {  /* negative index */
    api_check(L, idx != 0 && -idx <= L->top - (ci->func + 1), "invalid index");
    return L->top + idx;
  }
  else if (idx == LUA_REGISTRYINDEX)
    return &G(L)->l_registry;
  else {  /* upvalues */
    idx = LUA_REGISTRYINDEX - idx;
    api_check(L, idx <= MAXUPVAL + 1, "upvalue index too large");
    if (ci->func->is_lcf())  /* light C function? */
      return NONVALIDVALUE;  /* it has no upvalues */
    else {
      CClosure *func = (ci->func)->to_c_closure();
      return (idx <= func->nupvalues) ? &func->upvalue[idx-1] : NONVALIDVALUE;
    }
  }
}


/*
** to be called by 'lua_checkstack' in protected mode, to grow stack
** capturing memory errors
*/
class GrowStack:public Pfunc {
 private:
  int& m_size;
 public:
  GrowStack(int& size):m_size(size) {}
  void func	(lua_State *L) {
    luaD_growstack(L, m_size);
  }
};


LUA_API int lua_checkstack (lua_State *L, int size) {
  int res;
  CallInfo *ci = L->ci;
  lua_lock(L);
  if (L->stack_last - L->top > size)  /* stack large enough? */
    res = 1;  /* yes; check is OK */
  else {  /* no; need to grow stack */
    int inuse = cast_int(L->top - L->stack) + EXTRA_STACK;
    if (inuse > LUAI_MAXSTACK - size)  /* can grow without overflow? */
      res = 0;  /* no */
    else { /* try to grow stack */
      GrowStack growstack(size);
      res = (luaD_rawrunprotected(L, &growstack) == LUA_OK);
    }
  }
  if (res && ci->top < L->top + size)
    ci->top = L->top + size;  /* adjust frame top */
  lua_unlock(L);
  return res;
}


LUA_API void lua_xmove (lua_State *from, lua_State *to, int n) {
  int i;
  if (from == to) return;
  lua_lock(to);
  api_checknelems(from, n);
  api_check(from, G(from) == G(to), "moving among independent states");
  api_check(from, to->ci->top - to->top >= n, "not enough elements to move");
  from->top -= n;
  for (i = 0; i < n; i++) {
    (to->top++)->set_obj(to, from->top + i);
  }
  lua_unlock(to);
}


LUA_API lua_CFunction lua_atpanic (lua_State *L, lua_CFunction panicf) {
  lua_CFunction old;
  lua_lock(L);
  old = G(L)->panic;
  G(L)->panic = panicf;
  lua_unlock(L);
  return old;
}


LUA_API const lua_Number *lua_version (lua_State *L) {
  static const lua_Number version = LUA_VERSION_NUM;
  if (L == NULL) return &version;
  else return G(L)->version;
}



/*
** basic stack manipulation
*/


/*
** convert an acceptable stack index into an absolute index
*/
LUA_API int lua_absindex (lua_State *L, int idx) {
  return (idx > 0 || ispseudo(idx))
         ? idx
         : cast_int(L->top - L->ci->func + idx);
}


LUA_API int lua_gettop (lua_State *L) {
  return cast_int(L->top - (L->ci->func + 1));
}


LUA_API void lua_settop (lua_State *L, int idx) {
  StkId func = L->ci->func;
  lua_lock(L);
  if (idx >= 0) {
    api_check(L, idx <= L->stack_last - (func + 1), "new top too large");
    while (L->top < (func + 1) + idx)
      (L->top++)->set_nil_value();
    L->top = (func + 1) + idx;
  }
  else {
    api_check(L, -(idx+1) <= (L->top - (func + 1)), "invalid new top");
    L->top += idx+1;  /* `subtract' index (index is negative) */
  }
  lua_unlock(L);
}


LUA_API void lua_remove (lua_State *L, int idx) {
  StkId p;
  lua_lock(L);
  p = index2addr(L, idx);
  api_checkstackindex(L, idx, p);
  while (++p < L->top) (p-1)->set_obj(L, p);
  L->top--;
  lua_unlock(L);
}


LUA_API void lua_insert (lua_State *L, int idx) {
  StkId p;
  StkId q;
  lua_lock(L);
  p = index2addr(L, idx);
  api_checkstackindex(L, idx, p);
  for (q = L->top; q > p; q--)  /* use L->top as a temporary */
    q->set_obj(L, q - 1);
  p->set_obj(L, L->top);
  lua_unlock(L);
}


static void moveto (lua_State *L, TValue *fr, int idx) {
  TValue *to = index2addr(L, idx);
  api_checkvalidindex(L, to);
  to->set_obj(L, fr);
  if (idx < LUA_REGISTRYINDEX)  /* function upvalue? */
    luaC_barrier(L, (L->ci->func)->to_c_closure(), fr);
  /* LUA_REGISTRYINDEX does not need gc barrier
     (collector revisits it before finishing collection) */
}


LUA_API void lua_replace (lua_State *L, int idx) {
  lua_lock(L);
  api_checknelems(L, 1);
  moveto(L, L->top - 1, idx);
  L->top--;
  lua_unlock(L);
}


LUA_API void lua_copy (lua_State *L, int fromidx, int toidx) {
  TValue *fr;
  lua_lock(L);
  fr = index2addr(L, fromidx);
  moveto(L, fr, toidx);
  lua_unlock(L);
}


LUA_API void lua_pushvalue (lua_State *L, int idx) {
  lua_lock(L);
  L->top->set_obj(L, index2addr(L, idx));
  api_incr_top(L);
  lua_unlock(L);
}



/*
** access functions (stack -> C)
*/


LUA_API int lua_type (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  return (isvalid(o) ? ttypenv(o) : LUA_TNONE);
}


LUA_API const char *lua_typename (lua_State *L, int t) {
  UNUSED(L);
  return ttypename(t);
}


LUA_API int lua_iscfunction (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  return (o->is_lcf() || (o->is_c_closure()));
}


LUA_API int lua_isnumber (lua_State *L, int idx) {
  TValue n;
  const TValue *o = index2addr(L, idx);
  return tonumber(o, &n);
}


LUA_API int lua_isstring (lua_State *L, int idx) {
  int t = lua_type(L, idx);
  return (t == LUA_TSTRING || t == LUA_TNUMBER);
}


LUA_API int lua_isuserdata (lua_State *L, int idx) {
  const TValue *o = index2addr(L, idx);
  return (((TValue*)o)->is_userdata() || ((TValue*)o)->is_light_userdata());
}


LUA_API int lua_rawequal (lua_State *L, int index1, int index2) {
  StkId o1 = index2addr(L, index1);
  StkId o2 = index2addr(L, index2);
  return (isvalid(o1) && isvalid(o2)) ? luaV_rawequalobj(o1, o2) : 0;
}


LUA_API void lua_arith (lua_State *L, int op) {
  StkId o1;  /* 1st operand */
  StkId o2;  /* 2nd operand */
  lua_lock(L);
  if (op != LUA_OPUNM) /* all other operations expect two operands */
    api_checknelems(L, 2);
  else {  /* for unary minus, add fake 2nd operand */
    api_checknelems(L, 1);
    L->top->set_obj(L, L->top - 1);
    L->top++;
  }
  o1 = L->top - 2;
  o2 = L->top - 1;
  if (o1->is_number() && o2->is_number()) {
    o1->set_value(luaO_arith(op, o1->to_number(), o2->to_number()));
  }
  else
    luaV_arith(L, o1, o1, o2, cast(TMS, op - LUA_OPADD + TM_ADD));
  L->top--;
  lua_unlock(L);
}


LUA_API int lua_compare (lua_State *L, int index1, int index2, int op) {
  StkId o1, o2;
  int i = 0;
  lua_lock(L);  /* may call tag method */
  o1 = index2addr(L, index1);
  o2 = index2addr(L, index2);
  if (isvalid(o1) && isvalid(o2)) {
    switch (op) {
      case LUA_OPEQ: i = equalobj(L, o1, o2); break;
      case LUA_OPLT: i = luaV_lessthan(L, o1, o2); break;
      case LUA_OPLE: i = luaV_lessequal(L, o1, o2); break;
      default: api_check(L, 0, "invalid option");
    }
  }
  lua_unlock(L);
  return i;
}


LUA_API lua_Number lua_tonumberx (lua_State *L, int idx, int *isnum) {
  TValue n;
  const TValue *o = index2addr(L, idx);
  if (tonumber(o, &n)) {
    if (isnum) *isnum = 1;
    return ((TValue*)o)->to_number();
  }
  else {
    if (isnum) *isnum = 0;
    return 0;
  }
}


LUA_API lua_Integer lua_tointegerx (lua_State *L, int idx, int *isnum) {
  TValue n;
  const TValue *o = index2addr(L, idx);
  if (tonumber(o, &n)) {
    lua_Integer res;
    lua_Number num = ((TValue*)o)->to_number();
    lua_number2integer(res, num);
    if (isnum) *isnum = 1;
    return res;
  }
  else {
    if (isnum) *isnum = 0;
    return 0;
  }
}


LUA_API lua_Unsigned lua_tounsignedx (lua_State *L, int idx, int *isnum) {
  TValue n;
  const TValue *o = index2addr(L, idx);
  if (tonumber(o, &n)) {
    lua_Unsigned res;
    lua_Number num = ((TValue*)o)->to_number();
    lua_number2unsigned(res, num);
    if (isnum) *isnum = 1;
    return res;
  }
  else {
    if (isnum) *isnum = 0;
    return 0;
  }
}


LUA_API int lua_toboolean (lua_State *L, int idx) {
  const TValue *o = index2addr(L, idx);
  return !l_isfalse(o);
}


LUA_API const char *lua_tolstring (lua_State *L, int idx, size_t *len) {
  StkId o = index2addr(L, idx);
  if (!o->is_string()) {
    lua_lock(L);  /* `luaV_tostring' may create a new string */
    if (!luaV_tostring(L, o)) {  /* conversion failed? */
      if (len != NULL) *len = 0;
      lua_unlock(L);
      return NULL;
    }
    luaC_checkGC(L);
    o = index2addr(L, idx);  /* previous call may reallocate the stack */
    lua_unlock(L);
  }
  if (len != NULL) *len = o->to_string()->len;
  return svalue(o);
}


LUA_API size_t lua_rawlen (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  switch (ttypenv(o)) {
    case LUA_TSTRING: return o->to_string()->len;
    case LUA_TUSERDATA: return o->to_userdata()->len;
    case LUA_TTABLE: return luaH_getn(o->to_table());
    default: return 0;
  }
}


LUA_API lua_CFunction lua_tocfunction (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  if (o->is_lcf()) return o->to_lcf();
  else if (o->is_c_closure())
    return (o)->to_c_closure()->f;
  else return NULL;  /* not a C function */
}


LUA_API void *lua_touserdata (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  switch (ttypenv(o)) {
    case LUA_TUSERDATA: return (o->to_userdata() + 1);
    case LUA_TLIGHTUSERDATA: return o->to_light_userdata();
    default: return NULL;
  }
}


LUA_API lua_State *lua_tothread (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  return (!o->is_thread()) ? NULL : o->to_thread();
}


LUA_API const void *lua_topointer (lua_State *L, int idx) {
  StkId o = index2addr(L, idx);
  switch (o->type()) {
    case LUA_TTABLE: return o->to_table();
    case LUA_TLCL: return o->to_l_closure();
    case LUA_TCCL: return o->to_c_closure();
    case LUA_TLCF: return cast(void *, cast(size_t, o->to_lcf()));
    case LUA_TTHREAD: return o->to_thread();
    case LUA_TUSERDATA:
    case LUA_TLIGHTUSERDATA:
      return lua_touserdata(L, idx);
    default: return NULL;
  }
}



/*
** push functions (C -> stack)
*/


LUA_API void lua_pushnil (lua_State *L) {
  lua_lock(L);
  (L->top)->set_nil_value();
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_pushnumber (lua_State *L, lua_Number n) {
  lua_lock(L);
  L->top->set_value(n);
  luai_checknum(L, L->top,
    luaG_runerror(L, "C API - attempt to push a signaling NaN"));
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_pushinteger (lua_State *L, lua_Integer n) {
  lua_lock(L);
  L->top->set_value(cast_num(n));
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_pushunsigned (lua_State *L, lua_Unsigned u) {
  lua_Number n;
  lua_lock(L);
  n = lua_unsigned2number(u);
  L->top->set_value(n);
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API const char *lua_pushlstring (lua_State *L, const char *s, size_t len) {
  TString *ts;
  lua_lock(L);
  luaC_checkGC(L);
  ts = luaS_newlstr(L, s, len);
  L->top->set_value(L, ts);
  api_incr_top(L);
  lua_unlock(L);
  return getstr(ts);
}


LUA_API const char *lua_pushstring (lua_State *L, const char *s) {
  if (s == NULL) {
    lua_pushnil(L);
    return NULL;
  }
  else {
    TString *ts;
    lua_lock(L);
    luaC_checkGC(L);
    ts = luaS_new(L, s);
    L->top->set_value(L, ts);
    api_incr_top(L);
    lua_unlock(L);
    return getstr(ts);
  }
}


LUA_API const char *lua_pushvfstring (lua_State *L, const char *fmt,
                                      va_list argp) {
  const char *ret;
  lua_lock(L);
  luaC_checkGC(L);
  ret = luaO_pushvfstring(L, fmt, argp);
  lua_unlock(L);
  return ret;
}


LUA_API const char *lua_pushfstring (lua_State *L, const char *fmt, ...) {
  const char *ret;
  va_list argp;
  lua_lock(L);
  luaC_checkGC(L);
  va_start(argp, fmt);
  ret = luaO_pushvfstring(L, fmt, argp);
  va_end(argp);
  lua_unlock(L);
  return ret;
}


LUA_API void lua_pushcclosure (lua_State *L, lua_CFunction fn, int n) {
  lua_lock(L);
  if (n == 0) {
    L->top->set_value(fn);
  }
  else {
    CClosure *cl;
    api_checknelems(L, n);
    api_check(L, n <= MAXUPVAL, "upvalue index too large");
    luaC_checkGC(L);
    cl = luaF_newCclosure(L, n);
    cl->f = fn;
    L->top -= n;
    while (n--)
      cl->upvalue[n].set_obj(L, L->top + n);
    L->top->set_value(L, cl);
  }
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_pushboolean (lua_State *L, int b) {
  lua_lock(L);
  L->top->set_value(b != 0);  /* ensure that true is 1 */
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_pushlightuserdata (lua_State *L, void *p) {
  lua_lock(L);
  L->top->set_value(p);
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API int lua_pushthread (lua_State *L) {
  lua_lock(L);
  L->top->set_value(L, L);
  api_incr_top(L);
  lua_unlock(L);
  return (G(L)->mainthread == L);
}



/*
** get functions (Lua -> stack)
*/


LUA_API void lua_getglobal (lua_State *L, const char *var) {
  Table *reg = (&G(L)->l_registry)->to_table();
  const TValue *gt;  /* global table */
  lua_lock(L);
  gt = luaH_getint(reg, LUA_RIDX_GLOBALS);
  (L->top++)->set_value(L, luaS_new(L, var));
  luaV_gettable(L, gt, L->top - 1, L->top - 1);
  lua_unlock(L);
}


LUA_API void lua_gettable (lua_State *L, int idx) {
  StkId t;
  lua_lock(L);
  t = index2addr(L, idx);
  luaV_gettable(L, t, L->top - 1, L->top - 1);
  lua_unlock(L);
}


LUA_API void lua_getfield (lua_State *L, int idx, const char *k) {
  StkId t;
  lua_lock(L);
  t = index2addr(L, idx);
  L->top->set_value(L, luaS_new(L, k));
  api_incr_top(L);
  luaV_gettable(L, t, L->top - 1, L->top - 1);
  lua_unlock(L);
}


LUA_API void lua_rawget (lua_State *L, int idx) {
  StkId t;
  lua_lock(L);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  (L->top - 1)->set_obj(L, luaH_get(t->to_table(), L->top - 1));
  lua_unlock(L);
}


LUA_API void lua_rawgeti (lua_State *L, int idx, int n) {
  StkId t;
  lua_lock(L);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  L->top->set_obj(L, luaH_getint(t->to_table(), n));
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_rawgetp (lua_State *L, int idx, const void *p) {
  StkId t;
  TValue k;
  lua_lock(L);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  k.set_value(cast(void *, p));
  L->top->set_obj(L, luaH_get(t->to_table(), &k));
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API void lua_createtable (lua_State *L, int narray, int nrec) {
  Table *t;
  lua_lock(L);
  luaC_checkGC(L);
  t = luaH_new(L);
  L->top->set_value(L, t);
  api_incr_top(L);
  if (narray > 0 || nrec > 0)
    luaH_resize(L, t, narray, nrec);
  lua_unlock(L);
}


LUA_API int lua_getmetatable (lua_State *L, int objindex) {
  const TValue *obj;
  Table *mt = NULL;
  int res;
  lua_lock(L);
  obj = index2addr(L, objindex);
  switch (ttypenv(obj)) {
    case LUA_TTABLE:
      mt = ((TValue*)obj)->to_table()->metatable;
      break;
    case LUA_TUSERDATA:
      mt = ((TValue*)obj)->to_userdata()->metatable;
      break;
    default:
      mt = G(L)->mt[ttypenv(obj)];
      break;
  }
  if (mt == NULL)
    res = 0;
  else {
    L->top->set_value(L, mt);
    api_incr_top(L);
    res = 1;
  }
  lua_unlock(L);
  return res;
}


LUA_API void lua_getuservalue (lua_State *L, int idx) {
  StkId o;
  lua_lock(L);
  o = index2addr(L, idx);
  api_check(L, ttisuserdata(o), "userdata expected");
  if (o->to_userdata()->env) {
    L->top->set_value(L, o->to_userdata()->env);
  } else
    (L->top)->set_nil_value();
  api_incr_top(L);
  lua_unlock(L);
}


/*
** set functions (stack -> Lua)
*/


LUA_API void lua_setglobal (lua_State *L, const char *var) {
  Table *reg = (&G(L)->l_registry)->to_table();
  const TValue *gt;  /* global table */
  lua_lock(L);
  api_checknelems(L, 1);
  gt = luaH_getint(reg, LUA_RIDX_GLOBALS);
  (L->top++)->set_value(L, luaS_new(L, var));
  luaV_settable(L, gt, L->top - 1, L->top - 2);
  L->top -= 2;  /* pop value and key */
  lua_unlock(L);
}


LUA_API void lua_settable (lua_State *L, int idx) {
  StkId t;
  lua_lock(L);
  api_checknelems(L, 2);
  t = index2addr(L, idx);
  luaV_settable(L, t, L->top - 2, L->top - 1);
  L->top -= 2;  /* pop index and value */
  lua_unlock(L);
}


LUA_API void lua_setfield (lua_State *L, int idx, const char *k) {
  StkId t;
  lua_lock(L);
  api_checknelems(L, 1);
  t = index2addr(L, idx);
  (L->top++)->set_value(L, luaS_new(L, k));
  luaV_settable(L, t, L->top - 1, L->top - 2);
  L->top -= 2;  /* pop value and key */
  lua_unlock(L);
}


LUA_API void lua_rawset (lua_State *L, int idx) {
  StkId t;
  lua_lock(L);
  api_checknelems(L, 2);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  (luaH_set(L, t->to_table(), L->top-2))->set_obj(L, L->top-1);
  invalidateTMcache(t->to_table());
  luaC_barrierback(L, t->to_gc(), L->top-1);
  L->top -= 2;
  lua_unlock(L);
}


LUA_API void lua_rawseti (lua_State *L, int idx, int n) {
  StkId t;
  lua_lock(L);
  api_checknelems(L, 1);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  luaH_setint(L, t->to_table(), n, L->top - 1);
  luaC_barrierback(L, t->to_gc(), L->top-1);
  L->top--;
  lua_unlock(L);
}


LUA_API void lua_rawsetp (lua_State *L, int idx, const void *p) {
  StkId t;
  TValue k;
  lua_lock(L);
  api_checknelems(L, 1);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  k.set_value(cast(void *, p));
  (luaH_set(L, t->to_table(), &k))->set_obj(L, L->top - 1);
  luaC_barrierback(L, t->to_gc(), L->top - 1);
  L->top--;
  lua_unlock(L);
}


LUA_API int lua_setmetatable (lua_State *L, int objindex) {
  TValue *obj;
  Table *mt;
  lua_lock(L);
  api_checknelems(L, 1);
  obj = index2addr(L, objindex);
  if ((L->top - 1)->is_nil())
    mt = NULL;
  else {
    api_check(L, ttistable(L->top - 1), "table expected");
    mt = (L->top - 1)->to_table();
  }
  switch (ttypenv(obj)) {
    case LUA_TTABLE: {
      obj->to_table()->metatable = mt;
      if (mt) {
        luaC_objbarrierback(L, obj->to_gc(), mt);
        luaC_checkfinalizer(L, obj->to_gc(), mt);
      }
      break;
    }
    case LUA_TUSERDATA: {
      obj->to_userdata()->metatable = mt;
      if (mt) {
        luaC_objbarrier(L, obj->to_userdata(), mt);
        luaC_checkfinalizer(L, obj->to_gc(), mt);
      }
      break;
    }
    default: {
      G(L)->mt[ttypenv(obj)] = mt;
      break;
    }
  }
  L->top--;
  lua_unlock(L);
  return 1;
}


LUA_API void lua_setuservalue (lua_State *L, int idx) {
  StkId o;
  lua_lock(L);
  api_checknelems(L, 1);
  o = index2addr(L, idx);
  api_check(L, ttisuserdata(o), "userdata expected");
  if ((L->top - 1)->is_nil())
    o->to_userdata()->env = NULL;
  else {
    api_check(L, ttistable(L->top - 1), "table expected");
    o->to_userdata()->env = (L->top - 1)->to_table();
    luaC_objbarrier(L, o->to_gc(), (L->top - 1)->to_table());
  }
  L->top--;
  lua_unlock(L);
}


/*
** `load' and `call' functions (run Lua code)
*/


#define checkresults(L,na,nr) \
     api_check(L, (nr) == LUA_MULTRET || (L->ci->top - L->top >= (nr) - (na)), \
	"results from function overflow current stack size")


LUA_API int lua_getctx (lua_State *L, int *ctx) {
  if (L->ci->callstatus & CIST_YIELDED) {
    if (ctx) *ctx = L->ci->u.c.ctx;
    return L->ci->u.c.status;
  }
  else return LUA_OK;
}


LUA_API void lua_callk (lua_State *L, int nargs, int nresults, int ctx,
                        lua_CFunction k) {
  StkId func;
  lua_lock(L);
  api_check(L, k == NULL || !isLua(L->ci),
    "cannot use continuations inside hooks");
  api_checknelems(L, nargs+1);
  api_check(L, L->status == LUA_OK, "cannot do calls on non-normal thread");
  checkresults(L, nargs, nresults);
  func = L->top - (nargs+1);
  if (k != NULL && L->nny == 0) {  /* need to prepare continuation? */
    L->ci->u.c.k = k;  /* save continuation */
    L->ci->u.c.ctx = ctx;  /* save context */
    luaD_call(L, func, nresults, 1);  /* do the call */
  }
  else  /* no continuation or no yieldable */
    luaD_call(L, func, nresults, 0);  /* just do the call */
  adjustresults(L, nresults);
  lua_unlock(L);
}



/*
** Execute a protected call.
*/
class CallS:public Pfunc {  /* data to `f_call' */
 public:
  StkId sfunc;
  int nresults;
  void func (lua_State *L) {
    luaD_call(L, sfunc, nresults, 0);
  }
};

LUA_API int lua_pcallk (lua_State *L, int nargs, int nresults, int errfunc,
                        int ctx, lua_CFunction k) {
  CallS c;
  int status;
  ptrdiff_t func;
  lua_lock(L);
  api_check(L, k == NULL || !isLua(L->ci),
    "cannot use continuations inside hooks");
  api_checknelems(L, nargs+1);
  api_check(L, L->status == LUA_OK, "cannot do calls on non-normal thread");
  checkresults(L, nargs, nresults);
  if (errfunc == 0)
    func = 0;
  else {
    StkId o = index2addr(L, errfunc);
    api_checkstackindex(L, errfunc, o);
    func = savestack(L, o);
  }
  c.sfunc = L->top - (nargs+1);  /* function to be called */
  if (k == NULL || L->nny > 0) {  /* no continuation or no yieldable? */
    c.nresults = nresults;  /* do a 'conventional' protected call */
    status = luaD_pcall(L, &c, savestack(L, c.sfunc), func);
  }
  else {  /* prepare continuation (call is already protected by 'resume') */
    CallInfo *ci = L->ci;
    ci->u.c.k = k;  /* save continuation */
    ci->u.c.ctx = ctx;  /* save context */
    /* save information for error recovery */
    ci->extra = savestack(L, c.sfunc);
    ci->u.c.old_allowhook = L->allowhook;
    ci->u.c.old_errfunc = L->errfunc;
    L->errfunc = func;
    /* mark that function may do error recovery */
    ci->callstatus |= CIST_YPCALL;
    luaD_call(L, c.sfunc, nresults, 1);  /* do the call */
    ci->callstatus &= ~CIST_YPCALL;
    L->errfunc = ci->u.c.old_errfunc;
    status = LUA_OK;  /* if it is here, there were no errors */
  }
  adjustresults(L, nresults);
  lua_unlock(L);
  return status;
}


LUA_API int lua_load (lua_State *L, lua_Reader* reader,
                      const char *chunkname, const char *mode) {
  ZIO z(L, reader);
  int status;
  lua_lock(L);
  if (!chunkname) chunkname = "?";
  status = luaD_protectedparser(L, &z, chunkname, mode);
  if (status == LUA_OK) {  /* no errors? */
    LClosure *f = (L->top - 1)->to_l_closure();  /* get newly created function */
    if (f->nupvalues == 1) {  /* does it have one upvalue? */
      /* get global table from registry */
      Table *reg = (&G(L)->l_registry)->to_table();
      const TValue *gt = luaH_getint(reg, LUA_RIDX_GLOBALS);
      /* set global table as 1st upvalue of 'f' (may be LUA_ENV) */
      f->upvals[0]->v->set_obj(L, gt);
      luaC_barrier(L, f->upvals[0], gt);
    }
  }
  lua_unlock(L);
  return status;
}


LUA_API int lua_dump (lua_State *L, lua_Writer writer, void *data) {
  int status;
  TValue *o;
  lua_lock(L);
  api_checknelems(L, 1);
  o = L->top - 1;
  if (o->is_l_closure())
    status = luaU_dump(L, getproto(o), writer, data, 0);
  else
    status = 1;
  lua_unlock(L);
  return status;
}


LUA_API int lua_status (lua_State *L) {
  return L->status;
}


/*
** Garbage-collection function
*/

LUA_API int lua_gc (lua_State *L, int what, int data) {
  int res = 0;
  global_State *g;
  lua_lock(L);
  g = G(L);
  switch (what) {
    case LUA_GCSTOP: {
      g->gcrunning = 0;
      break;
    }
    case LUA_GCRESTART: {
      luaE_setdebt(g, 0);
      g->gcrunning = 1;
      break;
    }
    case LUA_GCCOLLECT: {
      luaC_fullgc(L, 0);
      break;
    }
    case LUA_GCCOUNT: {
      /* GC values are expressed in Kbytes: #bytes/2^10 */
      res = cast_int(gettotalbytes(g) >> 10);
      break;
    }
    case LUA_GCCOUNTB: {
      res = cast_int(gettotalbytes(g) & 0x3ff);
      break;
    }
    case LUA_GCSTEP: {
      if (g->gckind == KGC_GEN) {  /* generational mode? */
        res = (g->GCestimate == 0);  /* true if it will do major collection */
        luaC_forcestep(L);  /* do a single step */
      }
      else {
       lu_mem debt = cast(lu_mem, data) * 1024 - GCSTEPSIZE;
       if (g->gcrunning)
         debt += g->GCdebt;  /* include current debt */
       luaE_setdebt(g, debt);
       luaC_forcestep(L);
       if (g->gcstate == GCSpause)  /* end of cycle? */
         res = 1;  /* signal it */
      }
      break;
    }
    case LUA_GCSETPAUSE: {
      res = g->gcpause;
      g->gcpause = data;
      break;
    }
    case LUA_GCSETMAJORINC: {
      res = g->gcmajorinc;
      g->gcmajorinc = data;
      break;
    }
    case LUA_GCSETSTEPMUL: {
      res = g->gcstepmul;
      g->gcstepmul = data;
      break;
    }
    case LUA_GCISRUNNING: {
      res = g->gcrunning;
      break;
    }
    case LUA_GCGEN: {  /* change collector to generational mode */
      luaC_changemode(L, KGC_GEN);
      break;
    }
    case LUA_GCINC: {  /* change collector to incremental mode */
      luaC_changemode(L, KGC_NORMAL);
      break;
    }
    default: res = -1;  /* invalid option */
  }
  lua_unlock(L);
  return res;
}



/*
** miscellaneous functions
*/


LUA_API int lua_error (lua_State *L) {
  lua_lock(L);
  api_checknelems(L, 1);
  luaG_errormsg(L);
  /* code unreachable; will unlock when control actually leaves the kernel */
  return 0;  /* to avoid warnings */
}


LUA_API int lua_next (lua_State *L, int idx) {
  StkId t;
  int more;
  lua_lock(L);
  t = index2addr(L, idx);
  api_check(L, ttistable(t), "table expected");
  more = luaH_next(L, t->to_table(), L->top - 1);
  if (more) {
    api_incr_top(L);
  }
  else  /* no more elements */
    L->top -= 1;  /* remove key */
  lua_unlock(L);
  return more;
}


LUA_API void lua_concat (lua_State *L, int n) {
  lua_lock(L);
  api_checknelems(L, n);
  if (n >= 2) {
    luaC_checkGC(L);
    luaV_concat(L, n);
  }
  else if (n == 0) {  /* push empty string */
    L->top->set_value(L, luaS_newlstr(L, "", 0));
    api_incr_top(L);
  }
  /* else n == 1; nothing to do */
  lua_unlock(L);
}


LUA_API void lua_len (lua_State *L, int idx) {
  StkId t;
  lua_lock(L);
  t = index2addr(L, idx);
  luaV_objlen(L, L->top, t);
  api_incr_top(L);
  lua_unlock(L);
}


LUA_API lua_Alloc lua_getallocf (lua_State *L, void **ud) {
  lua_Alloc f;
  lua_lock(L);
  if (ud) *ud = G(L)->ud;
  f = G(L)->frealloc;
  lua_unlock(L);
  return f;
}


LUA_API void lua_setallocf (lua_State *L, lua_Alloc f, void *ud) {
  lua_lock(L);
  G(L)->ud = ud;
  G(L)->frealloc = f;
  lua_unlock(L);
}


LUA_API void *lua_newuserdata (lua_State *L, size_t size) {
  Udata *u;
  lua_lock(L);
  luaC_checkGC(L);
  u = luaS_newudata(L, size, NULL);
  L->top->set_value(L, u);
  api_incr_top(L);
  lua_unlock(L);
  return u + 1;
}



static const char *aux_upvalue (StkId fi, int n, TValue **val,
                                GCObject **owner) {
  switch (fi->type()) {
    case LUA_TCCL: {  /* C closure */
      CClosure *f = fi->to_c_closure();
      if (!(1 <= n && n <= f->nupvalues)) return NULL;
      *val = &f->upvalue[n-1];
      if (owner) *owner = obj2gco(f);
      return "";
    }
    case LUA_TLCL: {  /* Lua closure */
      LClosure *f = fi->to_l_closure();
      TString *name;
      Proto *p = f->p;
      if (!(1 <= n && n <= p->sizeupvalues)) return NULL;
      *val = f->upvals[n-1]->v;
      if (owner) *owner = obj2gco(f->upvals[n - 1]);
      name = p->upvalues[n-1].name;
      return (name == NULL) ? "" : getstr(name);
    }
    default: return NULL;  /* not a closure */
  }
}


LUA_API const char *lua_getupvalue (lua_State *L, int funcindex, int n) {
  const char *name;
  TValue *val = NULL;  /* to avoid warnings */
  lua_lock(L);
  name = aux_upvalue(index2addr(L, funcindex), n, &val, NULL);
  if (name) {
    L->top->set_obj(L, val);
    api_incr_top(L);
  }
  lua_unlock(L);
  return name;
}


LUA_API const char *lua_setupvalue (lua_State *L, int funcindex, int n) {
  const char *name;
  TValue *val = NULL;  /* to avoid warnings */
  GCObject *owner = NULL;  /* to avoid warnings */
  StkId fi;
  lua_lock(L);
  fi = index2addr(L, funcindex);
  api_checknelems(L, 1);
  name = aux_upvalue(fi, n, &val, &owner);
  if (name) {
    L->top--;
    val->set_obj(L, L->top);
    luaC_barrier(L, owner, L->top);
  }
  lua_unlock(L);
  return name;
}


static UpVal **getupvalref (lua_State *L, int fidx, int n, LClosure **pf) {
  LClosure *f;
  StkId fi = index2addr(L, fidx);
  api_check(L, ttisLclosure(fi), "Lua function expected");
  f = fi->to_l_closure();
  api_check(L, (1 <= n && n <= f->p->sizeupvalues), "invalid upvalue index");
  if (pf) *pf = f;
  return &f->upvals[n - 1];  /* get its upvalue pointer */
}


LUA_API void *lua_upvalueid (lua_State *L, int fidx, int n) {
  StkId fi = index2addr(L, fidx);
  switch (fi->type()) {
    case LUA_TLCL: {  /* lua closure */
      return *getupvalref(L, fidx, n, NULL);
    }
    case LUA_TCCL: {  /* C closure */
      CClosure *f = fi->to_c_closure();
      api_check(L, 1 <= n && n <= f->nupvalues, "invalid upvalue index");
      return &f->upvalue[n - 1];
    }
    default: {
      api_check(L, 0, "closure expected");
      return NULL;
    }
  }
}


LUA_API void lua_upvaluejoin (lua_State *L, int fidx1, int n1,
                                            int fidx2, int n2) {
  LClosure *f1;
  UpVal **up1 = getupvalref(L, fidx1, n1, &f1);
  UpVal **up2 = getupvalref(L, fidx2, n2, NULL);
  *up1 = *up2;
  luaC_objbarrier(L, f1, *up2);
}

