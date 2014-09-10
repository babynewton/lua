/*
** $Id: lobject.h,v 2.71.1.1 2013/04/12 18:48:47 roberto Exp $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h


#include <stdarg.h>


#include "llimits.h"
#include "lua.h"


/*
** Extra tags for non-values
*/
#define LUA_TPROTO	LUA_NUMTAGS
#define LUA_TUPVAL	(LUA_NUMTAGS+1)
#define LUA_TDEADKEY	(LUA_NUMTAGS+2)

/*
** number of all possible tags (including LUA_TNONE but excluding DEADKEY)
*/
#define LUA_TOTALTAGS	(LUA_TUPVAL+2)


/*
** tags for Tagged Values have the following use of bits:
** bits 0-3: actual tag (a LUA_T* value)
** bits 4-5: variant bits
** bit 6: whether value is collectable
*/

#define VARBITS		(3 << 4)


/*
** LUA_TFUNCTION variants:
** 0 - Lua function
** 1 - light C function
** 2 - regular C function (closure)
*/

/* Variant tags for functions */
#define LUA_TLCL	(LUA_TFUNCTION | (0 << 4))  /* Lua closure */
#define LUA_TLCF	(LUA_TFUNCTION | (1 << 4))  /* light C function */
#define LUA_TCCL	(LUA_TFUNCTION | (2 << 4))  /* C closure */


/* Variant tags for strings */
#define LUA_TSHRSTR	(LUA_TSTRING | (0 << 4))  /* short strings */
#define LUA_TLNGSTR	(LUA_TSTRING | (1 << 4))  /* long strings */


/* Bit mark for collectable types */
#define BIT_ISCOLLECTABLE	(1 << 6)

/* mark a tag as collectable */
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)


/*
** Union of all collectable objects
*/
class GCObject {
 public:
  GCObject *next; lu_byte tt; lu_byte marked;
//  GCheader gch;  /* common header */
//  TString ts;
//  Udata u;
//  Closure cl;
//  Table h;
//  Proto p;
//  UpVal uv;
//  lua_State th;  /* thread */
};



/*
** Common header in struct form
*/
class GCheader : public GCObject {
};



/*
** Union of all Lua values
*/
typedef union Value Value;


#define numfield	lua_Number n;    /* numbers */



/*
** Tagged Values. This is the basic representation of values in Lua,
** an actual value plus a tag with its type.
*/

#define TValuefields	Value value_; int tt_

typedef class lua_TValue TValue;


/* macro defining a nil value */
#define NILCONSTANT	{NULL}, LUA_TNIL


//subject to be removed
#define val_(o)		((o)->value_)
#define num_(o)		(val_(o).n)


/* raw type tag of a TValue */
#define rttype(o)	((o)->tt_)

/* tag with no variants (bits 0-3) */
#define novariant(x)	((x) & 0x0F)

/* type tag of a TValue (bits 0-3 for tags + variant bits 4-5) */
#define ttype(o)	(rttype(o) & 0x3F)

/* type tag of a TValue with no variants (bits 0-3) */
#define ttypenv(o)	(novariant(rttype(o)))


/* Macros to test type */
//subject to be removed
#define checktag(o,t)		(rttype(o) == (t))
#define checktype(o,t)		(ttypenv(o) == (t))
//#define ttisnumber(o)		checktag((o), LUA_TNUMBER)
//#define ttisnil(o)		checktag((o), LUA_TNIL)
//#define ttisboolean(o)		checktag((o), LUA_TBOOLEAN)
//#define ttislightuserdata(o)	checktag((o), LUA_TLIGHTUSERDATA)
//#define ttisstring(o)		checktype((o), LUA_TSTRING)
//#define ttisshrstring(o)	checktag((o), ctb(LUA_TSHRSTR))
//#define ttislngstring(o)	checktag((o), ctb(LUA_TLNGSTR))
//#define ttistable(o)		checktag((o), ctb(LUA_TTABLE))
//#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
//#define ttisclosure(o)		((rttype(o) & 0x1F) == LUA_TFUNCTION)
//#define ttisCclosure(o)		checktag((o), ctb(LUA_TCCL))
//#define ttisLclosure(o)		checktag((o), ctb(LUA_TLCL))
//#define ttislcf(o)		checktag((o), LUA_TLCF)
//#define ttisuserdata(o)		checktag((o), ctb(LUA_TUSERDATA))
//#define ttisthread(o)		checktag((o), ctb(LUA_TTHREAD))
//#define ttisdeadkey(o)		checktag((o), LUA_TDEADKEY)

#define ttisequal(o1,o2)	(rttype(o1) == rttype(o2))

/* Macros to access values */
//#define nvalue(o)	check_exp(ttisnumber(o), num_(o))
//#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)
#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)
#define rawtsvalue(o)	check_exp(ttisstring(o), (TString*)(val_(o).gc))
#define tsvalue(o)	(rawtsvalue(o))
#define rawuvalue(o)	check_exp(ttisuserdata(o), (Udata*)(val_(o).gc))
#define uvalue(o)	(rawuvalue(o))
#define clvalue(o)	check_exp(ttisclosure(o), (Closure*)(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), (LClosure*)(val_(o).gc))
#define clCvalue(o)	check_exp(ttisCclosure(o), (CClosure*)(val_(o).gc))
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
#define hvalue(o)	check_exp(ttistable(o), (Table*)(val_(o).gc))
#define bvalue(o)	check_exp(ttisboolean(o), val_(o).b)
#define thvalue(o)	check_exp(ttisthread(o), (lua_State*)(val_(o).gc))
/* a dead value may get the 'gc' field, but cannot access its contents */
#define deadvalue(o)	check_exp(ttisdeadkey(o), cast(void *, val_(o).gc))

#define l_isfalse(o)	((((TValue*)(o))->is_nil()) || ((((TValue*)(o))->is_boolean()) && bvalue(o) == 0))


//#define iscollectable(o)	(rttype(o) & BIT_ISCOLLECTABLE)


/* Macros for internal tests */
#define righttt(obj)		(ttype(obj) == gcvalue(obj)->gch.tt)

#define checkliveness(g,obj) \
	lua_longassert(!iscollectable(obj) || \
			(righttt(obj) && !isdead(g,gcvalue(obj))))


/* Macros to set values */
#define settt_(o,t)	((o)->tt_=(t))

#define setnvalue(obj,x) \
  { TValue *io=(obj); num_(io)=(x); settt_(io, LUA_TNUMBER); }

#define setnilvalue(obj) settt_(obj, LUA_TNIL)

#define setfvalue(obj,x) \
  { TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_TLCF); }

#define setpvalue(obj,x) \
  { TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_TLIGHTUSERDATA); }

#define setbvalue(obj,x) \
  { TValue *io=(obj); val_(io).b=(x); settt_(io, LUA_TBOOLEAN); }

#define setgcovalue(L,obj,x) \
  { TValue *io=(obj); GCObject *i_g=(x); \
    val_(io).gc=i_g; settt_(io, ctb(gch(i_g)->tt)); }

#define setsvalue(L,obj,x) \
  { TValue *io=(obj); \
    TString *x_ = (x); \
    val_(io).gc=cast(GCObject *, x_); settt_(io, ctb(x_->tt)); \
    checkliveness(G(L),io); }

#define setuvalue(L,obj,x) \
  { TValue *io=(obj); \
    val_(io).gc=cast(GCObject *, (x)); settt_(io, ctb(LUA_TUSERDATA)); \
    checkliveness(G(L),io); }

#define setthvalue(L,obj,x) \
  { TValue *io=(obj); \
    val_(io).gc=cast(GCObject *, (x)); settt_(io, ctb(LUA_TTHREAD)); \
    checkliveness(G(L),io); }

#define setclLvalue(L,obj,x) \
  { TValue *io=(obj); \
    val_(io).gc=cast(GCObject *, (x)); settt_(io, ctb(LUA_TLCL)); \
    checkliveness(G(L),io); }

#define setclCvalue(L,obj,x) \
  { TValue *io=(obj); \
    val_(io).gc=cast(GCObject *, (x)); settt_(io, ctb(LUA_TCCL)); \
    checkliveness(G(L),io); }

#define sethvalue(L,obj,x) \
  { TValue *io=(obj); \
    val_(io).gc=cast(GCObject *, (x)); settt_(io, ctb(LUA_TTABLE)); \
    checkliveness(G(L),io); }

#define setdeadvalue(obj)	settt_(obj, LUA_TDEADKEY)



#define setobj(L,obj1,obj2) \
	{ const TValue *io2=(obj2); TValue *io1=(obj1); \
	  io1->value_ = io2->value_; io1->tt_ = io2->tt_; \
	  checkliveness(G(L),io1); }


/*
** different types of assignments, according to destination
*/

/* from stack to (same) stack */
#define setobjs2s	setobj
/* to stack (not from same stack) */
#define setobj2s	setobj
#define setsvalue2s	setsvalue
#define sethvalue2s	sethvalue
#define setptvalue2s	setptvalue
/* from table to same table */
#define setobjt2t	setobj
/* to table */
#define setobj2t	setobj
/* to new object */
#define setobj2n	setobj
#define setsvalue2n	setsvalue


/* check whether a number is valid (useful only for NaN trick) */
#define luai_checknum(L,o,c)	{ /* empty */ }


/*
** {======================================================
** NaN Trick
** =======================================================
*/
#if defined(LUA_NANTRICK)

/*
** numbers are represented in the 'd_' field. All other values have the
** value (NNMARK | tag) in 'tt__'. A number with such pattern would be
** a "signaled NaN", which is never generated by regular operations by
** the CPU (nor by 'strtod')
*/

/* allows for external implementation for part of the trick */
#if !defined(NNMARK)	/* { */


#if !defined(LUA_IEEEENDIAN)
#error option 'LUA_NANTRICK' needs 'LUA_IEEEENDIAN'
#endif


#define NNMARK		0x7FF7A500
#define NNMASK		0x7FFFFF00

#undef TValuefields
#undef NILCONSTANT

#if (LUA_IEEEENDIAN == 0)	/* { */

/* little endian */
#define TValuefields  \
	union { struct { Value v__; int tt__; } i; double d__; } u
#define NILCONSTANT	{{{NULL}, tag2tt(LUA_TNIL)}}
/* field-access macros */
#define v_(o)		((o)->u.i.v__)
#define d_(o)		((o)->u.d__)
#define tt_(o)		((o)->u.i.tt__)

#else				/* }{ */

/* big endian */
#define TValuefields  \
	union { struct { int tt__; Value v__; } i; double d__; } u
#define NILCONSTANT	{{tag2tt(LUA_TNIL), {NULL}}}
/* field-access macros */
#define v_(o)		((o)->u.i.v__)
#define d_(o)		((o)->u.d__)
#define tt_(o)		((o)->u.i.tt__)

#endif				/* } */

#endif			/* } */


/* correspondence with standard representation */
#undef val_
#define val_(o)		v_(o)
#undef num_
#define num_(o)		d_(o)


#undef numfield
#define numfield	/* no such field; numbers are the entire struct */

/* basic check to distinguish numbers from non-numbers */
#undef ttisnumber
#define ttisnumber(o)	((tt_(o) & NNMASK) != NNMARK)

#define tag2tt(t)	(NNMARK | (t))

#undef rttype
#define rttype(o)	(ttisnumber(o) ? LUA_TNUMBER : tt_(o) & 0xff)

#undef settt_
#define settt_(o,t)	(tt_(o) = tag2tt(t))

#undef setnvalue
#define setnvalue(obj,x) \
	{ TValue *io_=(obj); num_(io_)=(x); lua_assert(ttisnumber(io_)); }

#undef setobj
#define setobj(L,obj1,obj2) \
	{ const TValue *o2_=(obj2); TValue *o1_=(obj1); \
	  o1_->u = o2_->u; \
	  checkliveness(G(L),o1_); }


/*
** these redefinitions are not mandatory, but these forms are more efficient
*/

#undef checktag
#undef checktype
#define checktag(o,t)	(tt_(o) == tag2tt(t))
#define checktype(o,t)	(ctb(tt_(o) | VARBITS) == ctb(tag2tt(t) | VARBITS))

#undef ttisequal
#define ttisequal(o1,o2)  \
	(ttisnumber(o1) ? ttisnumber(o2) : (tt_(o1) == tt_(o2)))


#undef luai_checknum
#define luai_checknum(L,o,c)	{ if (!ttisnumber(o)) c; }

#endif
/* }====================================================== */



/*
** {======================================================
** types and prototypes
** =======================================================
*/


union Value {
  GCObject *gc;    /* collectable objects */
  void *p;         /* light userdata */
  int b;           /* booleans */
  lua_CFunction f; /* light C functions */
  numfield         /* numbers */
};


class lua_TValue {
 private:
  inline const bool check_tag(int type) { return (tt_ == type); }
  inline const bool check_type(int type) { return (novariant(tt_) == type); }
 public:
  TValuefields;
  inline const bool is_collectable(void) { return (tt_ & BIT_ISCOLLECTABLE); }
  inline const bool is_number(void) { return check_tag(LUA_TNUMBER); }
  inline const bool is_nil(void) { return check_tag(LUA_TNIL); }
  inline const bool is_boolean(void) { return check_tag(LUA_TBOOLEAN); }
  inline const bool is_light_userdata(void) { return check_tag(LUA_TLIGHTUSERDATA); }
  inline const bool is_string(void) { return check_type(LUA_TSTRING); }
  inline const bool is_shr_string(void) { return check_tag(ctb(LUA_TSHRSTR)); }
  inline const bool is_table(void) { return check_tag(ctb(LUA_TTABLE)); }
  inline const bool is_function(void) { return check_type(LUA_TFUNCTION); }
  inline const bool is_closure(void) { return (tt_ & 0x1F) == LUA_TFUNCTION; }
  inline const bool is_c_closure(void) { return check_tag(ctb(LUA_TCCL)); }
  inline const bool is_l_closure(void) { return check_tag(ctb(LUA_TLCL)); }
  inline const bool is_lcf(void) { return check_type(LUA_TLCF); }
  inline const bool is_userdata(void) { return check_tag(ctb(LUA_TUSERDATA)); }
  inline const bool is_thread(void) { return check_tag(ctb(LUA_TTHREAD)); }
  inline const bool is_deadkey(void) { return check_type(LUA_TDEADKEY); }
  inline const lua_Number to_number(void) { return check_exp(is_number(), value_.n); }
  inline GCObject* to_gc(void) { return check_exp(is_collectable(), value_.gc); }
};


typedef TValue *StkId;  /* index to stack elements */




/*
** Header for string value; string bytes follow the end of this structure
*/
class TString : public GCObject {
 public:
  lu_byte extra;  /* reserved words for short strings; "has hash" for longs */
  unsigned int hash;
  size_t len;  /* number of characters in string */
};


/* get the actual string (array of bytes) from a TString */
#define getstr(ts)	cast(const char *, (ts) + 1)

/* get the actual string (array of bytes) from a Lua value */
#define svalue(o)       getstr(rawtsvalue(o))


/*
** Header for userdata; memory area follows the end of this structure
*/
class Udata : public GCObject {
 public:
  struct Table *metatable;
  struct Table *env;
  size_t len;  /* number of bytes */
};



/*
** Description of an upvalue for function prototypes
*/
typedef struct Upvaldesc {
  TString *name;  /* upvalue name (for debug information) */
  lu_byte instack;  /* whether it is in stack */
  lu_byte idx;  /* index of upvalue (in stack or in outer function's list) */
} Upvaldesc;


/*
** Description of a local variable for function prototypes
** (used for debug information)
*/
typedef struct LocVar {
  TString *varname;
  int startpc;  /* first point where variable is active */
  int endpc;    /* first point where variable is dead */
} LocVar;


/*
** Function Prototypes
*/
class Closure;

class Proto : public GCObject {
 public:
  TValue *k;  /* constants used by the function */
  Instruction *code;
  struct Proto **p;  /* functions defined inside the function */
  int *lineinfo;  /* map from opcodes to source lines (debug information) */
  LocVar *locvars;  /* information about local variables (debug information) */
  Upvaldesc *upvalues;  /* upvalue information */
  Closure *cache;  /* last created closure with this prototype */
  TString  *source;  /* used for debug information */
  int sizeupvalues;  /* size of 'upvalues' */
  int sizek;  /* size of `k' */
  int sizecode;
  int sizelineinfo;
  int sizep;  /* size of `p' */
  int sizelocvars;
  int linedefined;
  int lastlinedefined;
  GCObject *gclist;
  lu_byte numparams;  /* number of fixed parameters */
  lu_byte is_vararg;
  lu_byte maxstacksize;  /* maximum stack used by this function */
};



/*
** Lua Upvalues
*/
class UpVal : public GCObject {
 public:
  TValue *v;  /* points to stack or to its own value */
  union {
    TValue value;  /* the value (when closed) */
    struct {  /* double linked list (when open) */
      struct UpVal *prev;
      struct UpVal *next;
    } l;
  } u;
};


/*
** Closures
*/

#define ClosureHeader \
	lu_byte nupvalues; GCObject *gclist

class Closure : public GCObject {
 public:
  ClosureHeader;
};


class CClosure : public Closure {
 public:
  lua_CFunction f;
  TValue upvalue[1];  /* list of upvalues */
};


class LClosure : public Closure {
 public:
  struct Proto *p;
  UpVal *upvals[1];  /* list of upvalues */
};


//#define isLfunction(o)	ttisLclosure(o)

#define getproto(o)	(clLvalue(o)->p)


/*
** Tables
*/

typedef union TKey {
  struct {
    TValuefields;
    struct Node *next;  /* for chaining */
  } nk;
  TValue tvk;
} TKey;


typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;


class Table : public GCObject{
 public:
  lu_byte flags;  /* 1<<p means tagmethod(p) is not present */
  lu_byte lsizenode;  /* log2 of size of `node' array */
  struct Table *metatable;
  TValue *array;  /* array part */
  Node *node;
  Node *lastfree;  /* any free position is before this position */
  GCObject *gclist;
  int sizearray;  /* size of `array' array */
};



/*
** `module' operation for hashing (size is always a power of 2)
*/
#define lmod(s,size) \
	(check_exp((size&(size-1))==0, (cast(int, (s) & ((size)-1)))))


#define twoto(x)	(1<<(x))
#define sizenode(t)	(twoto((t)->lsizenode))


/*
** (address of) a fixed nil value
*/
#define luaO_nilobject		(&luaO_nilobject_)


LUAI_DDEC const TValue luaO_nilobject_;


LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC lua_Number luaO_arith (int op, lua_Number v1, lua_Number v2);
LUAI_FUNC int luaO_str2d (const char *s, size_t len, lua_Number *result);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);


#endif

