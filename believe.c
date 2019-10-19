#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#ifdef BEL_DEBUG
#define GC_DEBUG
#endif

#include <gc.h>

#define BELIEVE_VERSION   "0.1"
#define BELIEVE_COPYRIGHT "2019 Lucas Vieira"
#define BELIEVE_LICENSE   "MIT"

typedef enum
{
    BEL_SYMBOL,
    BEL_PAIR,
    BEL_CHAR,
    BEL_STREAM
} BEL_TYPE;

typedef struct BEL Bel; // Forward declaration

typedef struct
{
    Bel *car;
    Bel *cdr;
} Bel_pair;

typedef int8_t Bel_char;

typedef uint64_t Bel_sym;

typedef enum BEL_STREAM_STATUS
{
    BEL_STREAM_CLOSED,
    BEL_STREAM_READ,
    BEL_STREAM_WRITE
} BEL_STREAM_STATUS;

typedef struct
{
    BEL_STREAM_STATUS  status;
    FILE              *raw_stream;
    uint8_t            cache;
    uint8_t            cache_used;
} Bel_stream;

// Aliased as 'Bel' before
struct BEL
{
    BEL_TYPE type;
    union {
        Bel_sym     sym;
        Bel_pair   *pair;
        Bel_char    chr;
        Bel_stream  stream;
    };
};

#define BEL_NIL   ((Bel_sym)0)
#define BEL_T     ((Bel_sym)1)
#define BEL_O     ((Bel_sym)2)
#define BEL_APPLY ((Bel_sym)3)

Bel *bel_g_nil;
Bel *bel_g_t;
Bel *bel_g_o;
Bel *bel_g_apply;

Bel *bel_g_chars;
Bel *bel_g_ins_sys;
Bel *bel_g_outs_sys;
Bel *bel_g_ins;
Bel *bel_g_outs;
Bel *bel_g_prim;
Bel *bel_g_clo;

Bel *bel_g_scope;
Bel *bel_g_dynae;
Bel *bel_g_globe;

Bel *bel_mkerror(Bel *format, Bel *vars);   // Forward declaration
Bel *bel_mkstring(const char*);             // Forward declaration
Bel *bel_mksymbol(const char*);             // Forward declaration
Bel *bel_car(Bel*);                         // Forward declaration
Bel *bel_cdr(Bel*);                         // Forward declaration

#define bel_symbolp(x) ((x)->type==BEL_SYMBOL)

#define bel_nilp(x)                             \
    (bel_symbolp(x) && ((x)->sym==BEL_NIL))

#define bel_pairp(x)                            \
    (((x)->type==BEL_PAIR) || bel_nilp(x))

#define bel_atomp(x)                             \
    (bel_nilp(x) || !bel_pairp(x))

#define bel_charp(x)                            \
    (((x)->type==BEL_CHAR))

#define bel_streamp(x)                          \
    (((x)->type==BEL_STREAM))

int
bel_idp(Bel *x, Bel *y)
{
    if((!bel_symbolp(x) && !bel_charp(x))
       || (!bel_symbolp(y) && !bel_charp(y)))
        return 0;

    if(bel_symbolp(x))
        return (x->sym == y->sym);
    return (x->chr == y->chr);
}

int
bel_errorp(Bel *x)
{
    if(!bel_pairp(x)) return 0;
    if(!bel_idp(bel_car(x), bel_mksymbol("lit")))
        return 0;
    Bel *cdr = bel_cdr(x);
    if(!bel_idp(bel_car(cdr), bel_mksymbol("err")))
        return 0;
    return 1;
}

int
bel_proper_list_p(Bel *x)
{
    if(!bel_pairp(x))
        return 0;
    
    if(bel_nilp(x))
        return 1;
    
    Bel *itr = x;
    while(!bel_nilp(itr)) {
        itr = bel_cdr(itr);
        if(!bel_pairp(x))
            return 0;
    }

    return 1;
}

int
bel_stringp(Bel *x)
{
    if(!bel_proper_list_p(x)) {
        return 0;
    }

    Bel *itr = x;
    while(!bel_nilp(itr)) {
        Bel *car = bel_car(itr);

        if(!bel_charp(car))
            return 0;

        itr = bel_cdr(itr);
    }

    return 1;
}

int
bel_literalp(Bel *x)
{
    if(!bel_proper_list_p(x))
        return 0;

    return bel_idp(bel_car(x),
                   bel_mksymbol("lit"));
}

int
bel_primitivep(Bel *x)
{
    return bel_literalp(x)
        && bel_idp(bel_car(bel_cdr(x)),
                   bel_mksymbol("prim"));
}

int
bel_closurep(Bel *x)
{
    return bel_literalp(x)
        && bel_idp(bel_car(bel_cdr(x)),
                   bel_mksymbol("clo"));
}

int
bel_quotep(Bel *x)
{
    if(!bel_proper_list_p(x))
        return 0;

    return bel_idp(bel_car(x),
                   bel_mksymbol("quote"));
}

typedef struct {
    const char **tbl;
    uint64_t     n_syms;
    uint64_t     size;
} _Bel_sym_table;

_Bel_sym_table g_sym_table;

void
bel_sym_table_init(void)
{
    g_sym_table.n_syms = 4;
    g_sym_table.size   = 4;
    g_sym_table.tbl    =
        GC_MALLOC(g_sym_table.size * sizeof(char*));

    g_sym_table.tbl[BEL_NIL]   = "nil";
    g_sym_table.tbl[BEL_T]     = "t";
    g_sym_table.tbl[BEL_O]     = "o";
    g_sym_table.tbl[BEL_APPLY] = "apply";
}

Bel_sym bel_sym_table_add(const char*); // Forward declaration

Bel_sym
bel_sym_table_find(const char *sym_literal)
{
    uint64_t i;
    for(i = 0; i < g_sym_table.n_syms; i++) {
        if(!strcmp(sym_literal, g_sym_table.tbl[i])) {
            return i;
        }
    }

    return bel_sym_table_add(sym_literal);
}

Bel_sym
bel_sym_table_add(const char *sym_literal)
{
    if(g_sym_table.n_syms == g_sym_table.size) {
        uint64_t new_size = 2 * g_sym_table.size;
        g_sym_table.tbl = GC_REALLOC(g_sym_table.tbl,
                                     new_size * sizeof(char*));
        g_sym_table.size = new_size;
    }
    g_sym_table.tbl[g_sym_table.n_syms++] = sym_literal;
    return (g_sym_table.n_syms - 1);
}

const char*
bel_sym_find_name(Bel *sym)
{
    return g_sym_table.tbl[sym->sym];
}

Bel*
bel_mksymbol(const char *str)
{
    Bel *ret  = GC_MALLOC(sizeof (*ret));
    ret->type = BEL_SYMBOL;
    ret->sym  = bel_sym_table_find(str);
    return ret;
}

Bel*
bel_mkpair(Bel *car, Bel *cdr)
{
    Bel *ret  = GC_MALLOC(sizeof (*ret));
    ret->type = BEL_PAIR;
    ret->pair = GC_MALLOC(sizeof (Bel_pair));
    ret->pair->car = car;
    ret->pair->cdr = cdr;
    return ret;
}

Bel*
bel_car(Bel *p)
{
    if(!bel_pairp(p)) {
        return bel_mkerror(
            bel_mkstring("Cannot extract the car of ~a."),
            bel_mkpair(p, bel_g_nil));
    }
    
    if(bel_nilp(p))
        return bel_g_nil;
    return p->pair->car;
}

Bel*
bel_cdr(Bel *p)
{
    if(!bel_pairp(p)) {
        return bel_mkerror(
            bel_mkstring("Cannot extract the cdr of ~a."),
            bel_mkpair(p, bel_g_nil));
    }
    
    if(bel_nilp(p))
        return bel_g_nil;
    return p->pair->cdr;
}

uint64_t
bel_length(Bel *list)
{
    Bel *itr = list;
    uint64_t len = 0;
    while(!bel_nilp(itr)) {
        len++;
        itr = bel_cdr(itr);
    }
    return len;
}

Bel*
bel_mkchar(Bel_char c)
{
    Bel *ret  = GC_MALLOC(sizeof *ret);
    ret->type = BEL_CHAR;
    ret->chr  = c;
    return ret;
}

Bel*
bel_char_from_binary(Bel *list)
{
    if(!bel_pairp(list)) {
        return bel_mkerror(
            bel_mkstring("The binary representation of "
                         "a character must be a string of "
                         "characters \\0 and \\1."),
            bel_g_nil);
    }

    if(!bel_proper_list_p(list)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a proper "
                         "list, and therefore not a list "
                         "of characters \\0 and \\1."),
            bel_mkpair(list, bel_g_nil));
    }

    size_t len = bel_length(list);

    if(len != 8) {
        return bel_mkerror(
            bel_mkstring("The binary representation of "
                         "a character must have exactly "
                         "eight characters \\0 or \\1."),
            bel_g_nil);
    }
    
    Bel_char mask = '\0';
    size_t i;
    Bel *current = list;
    
    for(i = 0; i < len; i++) {
        Bel *bitchar = bel_car(current);

        if(!bel_charp(bitchar)) {
            return bel_mkerror(
                bel_mkstring("The provided binary "
                             "representation of a "
                             "character does not contain "
                             "only characters."),
                bel_g_nil);
        }

        if(bitchar->chr != '0' && bitchar->chr != '1') {
            return bel_mkerror(
                bel_mkstring("The binary representation of "
                             "a character must have exactly "
                             "eight characters \\0 or \\1."),
                bel_g_nil);
        }
        
        if(bitchar->chr == '1') {
            mask |= (1 << (7 - i));
        }
        current = bel_cdr(current);
    }
    return bel_mkchar(mask);
}

Bel*
bel_mkstring(const char *str)
{
    size_t len = strlen(str);

    if(len == 0)
        return bel_g_nil;
    
    Bel **pairs = GC_MALLOC(len * sizeof (Bel));

    // Create pairs where CAR is a character and CDR is nil
    size_t i;
    for(i = 0; i < len; i++) {
        Bel *chr  = GC_MALLOC(sizeof *chr);
        chr->type = BEL_CHAR;
        chr->chr  = str[i];
        pairs[i]  = bel_mkpair(chr, bel_g_nil);
    }

    // Link all pairs properly
    for(i = 0; i < len - 1; i++) {
        pairs[i]->pair->cdr = pairs[i + 1];
    }

    return pairs[0];
}

char*
bel_cstring(Bel *belstr)
{
    if(!bel_pairp(belstr)) {
        puts("INTERNAL ERROR on bel_cstring: "
             "argument is not a pair");
        return NULL;
    }
    
    if(!bel_stringp(belstr)) {
        puts("INTERNAL ERROR on bel_cstring: "
             "argument is not a string");
        return NULL;
    }
    
    uint64_t len = bel_length(belstr);
    if(len == 0) return NULL;
    
    char *str    = GC_MALLOC((len + 1) * sizeof (*str));

    Bel *itr     = belstr;
    size_t i     = 0;

    while(!bel_nilp(itr)) {
        str[i] = bel_car(itr)->chr;
        itr    = bel_cdr(itr);
        i++;
    }
    str[i] = '\0';
    return str;
}

Bel*
bel_mkstream(const char* name, BEL_STREAM_STATUS status)
{
    Bel *ret           = GC_MALLOC(sizeof *ret);
    ret->type          = BEL_STREAM;

    if(status == BEL_STREAM_CLOSED) {
        return bel_mkerror(
            bel_mkstring("Cannot create a stream with "
                         "CLOSED status."),
            bel_g_nil);
    }

    if(!strncmp(name, "ins", 3)) {
        ret->stream.raw_stream = stdin;
    } else if(!strncmp(name, "outs", 4)) {
        ret->stream.raw_stream = stdout;
    } else {
        ret->stream.raw_stream =
            fopen(name,
                  status == BEL_STREAM_READ ? "rb" : "wb");
        
        if(!ret->stream.raw_stream) {
            return bel_mkerror(
                bel_mkstring("Unable to open stream ~a."),
                bel_mkpair(
                    bel_mkstring(name), bel_g_nil));
        }
    }

    ret->stream.status     = status;
    ret->stream.cache      = 0u;
    ret->stream.cache_used = 0u;
    return ret;
}

Bel*
bel_stream_dump_cache(Bel_stream *stream)
{
    if(!fwrite(&stream->cache, 1, 1, stream->raw_stream)) {
        return bel_g_nil;
    }
    stream->cache_used = 0u;
    stream->cache      = 0u;
    return bel_g_t;
}

Bel*
bel_stream_write_bit(Bel_stream *stream, Bel_char bit)
{
    if(bit != '0' || bit != '1') {
        return bel_mkerror(
            bel_mkstring("Written bit must be represented "
                         "as a character 0 or 1"),
            bel_g_nil);
    }

    if(stream->status != BEL_STREAM_WRITE) {
        return bel_mkerror(
            bel_mkstring("Write stream is not at WRITE "
                         "state"),
            bel_g_nil);
    }

    if(stream->cache_used >= 8) {
        return bel_stream_dump_cache(stream);
    } else {
        if(bit == '1') {
            stream->cache |= (1 << (7 - stream->cache_used));
        }
        stream->cache_used++;
    }
    
    return bel_mkchar(bit);
}

Bel*
bel_stream_fill_cache(Bel_stream *stream)
{
    if(!fread(&stream->cache, 1, 1, stream->raw_stream)) {
        // Return nil on EOF
        return bel_g_nil;
    }
    stream->cache_used = 8;
    return bel_g_t;
}

Bel*
bel_stream_read_bit(Bel_stream *stream)
{
    if(stream->status != BEL_STREAM_READ) {
        return bel_mkerror(
            bel_mkstring("Read stream is not at READ "
                         "state"),
            bel_g_nil);
    }
    
    Bel *ret;
    if(stream->cache_used == 0) {
        ret = bel_stream_fill_cache(stream);
        if(bel_nilp(ret)) {
            return bel_mksymbol("eof");
        }
    }

    uint8_t mask = (1 << (stream->cache_used - 1));
    ret = bel_mkchar(((mask & stream->cache) == mask)
                     ? ((Bel_char)'1') : ((Bel_char)'0'));
    stream->cache_used--;
    return ret;
}

Bel*
bel_stream_close(Bel *obj)
{
    if(obj->type != BEL_STREAM) {
        return bel_mkerror(
            bel_mkstring("Cannot close something that "
                         "is not a stream."),
            bel_g_nil);
    }
    
    if(obj->stream.status == BEL_STREAM_CLOSED) {
        return bel_mkerror(
            bel_mkstring("Cannot close a closed stream."),
            bel_g_nil);
    }

    // Dump cache before closing
    if(obj->stream.status == BEL_STREAM_WRITE) {
        bel_stream_dump_cache(&obj->stream);
    }
    
    if(!fclose(obj->stream.raw_stream)) {
        obj->stream.raw_stream = NULL;
        obj->stream.status     = BEL_STREAM_CLOSED;
        return bel_g_t;
    }

    return bel_mkerror(
        bel_mkstring("Error closing stream: ~a."),
        bel_mkpair(
            bel_mkstring(strerror(errno)),
            bel_g_nil));
}

void
bel_init_streams(void)
{
    bel_g_ins      = bel_g_nil;
    bel_g_outs     = bel_g_nil;
    bel_g_ins_sys  = bel_mkstream("ins",  BEL_STREAM_READ);
    bel_g_outs_sys = bel_mkstream("outs", BEL_STREAM_WRITE);
}

Bel*
bel_mkerror(Bel *format, Bel *arglist)
{
    return bel_mkpair(
        bel_mksymbol("lit"),
        bel_mkpair(
            bel_mksymbol("err"),
            bel_mkpair(format, arglist)));
}

void
bel_init_ax_vars(void)
{
    bel_g_nil   = bel_mksymbol("nil");
    bel_g_t     = bel_mksymbol("t");
    bel_g_o     = bel_mksymbol("o");
    bel_g_apply = bel_mksymbol("apply");

    bel_g_prim  = bel_mksymbol("prim");
    bel_g_clo   = bel_mksymbol("clo");
}

char*
bel_conv_bits(uint8_t num)
{
    char *str = GC_MALLOC(9 * sizeof(*str));
    
    uint8_t i;
    for(i = 0; i < 8; i++) {
        int is_bit_set = num & (1 << i);
        str[7 - i] = is_bit_set ? '1' : '0';
    }
    str[8] = '\0';
    
    return str;
}

void
bel_init_ax_chars(void)
{
    // Create a vector of 255 list nodes
    Bel **list = GC_MALLOC(255 * sizeof(*list));

    size_t i;
    for(i = 0; i < 255; i++) {        
        // Build a pair which holds the character information
        Bel *pair = bel_mkpair(bel_mkchar((Bel_char)i),
                               bel_mkstring(bel_conv_bits(i)));
        // Assign the car of a node to the current pair,
        // set its cdr temporarily to nil
        list[i] = bel_mkpair(pair, bel_g_nil);
    }

    // Assign each pair cdr to the pair on the front.
    // Last pair should have a nil cdr still.
    for(i = 0; i < 254; i++) {
        list[i]->pair->cdr = list[i + 1];
    }

    // Hold reference to first element only
    bel_g_chars = list[0];
}

Bel*
bel_env_push(Bel *env, Bel *var, Bel *val)
{
    Bel *new_pair = bel_mkpair(var, val);
    return bel_mkpair(new_pair, env);
}

#define BEL_ENV_GLOBAL_PUSH(SYMSTR, VAL)           \
    (bel_g_globe =                                 \
     bel_env_push(bel_g_globe,                     \
                  bel_mksymbol(SYMSTR), VAL))

void
bel_init_ax_env(void)
{
    bel_g_globe = bel_g_nil;
    bel_g_dynae = bel_g_nil;
    bel_g_scope = bel_g_nil; // TODO: is this really necessary?
    
    BEL_ENV_GLOBAL_PUSH("chars", bel_g_chars);
    BEL_ENV_GLOBAL_PUSH("ins",   bel_g_ins);
    BEL_ENV_GLOBAL_PUSH("outs",  bel_g_outs);
}

Bel*
bel_env_lookup(Bel *env, Bel *sym)
{
    if(bel_nilp(env)) {
        return bel_g_nil;
    }
    
    if(!bel_symbolp(sym)) {
        return bel_mkerror(
            bel_mkstring("Cannot perform lookup of ~a, "
                         "which is not a symbol."),
            bel_mkpair(sym, bel_g_nil));
    }

    Bel *itr = env;
    while(!bel_nilp(itr)) {
        Bel *p = bel_car(itr);
        if(bel_car(p)->type == BEL_SYMBOL
           && bel_car(p)->sym == sym->sym) {
            return bel_cdr(p);
        }
        
        itr = bel_cdr(itr);
    }
    return bel_g_nil;
}

Bel*
bel_lookup(Bel *lenv, Bel *sym)
{
    Bel *value;

    // Lexical scope lookup
    value = bel_env_lookup(lenv, sym);
    if(!bel_nilp(value)) {
        return value;
    }

    // Dynamic scope lookup
    value = bel_env_lookup(bel_g_dynae, sym);
    if(!bel_nilp(value)) {
        return value;
    }

    // Global scope lookup
    value = bel_env_lookup(bel_g_globe, sym);
    if(bel_nilp(value)) {
        return bel_mkerror(
            bel_mkstring("The symbol ~a is unbound."),
            bel_mkpair(sym, bel_g_nil));
    }

    return value;
}

Bel*
bel_env_replace_val(Bel *env, Bel *sym, Bel *new_val)
{
    if(bel_nilp(env)) {
        return bel_g_nil;
    }
    
    Bel *itr = env;
    while(!bel_nilp(itr)) {
        Bel *p = bel_car(itr);
        if(bel_idp(sym, bel_car(p))) {
            p->pair->cdr = new_val;
            return sym;
        }
        itr = bel_cdr(itr);
    }
    return bel_g_nil;
}

Bel*
bel_env_unbind(Bel **env, Bel *sym)
{
    if(bel_nilp(*env)) {
        return bel_g_nil;
    }
    
    // If first element is a match, return
    // cdr of environment
    if(bel_idp(bel_car(bel_car(*env)), sym)) {
        *env = bel_cdr(*env);
        return bel_g_t;
    }

    // Iterate looking at the next element always.
    // If next element is a match, set current cdr
    // to cdr of next element
    Bel *itr = *env;
    while(!bel_nilp(bel_cdr(itr))) {
        Bel *p = bel_car(bel_cdr(itr));
        if(bel_idp(bel_car(p), sym)) {
            itr->pair->cdr = p->pair->cdr;
            return bel_g_t;
        }
        
        itr = bel_cdr(itr);
    }

    // On no substitution, return nil
    return bel_g_nil;
}

Bel*
bel_assign(Bel *lenv, Bel *sym, Bel *new_val)
{
    Bel *ret;

    // Lexical assignment
    ret = bel_env_replace_val(lenv, sym, new_val);
    if(!bel_nilp(ret)) return sym;

    // Dynamic assignment
    ret = bel_env_replace_val(bel_g_dynae, sym, new_val);
    if(!bel_nilp(ret)) return sym;

    // Global assignment
    ret = bel_env_replace_val(bel_g_globe, sym, new_val);
    if(!bel_nilp(ret)) return sym;

    // When not assignment was made, we push a global value
    // TODO: We might need a proper warning API
    puts("WARNING: Assignment failed, registering new global");
    bel_g_globe = bel_env_push(bel_g_globe, sym, new_val);
    return sym;
}

Bel*
bel_unbind(Bel **lenv, Bel *sym)
{
    Bel *ans;

    // Lexical unbinding
    ans = bel_env_unbind(lenv, sym);
    if(!bel_nilp(ans)) {
        return sym;
    }

    // Dynamic unbinding
    ans = bel_env_unbind(&bel_g_dynae, sym);
    if(!bel_nilp(ans)) {
        return sym;
    }

    // Global unbinding
    ans = bel_env_unbind(&bel_g_globe, sym);
    if(!bel_nilp(ans)) {
        return sym;
    }

    // On no unbinding, return nil
    return bel_g_nil;
}

Bel*
bel_mkliteral(Bel *rest)
{
    if(!bel_proper_list_p(rest)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a "
                         "proper list to be turned "
                         "into a literal."),
            bel_mkpair(rest, bel_g_nil));
    }

    return bel_mkpair(bel_mksymbol("lit"),
                      rest);
}

Bel*
bel_mkprim(Bel *sym)
{
    return bel_mkliteral(
        bel_mkpair(bel_g_prim,
                   bel_mkpair(sym, bel_g_nil)));
}

#define BEL_REGISTER_PRIM(env, x)               \
    {                                           \
    Bel *sym = bel_mksymbol(x);                 \
    env = bel_env_push(env, sym,                \
                       bel_mkprim(sym));        \
    }

Bel*
bel_gen_primitives(Bel *env)
{
    // Primitive functions
    BEL_REGISTER_PRIM(env, "id");
    BEL_REGISTER_PRIM(env, "join");
    BEL_REGISTER_PRIM(env, "car");
    BEL_REGISTER_PRIM(env, "cdr");
    BEL_REGISTER_PRIM(env, "type");
    BEL_REGISTER_PRIM(env, "xar");
    BEL_REGISTER_PRIM(env, "xdr");
    BEL_REGISTER_PRIM(env, "sym");
    BEL_REGISTER_PRIM(env, "nom");
    BEL_REGISTER_PRIM(env, "wrb");
    BEL_REGISTER_PRIM(env, "rdb");
    BEL_REGISTER_PRIM(env, "ops");
    BEL_REGISTER_PRIM(env, "cls");
    BEL_REGISTER_PRIM(env, "stat");
    BEL_REGISTER_PRIM(env, "coin");
    BEL_REGISTER_PRIM(env, "sys");

    // Primitive operators
    BEL_REGISTER_PRIM(env, "+");
    BEL_REGISTER_PRIM(env, "-");
    BEL_REGISTER_PRIM(env, "*");
    BEL_REGISTER_PRIM(env, "/");
    BEL_REGISTER_PRIM(env, "<");
    BEL_REGISTER_PRIM(env, "<=");
    BEL_REGISTER_PRIM(env, ">");
    BEL_REGISTER_PRIM(env, ">=");
    BEL_REGISTER_PRIM(env, "=");

    // Other primitives
    BEL_REGISTER_PRIM(env, "err");
    
    return env;
}

void
bel_init_ax_primitives()
{
    bel_g_globe = bel_gen_primitives(bel_g_globe);
}

Bel*
bel_mkclosure(Bel *lenv, Bel *rest)
{
    return bel_mkliteral(
        bel_mkpair(bel_g_clo,
                   bel_mkpair(lenv, rest)));
}

void bel_print(Bel*); // Forward declaration

void
bel_print_pair(Bel *obj)
{
    if(bel_nilp(obj)) return;
    
    Bel *itr = obj;
    
    putchar('(');
    while(!bel_nilp(itr)) {
        Bel *car = bel_car(itr);
        Bel *cdr = bel_cdr(itr);

        bel_print(car);
        
        if(bel_nilp(cdr)) {
            break;
        } else if(cdr->type != BEL_PAIR) {
            putchar(' ');
            putchar('.');
            putchar(' ');
            bel_print(cdr);
            break;
        }
        putchar(' ');
        itr = cdr;
    }
    putchar(')');
}

void
bel_print_string(Bel *obj)
{
    putchar('\"');
    Bel *itr = obj;
    while(!bel_nilp(itr)) {
        Bel_char c = bel_car(itr)->chr;

        switch(c) {
        case '\a': printf("\\bel"); break;
        default:   putchar(c);      break;
        }

        itr = bel_cdr(itr);
    }
    putchar('\"');
}

void
bel_print_stream(Bel *obj)
{
    printf("#<stream :status ");
    if(obj->stream.status == BEL_STREAM_CLOSED) {
        printf("closed>");
    } else {
        switch(obj->stream.status) {
        case BEL_STREAM_READ:  printf("input ");  break;
        case BEL_STREAM_WRITE: printf("output "); break;
        default: printf("unknown ");              break;
        }
        printf("{0x%08lx}>", (uint64_t)obj->stream.raw_stream);
    }
}

void
bel_print(Bel *obj)
{
    switch(obj->type) {
    case BEL_SYMBOL:
        printf("%s", g_sym_table.tbl[obj->sym]);
        break;
    case BEL_PAIR:
        if(!bel_stringp(obj)) {
            bel_print_pair(obj);
        } else {
            bel_print_string(obj);
        }
        break;
    case BEL_CHAR:
        if(obj->chr == '\a')
            printf("\\bel"); // There is no Bel without \bel
        else printf("\\%c", obj->chr);
        break;
    case BEL_STREAM:
        bel_print_stream(obj);
        break;
    default:
        printf("#<\?\?\?>"); // wat
        break;
    };
}

Bel *bel_eval(Bel *exp, Bel *lenv);             // Forward declaration
Bel *bel_apply(Bel *proc, Bel *args);           // Forward declaration
Bel *bel_evlist(Bel *elist, Bel *lenv);         // Forward declaration
Bel *bel_apply_primop(Bel *sym, Bel *args);     // Forward declaration
Bel *bel_bind(Bel *vars, Bel *vals, Bel *lenv); // Forward declaration

Bel*
bel_eval(Bel *exp, Bel *lenv)
{
#ifdef BEL_DEBUG
        printf("eval>  ");
        bel_print(exp);
        putchar(10);
#endif

    // number: eval to itself
    // TODO
    
    // symbol: look it up
    if(bel_symbolp(exp)) {
        return bel_lookup(lenv, exp);
    }

    // quote, length two:  give back the second element
    if(bel_quotep(exp)) {
        uint64_t len = bel_length(exp);
        if(len != 2) {
            return bel_mkerror(
                bel_mkstring("Malformed quote: can only "
                             "quote one object."),
                bel_g_nil);
        }

        return bel_car(bel_cdr(exp));
    }
    
    // lit: eval to itself
    else if(bel_literalp(exp)) {
        return exp;
    }
    
    // fn: closure
    else if(bel_proper_list_p(exp)
            && bel_idp(bel_car(exp),
                      bel_mksymbol("fn"))) {
        return bel_mkclosure(lenv, bel_cdr(exp));
    }
    
    // if
    // TODO
    
    // apply
    // TODO
    
    // where (not straightforward)
    // TODO
    
    // dyn: dynamic binding
    // TODO
    
    // after
    // TODO
    
    // set: Global binding
    // TODO
    
    // TODO: ccc
    
    // TODO: thread

    // else it is the case of an application
    else {
        if(!bel_proper_list_p(exp)) {
            return bel_mkerror(
                bel_mkstring("~a is not a proper list "
                             "for the application of "
                             "a function."),
                bel_mkpair(exp, bel_g_nil));
        }
        return bel_apply(bel_eval(bel_car(exp), lenv),
                         bel_evlist(bel_cdr(exp), lenv));
    }
}

Bel*
bel_apply(Bel *fun, Bel *args)
{
#ifdef BEL_DEBUG
    printf("apply> ");
    bel_print(fun);
    printf(" at ");
    bel_print(args);
    putchar(10);
#endif
    
    // Check for errors on fun
    if(bel_errorp(fun)) {
        return fun;
    }
    
    // Primitive procedure
    else if(bel_primitivep(fun)) {
        return bel_apply_primop(
            bel_car(bel_cdr(bel_cdr(fun))),
            args);
    }
    
    // Closure
    else if(bel_closurep(fun)) {
        Bel *lenv =
            bel_car(
                bel_cdr(bel_cdr(fun)));
        Bel *lambda_list =
            bel_car(
                bel_cdr(bel_cdr(bel_cdr(fun))));
        Bel *body =
            bel_car(
                bel_cdr(bel_cdr(bel_cdr(
                                    bel_cdr(fun)))));
        
        // Generate a new environment with the
        // arguments bound in it
        Bel *new_env = bel_bind(lambda_list,
                                args,
                                lenv);

        if(bel_errorp(new_env)) {
            return new_env;
        }

        // Evaluate body on the new environment
        return bel_eval(body, new_env);
    }

    // Error
    else {
        return bel_mkerror(
            bel_mkstring("~a is not a procedure"),
            bel_mkpair(fun, bel_g_nil));
    }
}

Bel*
bel_evlist(Bel *elist, Bel *lenv)
{
    if(bel_nilp(elist)) {
        return bel_g_nil;
    }

    Bel *eval_result =
        bel_eval(bel_car(elist), lenv);

    if(bel_errorp(eval_result)) {
        return eval_result;
    }

    Bel *ev_rest =
        bel_evlist(bel_cdr(elist), lenv);

    if(bel_errorp(ev_rest)) {
        return ev_rest;
    }

    return bel_mkpair(eval_result, ev_rest);
}

// Forward declarations of primitive functions
Bel *bel_prim_id(Bel *args);
Bel *bel_prim_join(Bel *args);
Bel *bel_prim_car(Bel *args);
Bel *bel_prim_cdr(Bel *args);
Bel *bel_prim_type(Bel *args);
Bel *bel_prim_xar(Bel *args);
Bel *bel_prim_xdr(Bel *args);
Bel *bel_prim_sym(Bel *args);
Bel *bel_prim_nom(Bel *args);
Bel *bel_prim_wrb(Bel *args);
Bel *bel_prim_rdb(Bel *args);
Bel *bel_prim_ops(Bel *args);
Bel *bel_prim_cls(Bel *args);
Bel *bel_prim_stat(Bel *args);
Bel *bel_prim_coin(Bel *args);
//Bel *bel_prim_sys(Bel *args);

// Forward declarations of primitive operators
//Bel *bel_prim_add(Bel *args);
//Bel *bel_prim_sub(Bel *args);
//Bel *bel_prim_mul(Bel *args);
//Bel *bel_prim_div(Bel *args);
//Bel *bel_prim_less(Bel *args);
//Bel *bel_prim_leq(Bel *args);
//Bel *bel_prim_great(Bel *args);
//Bel *bel_prim_geq(Bel *args);
//Bel *bel_prim_eq(Bel *args);

//  Forward declarations of other primitives
Bel *bel_prim_err(Bel *args);

#define bel_is_prim(sym, lit)                     \
    (bel_idp(sym, bel_mksymbol(lit)))

#define bel_unimplemented(sym)                                  \
    bel_mkerror(                                                \
    bel_mkstring("~a is not implemented."),                     \
    bel_mkpair(sym, bel_g_nil))

Bel*
bel_apply_primop(Bel *sym, Bel *args)
{
    // Primitive functions
    if(bel_is_prim(sym, "id"))
        return bel_prim_id(args);

    else if(bel_is_prim(sym, "join"))
        return bel_prim_join(args);

    else if(bel_is_prim(sym, "car"))
        return bel_prim_car(args);

    else if(bel_is_prim(sym, "cdr"))
        return bel_prim_cdr(args);

    else if(bel_is_prim(sym, "type"))
        return bel_prim_type(args);

    else if(bel_is_prim(sym, "xar"))
        return bel_prim_xar(args);

    else if(bel_is_prim(sym, "xdr"))
        return bel_prim_xdr(args);

    else if(bel_is_prim(sym, "sym"))
        return bel_prim_sym(args);

    else if(bel_is_prim(sym, "nom"))
        return bel_prim_nom(args);

    else if(bel_is_prim(sym, "wrb"))
        return bel_prim_wrb(args);

    else if(bel_is_prim(sym, "rdb"))
        return bel_prim_rdb(args);

    else if(bel_is_prim(sym, "ops"))
        return bel_prim_ops(args);

    else if(bel_is_prim(sym, "cls"))
        return bel_prim_cls(args);

    else if(bel_is_prim(sym, "stat"))
        return bel_prim_stat(args);

    else if(bel_is_prim(sym, "coin"))
        return bel_prim_coin(args);

    else if(bel_is_prim(sym, "sys"))
        return bel_unimplemented(sym);

    // Primitive operators
    else if(bel_is_prim(sym, "+"))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, "-"))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, "*"))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, "/"))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, "<"))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, "<="))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, ">"))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, ">="))
        return bel_unimplemented(sym);

    else if(bel_is_prim(sym, "="))
        return bel_unimplemented(sym);
    
    // Other primitives
    else if(bel_is_prim(sym, "err"))
        return bel_prim_err(args);

    else {
        return bel_mkerror(
            bel_mkstring("Unknown primitive ~a."),
            bel_mkpair(sym, bel_g_nil));
    }
}

#define BEL_CHECK_ARITY(args, num)                      \
    {                                                   \
    uint64_t length = bel_length(args);                 \
    if(length != num) {                                 \
    return bel_mkerror(                                 \
        bel_mkstring("Arity error"), bel_g_nil);        \
    }                                                   \
    }

Bel*
bel_prim_id(Bel *args)
{
    BEL_CHECK_ARITY(args, 2);
    return (bel_idp(bel_car(args),
                    bel_car(bel_cdr(args)))
            ? bel_g_t : bel_g_nil);
}

Bel*
bel_prim_join(Bel *args)
{
    BEL_CHECK_ARITY(args, 2);
    return bel_mkpair(bel_car(args),
                      bel_car(bel_cdr(args)));
}

Bel*
bel_prim_car(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    return bel_car(bel_car(args));
}

Bel*
bel_prim_cdr(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    return bel_cdr(bel_car(args));
}

Bel*
bel_prim_type(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    switch(bel_car(args)->type) {
    case BEL_SYMBOL: return bel_mksymbol("symbol");  break;
    case BEL_PAIR:   return bel_mksymbol("pair");    break;
    case BEL_CHAR:   return bel_mksymbol("char");    break;
    case BEL_STREAM: return bel_mksymbol("stream");  break;
    default:         return bel_mksymbol("unknown"); break;
    };
}

Bel*
bel_prim_xar(Bel *args)
{
    BEL_CHECK_ARITY(args, 2);
    Bel *pair = bel_car(args);
    Bel *val  = bel_car(bel_cdr(args));
    if(!bel_pairp(pair)) {
        return bel_mkerror(
            bel_mkstring("~a is not a pair."),
            bel_mkpair(pair, bel_g_nil));
    }

    pair->pair->car = val;
    return val;
}

Bel*
bel_prim_xdr(Bel *args)
{
    BEL_CHECK_ARITY(args, 2);
    Bel *pair = bel_car(args);
    Bel *val  = bel_car(bel_cdr(args));
    if(!bel_pairp(pair)) {
        return bel_mkerror(
            bel_mkstring("~a is not a pair."),
            bel_mkpair(pair, bel_g_nil));
    }

    pair->pair->cdr = val;
    return val;
}

Bel*
bel_prim_sym(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    Bel *str = bel_car(args);
    if(!bel_stringp(str)) {
        return bel_mkerror(
            bel_mkstring("The object ~a must be a string."),
            bel_mkpair(str, bel_g_nil));
    }

    char *cstr = bel_cstring(str);
    if(!cstr || !strcmp(cstr, "")) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a proper string."),
            bel_mkpair(str, bel_g_nil));
    }

    return bel_mksymbol(cstr);
}

Bel*
bel_prim_nom(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    Bel *sym = bel_car(args);
    if(!bel_symbolp(sym)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a string."),
            bel_mkpair(sym, bel_g_nil));
    }

    return bel_mkstring(
        bel_sym_find_name(sym));
}

Bel*
bel_prim_wrb(Bel *args)
{
    BEL_CHECK_ARITY(args, 2);
    Bel *x = bel_car(args);
    Bel *y = bel_car(bel_cdr(args));

    if(!bel_charp(x)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a character."),
            bel_mkpair(x, bel_g_nil));
    }

    if(bel_nilp(y)) {
        y = bel_lookup(bel_g_nil, bel_mksymbol("outs"));
    } else {
        if(!bel_streamp(y)) {
            return bel_mkerror(
                bel_mkstring("The object ~a must be a stream."),
                bel_mkpair(y, bel_g_nil));
        }
    }

    return bel_stream_write_bit(&y->stream, y->chr);
}

Bel*
bel_prim_rdb(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    Bel *x = bel_car(args);

    if(bel_nilp(x)) {
        bel_lookup(bel_g_nil, bel_mksymbol("ins"));
    } else {
        if(!bel_streamp(x)) {
            return bel_mkerror(
                bel_mkstring("The object ~a must be a stream."),
                bel_mkpair(x, bel_g_nil));
        }
    }

    return bel_stream_read_bit(&x->stream);
}

Bel*
bel_prim_ops(Bel *args)
{
    BEL_CHECK_ARITY(args, 2);
    Bel *x = bel_car(args);
    Bel *y = bel_car(bel_cdr(args));

    if(!bel_stringp(x)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a string."),
            bel_mkpair(x, bel_g_nil));
    }

    if(!bel_symbolp(y)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a symbol."),
            bel_mkpair(y, bel_g_nil));
    }

    if(bel_idp(y, bel_mksymbol("in"))) {
        return bel_mkstream(bel_cstring(x), BEL_STREAM_READ);
    } else if(bel_idp(y, bel_mksymbol("out"))) {
        return bel_mkstream(bel_cstring(x), BEL_STREAM_WRITE);
    }

    return bel_mkerror(
        bel_mkstring("The object ~a is not one of the "
                     "symbols `in` and `out`."),
        bel_mkpair(y, bel_g_nil));
}

Bel*
bel_prim_cls(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    Bel *stream = bel_car(args);

    if(!bel_streamp(stream)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a stream."),
            bel_mkpair(stream, bel_g_nil));
    }

    if(stream->stream.status == BEL_STREAM_CLOSED) {
        return bel_g_nil;
    }

    return bel_stream_close(stream);
}

Bel*
bel_prim_stat(Bel *args)
{
    BEL_CHECK_ARITY(args, 1);
    Bel *stream = bel_car(args);
    if(!bel_streamp(stream)) {
        return bel_mkerror(
            bel_mkstring("The object ~a is not a stream."),
            bel_mkpair(stream, bel_g_nil));
    }

    switch(stream->stream.status) {
    case BEL_STREAM_CLOSED: return bel_mksymbol("closed");
    case BEL_STREAM_READ:   return bel_mksymbol("in");
    case BEL_STREAM_WRITE:  return bel_mksymbol("out");
    default: // ...wat
        return bel_mkerror(
            bel_mkstring("The stream ~a has an unknown "
                         "status."),
            bel_mkpair(stream, bel_g_nil));
    }
}

Bel*
bel_prim_coin(Bel *args)
{
    BEL_CHECK_ARITY(args, 0);
    return (rand() % 2) ? bel_g_t : bel_g_nil;
}

Bel*
bel_prim_err(Bel *args)
{
    Bel *string = bel_car(args);
    if(!bel_stringp(string)) {
        return bel_mkerror(
            bel_mkstring("First argument of `err` must "
                         "be a string format."),
            bel_g_nil);
    }

    // TODO: Maybe quote?
    return bel_mkerror(string, bel_cdr(args));
}

Bel*
bel_bind(Bel *vars, Bel *vals, Bel *lenv)
{
    int vars_ended = bel_nilp(vars);
    int vals_ended = bel_nilp(vals);

    if(vars_ended && !vals_ended) {
        return bel_mkerror(
            bel_mkstring("Too many variables in "
                         "function application"),
            bel_g_nil);
    } else if(!vars_ended && vals_ended) {
        return bel_mkerror(
            bel_mkstring("Too few values in "
                         "function application"),
            bel_g_nil);
    } else if(vars_ended && vals_ended) {
        return lenv;
    }

    Bel *binding = bel_mkpair(bel_car(vars),
                              bel_car(vals));

    return bel_bind(bel_cdr(vars),
                    bel_cdr(vals),
                    bel_mkpair(binding, lenv));
}

void
string_test()
{
    Bel *bel  = bel_mkstring("Hello, Bel!");
    bel_print(bel);
    printf(" => %s\n", bel_cstring(bel));

    bel = bel_mkstring("There is no Bel without \a");
    bel_print(bel);
    putchar(10);
}

void
notation_test()
{
    Bel*
    bel = bel_mkpair(bel_mkpair(bel_mksymbol("foo"),
                                bel_mksymbol("bar")),
                     bel_mkpair(bel_mksymbol("baz"),
                                bel_mksymbol("quux")));
    bel_print(bel);
    putchar(10);
}

void
list_test()
{
    Bel*
    bel = bel_mkpair(
        bel_mksymbol("The"),
        bel_mkpair(
            bel_mksymbol("quick"),
            bel_mkpair(
                bel_mksymbol("brown"),
                bel_mkpair(
                    bel_mksymbol("fox"),
                    bel_mkpair(
                        bel_mksymbol("jumps"),
                        bel_mkpair(
                            bel_mksymbol("over"),
                            bel_mkpair(
                                bel_mksymbol("the"),
                                bel_mkpair(
                                    bel_mksymbol("lazy"),
                                    bel_mkpair(
                                        bel_mksymbol("dog"),
                                        bel_g_nil)))))))));
    bel_print(bel);
    putchar(10);
}

void
closure_repr_test()
{
    Bel*
    bel = bel_mkpair(bel_mksymbol("lit"),
                     bel_mkpair(
                         bel_mksymbol("clo"),
                         bel_mkpair(
                             bel_g_nil,
                             bel_mkpair(
                                 bel_mkpair(bel_mksymbol("x"),
                                            bel_g_nil),
                                 bel_mkpair(
                                     bel_mkpair(
                                         bel_mksymbol("*"),
                                         bel_mkpair(
                                             bel_mksymbol("x"),
                                             bel_mkpair(
                                                 bel_mksymbol("x"),
                                                 bel_g_nil))),
                                     bel_g_nil)))));
    bel_print(bel);
    putchar(10);
}

void
character_list_test()
{
    // Character list
    // Char: 000 (?) => "00000000"
    // Char: 001 (?) => "00000001"
    // etc
    const int first_char = 'a';
    
    Bel *bel = bel_env_lookup(bel_g_globe, bel_mksymbol("chars"));
    
    int i;

    // Get nth cdr
    for(i = 0; i < first_char; i++) {
        bel = bel_cdr(bel);
    }

    i = 'a';
    while(!bel_nilp(bel) && i < first_char + 10) {
        Bel *car = bel_car(bel);
        printf("Char: %03d (%c) => ",
               bel_car(car)->chr,
               ((Bel_char)i));
        bel_print(bel_cdr(car));
        putchar(10);
        bel = bel_cdr(bel);
        i++;
    }
}

void
read_file_test()
{
    // We are going to read ten bytes from Bel's
    // own source code file.
    Bel *file = bel_mkstream("believe.c", BEL_STREAM_READ);

    if(bel_errorp(file)) {
        bel_print(file);
        return;
    }

    printf("Stream: ");
    bel_print(file);
    putchar(10);
    
    int n_bytes = 10;
    while(n_bytes > 0) {
        // 1 byte = 8 bits, so we make a list of
        // eight characters
        Bel **char_nodes = GC_MALLOC(8 * sizeof(Bel*));

        int i;
        for(i = 0; i < 8; i++) {
            Bel *read_char =
                bel_stream_read_bit(&file->stream);
            char_nodes[i] = bel_mkpair(read_char, bel_g_nil);
        }

        // Link nodes
        for(i = 0; i < 7; i++) {
            char_nodes[i]->pair->cdr = char_nodes[i + 1];
        }

        // Display on screen
        bel_print(char_nodes[0]);
        printf(" => ");
        bel_print(
            bel_char_from_binary(char_nodes[0]));
        putchar(10);
        
        n_bytes--;
    }

    bel_stream_close(file);
}

void
show_errors_test()
{
    Bel *err;
    
    // Unexisting file
    err = bel_mkstream("waddawaddawadda", BEL_STREAM_READ);
    bel_print(err);
    putchar(10);
    printf("Is this an error? %c\n",
           bel_errorp(err) ? 'y' : 'n');

    // Incorrect use of car and cdr
    err = bel_car(bel_g_t);
    bel_print(err); putchar(10);
    err = bel_cdr(bel_g_t);
    bel_print(err); putchar(10);

    // Incorrect generation of Bel character from binary
    /* Bel *str = bel_mkstring("110"); */
    /* err = bel_char_from_binary(str); */
    /* bel_print(err); putchar(10); */

    /* str = bel_mkstring("110a1101"); */
    /* err = bel_char_from_binary(str); */
    /* bel_print(err); putchar(10); */
}

void
lookup_primitives_test()
{
    Bel *bel;
    bel = bel_lookup(bel_g_nil, bel_mksymbol("car"));
    bel_print(bel);
    putchar(10);

    bel = bel_lookup(bel_g_nil, bel_mksymbol("cdr"));
    bel_print(bel);
    putchar(10);

    bel = bel_lookup(bel_g_nil, bel_mksymbol("coin"));
    bel_print(bel);
    putchar(10);
    
    bel = bel_lookup(bel_g_nil, bel_mksymbol("stat"));
    bel_print(bel);
    putchar(10);

    // Undefined primitive
    bel_print(bel_g_nil); putchar(10);
    bel = bel_lookup(bel_g_nil, bel_mksymbol("wadawada"));
    bel_print(bel);
    putchar(10);
}

void
lexical_environment_test()
{
    Bel *lexenv = bel_g_nil;
    Bel *ret;

    puts("    -- Registering local `foo`");
    lexenv = bel_env_push(lexenv,
                          bel_mksymbol("foo"),
                          bel_mksymbol("bar"));
    
    printf("Environment:       ");
    bel_print(lexenv);
    printf("\nLookup:            ");
    bel_print(bel_lookup(lexenv, bel_mksymbol("foo")));
    putchar(10); putchar(10);

    // Assignment
    puts("    -- Assigning new value to `foo`");
    ret =
        bel_assign(lexenv,
                   bel_mksymbol("foo"),
                   bel_mkliteral(bel_mkpair(bel_mksymbol("baz"),
                                            bel_g_nil)));

    printf("Environment:       ");
    bel_print(lexenv);
    printf("\nAssignment result: ");
    bel_print(ret);
    printf("\nLookup:            ");
    bel_print(bel_lookup(lexenv, bel_mksymbol("foo")));
    putchar(10); putchar(10);

    // Unbinding
    puts("    -- Unbinding `foo`");
    ret = bel_unbind(&lexenv, bel_mksymbol("foo"));
    
    printf("Environment:       ");
    bel_print(lexenv);
    printf("\nUnbinding result:  ");
    bel_print(ret);
    printf("\nLookup:            ");
    bel_print(bel_lookup(lexenv, bel_mksymbol("foo")));
    putchar(10);
}

void
global_assignment_test()
{
    Bel *lexenv = bel_g_nil;
    Bel *ret;

    // Global creation through assignment
    puts("    -- Assigning `foo` without previous definition");
    ret = bel_assign(bel_g_nil,
                     bel_mksymbol("foo"),
                     bel_mksymbol("bar"));

    printf("Assignment result: ");
    bel_print(ret);
    printf("\nLookup:            ");
    bel_print(bel_lookup(bel_g_nil, bel_mksymbol("foo")));
    putchar(10); putchar(10);

    // Local creation of variable bound to
    // same symbol
    puts("    -- Shadowing global `foo` with a local");
    lexenv =
        bel_env_push(lexenv,
                     bel_mksymbol("foo"),
                     bel_mksymbol("quux"));

    printf("Environment:       ");
    bel_print(lexenv);
    printf("\nLookup:            ");
    bel_print(bel_lookup(lexenv, bel_mksymbol("foo")));

    // Three unbindings
    printf("\n    -- Unbinding `foo` three times");
    int i;
    for(i = 0; i < 3; i++) {
        ret = bel_unbind(&lexenv, bel_mksymbol("foo"));

        printf("\n\nEnvironment:       ");
        bel_print(lexenv);
        printf("\nUnbinding result:  ");
        bel_print(ret);
        printf("\nLookup:            ");
        bel_print(bel_lookup(lexenv, bel_mksymbol("foo")));
    }
    putchar(10);
}

void
eval_test()
{
    Bel *form;
    Bel *result;

    // (quote foo)
    form = bel_mkpair(
        bel_mksymbol("quote"),
        bel_mkpair(
            bel_mksymbol("foo"),
            bel_g_nil));
    printf("Form:   ");
    bel_print(form);
    putchar(10);
    
    result = bel_eval(form, bel_g_nil);
    
    printf("Result: "); bel_print(result);
    putchar(10); putchar(10);
    
    // (join (quote foo) (quote bar))
    form =
        bel_mkpair(
            bel_mksymbol("join"),
            bel_mkpair(
                bel_mkpair(
                    bel_mksymbol("quote"),
                    bel_mkpair(
                        bel_mksymbol("foo"),
                        bel_g_nil)),
                bel_mkpair(
                    bel_mkpair(
                        bel_mksymbol("quote"),
                        bel_mkpair(
                            bel_mksymbol("bar"),
                            bel_g_nil)),
                    bel_g_nil)));
    printf("Form:   ");
    bel_print(form);
    putchar(10);
    
    result = bel_eval(form, bel_g_nil);
    
    printf("Result: "); bel_print(result);
    putchar(10); putchar(10);


    // (fn (x) (id x x))
    form = bel_mkpair(
        bel_mksymbol("fn"),
        bel_mkpair(
            bel_mkpair(
                bel_mksymbol("x"),
                bel_g_nil),
            bel_mkpair(
                bel_mkpair(
                    bel_mksymbol("id"),
                    bel_mkpair(
                        bel_mksymbol("x"),
                        bel_mkpair(
                            bel_mksymbol("x"),
                            bel_g_nil))),
                bel_g_nil)));
    printf("Form:   ");
    bel_print(form);
    putchar(10);
    
    result = bel_eval(form, bel_g_nil);
    
    printf("Result: "); bel_print(result);
    putchar(10); putchar(10);

    
    // ((fn (x) (id x x)) (quote foo))
    form =
        bel_mkpair(
            form, // Use closure from last example
            bel_mkpair(
                bel_mkpair(
                    bel_mksymbol("quote"),
                    bel_mkpair(
                        bel_mksymbol("foo"),
                        bel_g_nil)),
                bel_g_nil));
    printf("Form:   ");
    bel_print(form);
    putchar(10);
    
    result = bel_eval(form, bel_g_nil);
    
    printf("Result: "); bel_print(result);
    putchar(10); putchar(10);
}

Bel*
bel_init(void)
{
    // Initialize garbage collector
    GC_INIT();

    // Initialize random number generation
    // Warning: This is a VERY naive approach
    srand(time(NULL));

    // Initialize symbol table
    bel_sym_table_init();

    // Axioms
    bel_init_ax_vars();
    bel_init_ax_chars();
    bel_init_streams();
    bel_init_ax_env();
    bel_init_ax_primitives();

    // TODO: Return an environment?
    return bel_g_nil;
}

void
run_tests()
{
    puts("-- Running debug tests");
    puts("  -- String test");
    string_test();
    puts("  -- Notation test");
    notation_test();
    puts("  -- List test");
    list_test();
    puts("  -- Closure representation test");
    closure_repr_test();
    puts("  -- Character List & Lookup test");
    character_list_test();
    puts("  -- Read five bytes from Believe's source");
    read_file_test();
    puts("  -- Show a few errors on screen");
    show_errors_test();
    puts("  -- Lookup a few primitives and print them");
    lookup_primitives_test();
    puts("  -- Lexical environment tests");
    lexical_environment_test();
    puts("  -- Globals and assignment tests");
    global_assignment_test();
    puts("  -- Evaluator test");
    eval_test();
}

int
main(void)
{
    printf("Believe %s\n", BELIEVE_VERSION);
    printf("A Bel Lisp interpreter\n");
    printf("Copyright (c) %s\n", BELIEVE_COPYRIGHT);
    printf("This software is distributed under the %s license.\n",
          BELIEVE_LICENSE);

    bel_init();

#ifdef BEL_DEBUG
    run_tests();
#endif
    
    return 0;
}
