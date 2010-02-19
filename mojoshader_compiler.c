/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

#if DEBUG_COMPILER_PARSER
#define LEMON_SUPPORT_TRACING 1
#endif

typedef union TokenData
{
    int64 i64;
    double dbl;
    const char *string;
} TokenData;

typedef struct StringBucket
{
    char *string;
    struct StringBucket *next;
} StringBucket;

typedef struct Context
{
    int isfail;
    int out_of_memory;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    int error_count;
    ErrorList *errors;
    Preprocessor *preprocessor;
    StringBucket *string_hashtable[256];
    const char *usertypes[512];  // !!! FIXME: dynamic allocation
    int usertype_count;
} Context;


// Convenience functions for allocators...

static inline void out_of_memory(Context *ctx)
{
    ctx->isfail = ctx->out_of_memory = 1;
} // out_of_memory

static inline void *Malloc(Context *ctx, const size_t len)
{
    void *retval = ctx->malloc((int) len, ctx->malloc_data);
    if (retval == NULL)
        out_of_memory(ctx);
    return retval;
} // Malloc

static inline char *StrDup(Context *ctx, const char *str)
{
    char *retval = (char *) Malloc(ctx, strlen(str) + 1);
    if (retval != NULL)
        strcpy(retval, str);
    return retval;
} // StrDup

static inline void Free(Context *ctx, void *ptr)
{
    if (ptr != NULL)  // check for NULL in case of dumb free() impl.
        ctx->free(ptr, ctx->malloc_data);
} // Free

typedef enum Operator
{
    OP_START_RANGE_UNARY,
    OP_POSTINCREMENT,
    OP_POSTDECREMENT,
    OP_PREINCREMENT,
    OP_PREDECREMENT,
    OP_NEGATE,
    OP_COMPLEMENT,
    OP_NOT,
    OP_END_RANGE_UNARY,

    OP_START_RANGE_BINARY,
    OP_DEREF_ARRAY,
    OP_CALLFUNC,
    OP_DEREF_STRUCT,
    OP_COMMA,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MODULO,
    OP_ADD,
    OP_SUBTRACT,
    OP_LSHIFT,
    OP_RSHIFT,
    OP_LESSTHAN,
    OP_GREATERTHAN,
    OP_LESSTHANOREQUAL,
    OP_GREATERTHANOREQUAL,
    OP_EQUAL,
    OP_NOTEQUAL,
    OP_BINARYAND,
    OP_BINARYXOR,
    OP_BINARYOR,
    OP_LOGICALAND,
    OP_LOGICALOR,
    OP_ASSIGN,
    OP_MULASSIGN,
    OP_DIVASSIGN,
    OP_MODASSIGN,
    OP_ADDASSIGN,
    OP_SUBASSIGN,
    OP_LSHIFTASSIGN,
    OP_RSHIFTASSIGN,
    OP_ANDASSIGN,
    OP_XORASSIGN,
    OP_ORASSIGN,
    OP_END_RANGE_BINARY,

    OP_START_RANGE_TERNARY,
    OP_CONDITIONAL,
    OP_END_RANGE_TERNARY,

    OP_START_RANGE_DATA,
    OP_IDENTIFIER,
    OP_INT_LITERAL,
    OP_FLOAT_LITERAL,
    OP_STRING_LITERAL,
    OP_END_RANGE_DATA,
} Operator;

static inline int operator_is_unary(const Operator op)
{
    return ((op > OP_START_RANGE_UNARY) && (op < OP_END_RANGE_UNARY));
} // operator_is_unary

static inline int operator_is_binary(const Operator op)
{
    return ((op > OP_START_RANGE_BINARY) && (op < OP_END_RANGE_BINARY));
} // operator_is_binary

static inline int operator_is_ternary(const Operator op)
{
    return ((op > OP_START_RANGE_TERNARY) && (op < OP_END_RANGE_TERNARY));
} // operator_is_ternary


typedef struct Expression
{
    Operator op;  // operator
} Expression;

#define NEW_EXPR(cls) \
    cls *retval = Malloc(ctx, sizeof (cls)); \
    if (retval == NULL) { return NULL; }

typedef struct ExpressionUnary
{
    Operator op;  // operator
    Expression *operand;
} ExpressionUnary;

typedef struct ExpressionBinary
{
    Operator op;  // operator
    Expression *left;
    Expression *right;
} ExpressionBinary;

typedef struct ExpressionTernary
{
    Operator op;  // operator
    Expression *left;
    Expression *center;
    Expression *right;
} ExpressionTernary;

typedef struct ExpressionIdentifier
{
    Operator op;  // Always OP_IDENTIFIER
    const char *identifier;
} ExpressionIdentifier;

typedef struct ExpressionIntLiteral
{
    Operator op;  // Always OP_INT_LITERAL
    int64 value;
} ExpressionIntLiteral;

typedef struct ExpressionFloatLiteral
{
    Operator op;  // Always OP_FLOAT_LITERAL
    double value;
} ExpressionFloatLiteral;

typedef struct ExpressionStringLiteral
{
    Operator op;  // Always OP_STRING_LITERAL
    const char *string;
} ExpressionStringLiteral;

static Expression *new_unary_expr(Context *ctx, const Operator op,
                                  Expression *operand)
{
    NEW_EXPR(ExpressionUnary);
    assert(operator_is_unary(op));
    retval->op = op;
    retval->operand = operand;
    return (Expression *) retval;
} // new_unary_expr

static Expression *new_binary_expr(Context *ctx, const Operator op,
                                   Expression *left, Expression *right)
{
    NEW_EXPR(ExpressionBinary);
    assert(operator_is_binary(op));
    retval->op = op;
    retval->left = left;
    retval->right = right;
    return (Expression *) retval;
} // new_binary_expr

static Expression *new_ternary_expr(Context *ctx, const Operator op,
                                    Expression *left, Expression *center,
                                    Expression *right)
{
    NEW_EXPR(ExpressionTernary);
    assert(operator_is_ternary(op));
    retval->op = op;
    retval->left = left;
    retval->center = center;
    retval->right = right;
    return (Expression *) retval;
} // new_ternary_expr

static Expression *new_identifier_expr(Context *ctx, const char *string)
{
    NEW_EXPR(ExpressionIdentifier);
    retval->op = OP_IDENTIFIER;
    retval->identifier = string;  // cached; don't copy string.
    return (Expression *) retval;
} // new_identifier_expr

static inline int64 strtoi64(const char *str, unsigned int len)
{
    int64 retval = 0;
    int64 mult = 1;
    int i = 0;

    while ((len) && (*str == ' '))
    {
        str++;
        len--;
    } // while

    if ((len) && (*str == '-'))
    {
        mult = -1;
        str++;
        len--;
    } // if

    while (i < len)
    {
        const char ch = str[i];
        if ((ch < '0') || (ch > '9'))
            break;
        i++;
    } // while

    while (--i >= 0)
    {
        const char ch = str[i];
        retval += ((int64) (ch - '0')) * mult;
        mult *= 10;
    } // while

    return retval;
} // strtoi64

static Expression *new_literal_int_expr(Context *ctx, const int64 value)
{
    NEW_EXPR(ExpressionIntLiteral);
    retval->op = OP_INT_LITERAL;
    retval->value = value;
    return (Expression *) retval;
} // new_literal_int_expr

static inline double strtodouble(const char *_str, unsigned int len)
{
    // !!! FIXME: laziness prevails.
    char *str = (char *) alloca(len+1);
    memcpy(str, _str, len);
    str[len] = '\0';
    return strtod(str, NULL);
} // strtodouble

static Expression *new_literal_float_expr(Context *ctx, const double dbl)
{
    NEW_EXPR(ExpressionFloatLiteral);
    retval->op = OP_FLOAT_LITERAL;
    retval->value = dbl;
    return (Expression *) retval;
} // new_literal_float_expr

static Expression *new_literal_string_expr(Context *ctx, const char *string)
{
    NEW_EXPR(ExpressionStringLiteral);
    retval->op = OP_STRING_LITERAL;
    retval->string = string;  // cached; don't copy string.
    return (Expression *) retval;
} // new_string_literal_expr


static void add_usertype(Context *ctx, const char *sym)
{
    // !!! FIXME: error if this is a reserved keyword.
    // !!! FIXME: dynamic allocation
    assert(ctx->usertype_count < STATICARRAYLEN(ctx->usertypes));
    ctx->usertypes[ctx->usertype_count++] = sym;
    ctx->usertype_count++;
} // add_usertype

static int is_usertype(const Context *ctx, const char *token,
                       const unsigned int tokenlen)
{
    // !!! FIXME: dynamic allocation
    // !!! FIXME: should probably redesign this anyhow.
    int i;
    for (i = 0; i < ctx->usertype_count; i++)
    {
        const char *type = ctx->usertypes[i];
        if (strncmp(type, token, tokenlen) == 0)
            return type[tokenlen] == '\0';
    } // for

    return 0;
} // is_usertype


// This is where the actual parsing happens. It's Lemon-generated!
#define __MOJOSHADER_HLSL_COMPILER__ 1
#include "mojoshader_parser_hlsl.h"


static void free_expr(Context *ctx, Expression *expr)
{
    if (operator_is_unary(expr->op))
    {
        const ExpressionUnary *unary = (const ExpressionUnary *) expr;
        free_expr(ctx, unary->operand);
    } // if
    else if (operator_is_binary(expr->op))
    {
        const ExpressionBinary *binary = (const ExpressionBinary *) expr;
        free_expr(ctx, binary->left);
        free_expr(ctx, binary->right);
    } // else if
    else if (operator_is_ternary(expr->op))
    {
        const ExpressionTernary *ternary = (const ExpressionTernary *) expr;
        free_expr(ctx, ternary->left);
        free_expr(ctx, ternary->center);
        free_expr(ctx, ternary->right);
    } // else if

    // don't need to free extra fields in other types at the moment.

    Free(ctx, expr);
} // free_expr

// !!! FIXME: sort of cut-and-paste from the preprocessor...

// this is djb's xor hashing function.
static inline uint32 hash_string_djbxor(const char *str, unsigned int len)
{
    register uint32 hash = 5381;
    while (len--)
        hash = ((hash << 5) + hash) ^ *(str++);
    return hash;
} // hash_string_djbxor

static inline uint8 hash_string(const char *str, const unsigned int len)
{
    return (uint8) hash_string_djbxor(str, len);
} // hash_string

static const char *cache_string(Context *ctx, const char *str,
                                const unsigned int len)
{
    const uint8 hash = hash_string(str, len);
    StringBucket *bucket = ctx->string_hashtable[hash];
    StringBucket *prev = NULL;
    while (bucket)
    {
        const char *bstr = bucket->string;
        if ((strncmp(bstr, str, len) == 0) && (bstr[len] == 0))
        {
            // Matched! Move this to the front of the list.
            if (prev != NULL)
            {
                assert(prev->next == bucket);
                prev->next = bucket->next;
                bucket->next = ctx->string_hashtable[hash];
                ctx->string_hashtable[hash] = bucket;
            } // if
            return bstr; // already cached
        } // if
        prev = bucket;
        bucket = bucket->next;
    } // while

    // no match, add to the table.
    bucket = (StringBucket *) Malloc(ctx, sizeof (StringBucket));
    if (bucket == NULL)
        return NULL;
    bucket->string = (char *) Malloc(ctx, len + 1);
    if (bucket->string == NULL)
    {
        Free(ctx, bucket);
        return NULL;
    } // if
    memcpy(bucket->string, str, len);
    bucket->string[len] = '\0';
    bucket->next = ctx->string_hashtable[hash];
    ctx->string_hashtable[hash] = bucket;
    return bucket->string;
} // cache_string

static void free_string_cache(Context *ctx)
{
    size_t i;
    for (i = 0; i < STATICARRAYLEN(ctx->string_hashtable); i++)
    {
        StringBucket *bucket = ctx->string_hashtable[i];
        ctx->string_hashtable[i] = NULL;
        while (bucket)
        {
            StringBucket *next = bucket->next;
            Free(ctx, bucket->string);
            Free(ctx, bucket);
            bucket = next;
        } // while
    } // for
} // free_string_cache


// This does not check correctness (POSITIONT993842 passes, etc).
static int is_semantic(const Context *ctx, const char *token,
                       const unsigned int tokenlen)
{
    static const char *names[] = {
        "BINORMAL", "BLENDINDICES", "BLENDWEIGHT",
        "COLOR", "NORMAL", "POSITION", "POSITIONT", "PSIZE", "TANGENT",
        "TEXCOORD", "FOG", "TESSFACTOR", "TEXCOORD", "VFACE", "VPOS",
        "DEPTH", NULL
    };

    // !!! FIXME: DX10 has SV_* ("System Value Semantics").
    const char **i;
    for (i = names; *i; i++)
    {
        const char *name = *i;
        const size_t namelen = strlen(name);
        if (tokenlen < namelen)
            continue;
        else if (memcmp(token, name, namelen) != 0)
            continue;

        for (name += namelen; *name; name++)
        {
            if ((*name < '0') || (*name > '9'))
                break;
        } // for

        if (*name == '\0')
            return 1;
    } // for

    return 0;
} // is_semantic


static int convert_to_lemon_token(const Context *ctx, const char *token,
                                  unsigned int tokenlen, const Token tokenval)
{
    switch (tokenval)
    {
        case ((Token) ','): return TOKEN_HLSL_COMMA;
        case ((Token) '='): return TOKEN_HLSL_ASSIGN;
        case ((Token) TOKEN_ADDASSIGN): return TOKEN_HLSL_ADDASSIGN;
        case ((Token) TOKEN_SUBASSIGN): return TOKEN_HLSL_SUBASSIGN;
        case ((Token) TOKEN_MULTASSIGN): return TOKEN_HLSL_MULASSIGN;
        case ((Token) TOKEN_DIVASSIGN): return TOKEN_HLSL_DIVASSIGN;
        case ((Token) TOKEN_MODASSIGN): return TOKEN_HLSL_MODASSIGN;
        case ((Token) TOKEN_LSHIFTASSIGN): return TOKEN_HLSL_LSHIFTASSIGN;
        case ((Token) TOKEN_RSHIFTASSIGN): return TOKEN_HLSL_RSHIFTASSIGN;
        case ((Token) TOKEN_ANDASSIGN): return TOKEN_HLSL_ANDASSIGN;
        case ((Token) TOKEN_ORASSIGN): return TOKEN_HLSL_ORASSIGN;
        case ((Token) TOKEN_XORASSIGN): return TOKEN_HLSL_XORASSIGN;
        case ((Token) '?'): return TOKEN_HLSL_QUESTION;
        case ((Token) TOKEN_OROR): return TOKEN_HLSL_OROR;
        case ((Token) TOKEN_ANDAND): return TOKEN_HLSL_ANDAND;
        case ((Token) '|'): return TOKEN_HLSL_OR;
        case ((Token) '^'): return TOKEN_HLSL_XOR;
        case ((Token) '&'): return TOKEN_HLSL_AND;
        case ((Token) TOKEN_EQL): return TOKEN_HLSL_EQL;
        case ((Token) TOKEN_NEQ): return TOKEN_HLSL_NEQ;
        case ((Token) '<'): return TOKEN_HLSL_LT;
        case ((Token) TOKEN_LEQ): return TOKEN_HLSL_LEQ;
        case ((Token) '>'): return TOKEN_HLSL_GT;
        case ((Token) TOKEN_GEQ): return TOKEN_HLSL_GEQ;
        case ((Token) TOKEN_LSHIFT): return TOKEN_HLSL_LSHIFT;
        case ((Token) TOKEN_RSHIFT): return TOKEN_HLSL_RSHIFT;
        case ((Token) '+'): return TOKEN_HLSL_PLUS;
        case ((Token) '-'): return TOKEN_HLSL_MINUS;
        case ((Token) '*'): return TOKEN_HLSL_STAR;
        case ((Token) '/'): return TOKEN_HLSL_SLASH;
        case ((Token) '%'): return TOKEN_HLSL_PERCENT;
        case ((Token) '!'): return TOKEN_HLSL_EXCLAMATION;
        case ((Token) '~'): return TOKEN_HLSL_COMPLEMENT;
        case ((Token) TOKEN_DECREMENT): return TOKEN_HLSL_MINUSMINUS;
        case ((Token) TOKEN_INCREMENT): return TOKEN_HLSL_PLUSPLUS;
        case ((Token) '.'): return TOKEN_HLSL_DOT;
        case ((Token) '['): return TOKEN_HLSL_LBRACKET;
        case ((Token) ']'): return TOKEN_HLSL_RBRACKET;
        case ((Token) '('): return TOKEN_HLSL_LPAREN;
        case ((Token) ')'): return TOKEN_HLSL_RPAREN;
        case ((Token) TOKEN_INT_LITERAL): return TOKEN_HLSL_INT_CONSTANT;
        case ((Token) TOKEN_FLOAT_LITERAL): return TOKEN_HLSL_FLOAT_CONSTANT;
        case ((Token) TOKEN_STRING_LITERAL): return TOKEN_HLSL_STRING_LITERAL;
        case ((Token) ':'): return TOKEN_HLSL_COLON;
        case ((Token) ';'): return TOKEN_HLSL_SEMICOLON;
        case ((Token) '{'): return TOKEN_HLSL_LBRACE;
        case ((Token) '}'): return TOKEN_HLSL_RBRACE;

        case ((Token) TOKEN_IDENTIFIER):
            #define tokencmp(t) ((tokenlen == strlen(t)) && (memcmp(token, t, tokenlen) == 0))
            //case ((Token) ''): return TOKEN_HLSL_TYPECAST
            //if (tokencmp("")) return TOKEN_HLSL_TYPE_NAME
            //if (tokencmp("...")) return TOKEN_HLSL_ELIPSIS
            if (tokencmp("else")) return TOKEN_HLSL_ELSE;
            if (tokencmp("inline")) return TOKEN_HLSL_INLINE;
            if (tokencmp("void")) return TOKEN_HLSL_VOID;
            if (tokencmp("in")) return TOKEN_HLSL_IN;
            if (tokencmp("inout")) return TOKEN_HLSL_INOUT;
            if (tokencmp("out")) return TOKEN_HLSL_OUT;
            if (tokencmp("uniform")) return TOKEN_HLSL_UNIFORM;
            if (tokencmp("linear")) return TOKEN_HLSL_LINEAR;
            if (tokencmp("centroid")) return TOKEN_HLSL_CENTROID;
            if (tokencmp("nointerpolation")) return TOKEN_HLSL_NOINTERPOLATION;
            if (tokencmp("noperspective")) return TOKEN_HLSL_NOPERSPECTIVE;
            if (tokencmp("sample")) return TOKEN_HLSL_SAMPLE;
            if (tokencmp("struct")) return TOKEN_HLSL_STRUCT;
            if (tokencmp("typedef")) return TOKEN_HLSL_TYPEDEF;
            if (tokencmp("const")) return TOKEN_HLSL_CONST;
            if (tokencmp("packoffset")) return TOKEN_HLSL_PACKOFFSET;
            if (tokencmp("register")) return TOKEN_HLSL_REGISTER;
            if (tokencmp("extern")) return TOKEN_HLSL_EXTERN;
            if (tokencmp("shared")) return TOKEN_HLSL_SHARED;
            if (tokencmp("static")) return TOKEN_HLSL_STATIC;
            if (tokencmp("volatile")) return TOKEN_HLSL_VOLATILE;
            if (tokencmp("row_major")) return TOKEN_HLSL_ROWMAJOR;
            if (tokencmp("column_major")) return TOKEN_HLSL_COLUMNMAJOR;
            if (tokencmp("bool")) return TOKEN_HLSL_BOOL;
            if (tokencmp("int")) return TOKEN_HLSL_INT;
            if (tokencmp("uint")) return TOKEN_HLSL_UINT;
            if (tokencmp("half")) return TOKEN_HLSL_HALF;
            if (tokencmp("float")) return TOKEN_HLSL_FLOAT;
            if (tokencmp("double")) return TOKEN_HLSL_DOUBLE;
            if (tokencmp("string")) return TOKEN_HLSL_STRING;
            if (tokencmp("snorm")) return TOKEN_HLSL_SNORM;
            if (tokencmp("unorm")) return TOKEN_HLSL_UNORM;
            if (tokencmp("buffer")) return TOKEN_HLSL_BUFFER;
            if (tokencmp("vector")) return TOKEN_HLSL_VECTOR;
            if (tokencmp("bool1")) return TOKEN_HLSL_BOOL1;
            if (tokencmp("bool2")) return TOKEN_HLSL_BOOL2;
            if (tokencmp("bool3")) return TOKEN_HLSL_BOOL3;
            if (tokencmp("bool4")) return TOKEN_HLSL_BOOL4;
            if (tokencmp("int1")) return TOKEN_HLSL_INT1;
            if (tokencmp("int2")) return TOKEN_HLSL_INT2;
            if (tokencmp("int3")) return TOKEN_HLSL_INT3;
            if (tokencmp("int4")) return TOKEN_HLSL_INT4;
            if (tokencmp("uint1")) return TOKEN_HLSL_UINT1;
            if (tokencmp("uint2")) return TOKEN_HLSL_UINT2;
            if (tokencmp("uint3")) return TOKEN_HLSL_UINT3;
            if (tokencmp("uint4")) return TOKEN_HLSL_UINT4;
            if (tokencmp("half1")) return TOKEN_HLSL_HALF1;
            if (tokencmp("half2")) return TOKEN_HLSL_HALF2;
            if (tokencmp("half3")) return TOKEN_HLSL_HALF3;
            if (tokencmp("half4")) return TOKEN_HLSL_HALF4;
            if (tokencmp("float1")) return TOKEN_HLSL_FLOAT1;
            if (tokencmp("float2")) return TOKEN_HLSL_FLOAT2;
            if (tokencmp("float3")) return TOKEN_HLSL_FLOAT3;
            if (tokencmp("float4")) return TOKEN_HLSL_FLOAT4;
            if (tokencmp("double1")) return TOKEN_HLSL_DOUBLE1;
            if (tokencmp("double2")) return TOKEN_HLSL_DOUBLE2;
            if (tokencmp("double3")) return TOKEN_HLSL_DOUBLE3;
            if (tokencmp("double4")) return TOKEN_HLSL_DOUBLE4;
            if (tokencmp("matrix")) return TOKEN_HLSL_MATRIX;
            if (tokencmp("bool1x1")) return TOKEN_HLSL_BOOL1X1;
            if (tokencmp("bool1x2")) return TOKEN_HLSL_BOOL1X2;
            if (tokencmp("bool1x3")) return TOKEN_HLSL_BOOL1X3;
            if (tokencmp("bool1x4")) return TOKEN_HLSL_BOOL1X4;
            if (tokencmp("bool2x1")) return TOKEN_HLSL_BOOL2X1;
            if (tokencmp("bool2x2")) return TOKEN_HLSL_BOOL2X2;
            if (tokencmp("bool2x3")) return TOKEN_HLSL_BOOL2X3;
            if (tokencmp("bool2x4")) return TOKEN_HLSL_BOOL2X4;
            if (tokencmp("bool3x1")) return TOKEN_HLSL_BOOL3X1;
            if (tokencmp("bool3x2")) return TOKEN_HLSL_BOOL3X2;
            if (tokencmp("bool3x3")) return TOKEN_HLSL_BOOL3X3;
            if (tokencmp("bool3x4")) return TOKEN_HLSL_BOOL3X4;
            if (tokencmp("bool4x1")) return TOKEN_HLSL_BOOL4X1;
            if (tokencmp("bool4x2")) return TOKEN_HLSL_BOOL4X2;
            if (tokencmp("bool4x3")) return TOKEN_HLSL_BOOL4X3;
            if (tokencmp("bool4x4")) return TOKEN_HLSL_BOOL4X4;
            if (tokencmp("int1x1")) return TOKEN_HLSL_INT1X1;
            if (tokencmp("int1x2")) return TOKEN_HLSL_INT1X2;
            if (tokencmp("int1x3")) return TOKEN_HLSL_INT1X3;
            if (tokencmp("int1x4")) return TOKEN_HLSL_INT1X4;
            if (tokencmp("int2x1")) return TOKEN_HLSL_INT2X1;
            if (tokencmp("int2x2")) return TOKEN_HLSL_INT2X2;
            if (tokencmp("int2x3")) return TOKEN_HLSL_INT2X3;
            if (tokencmp("int2x4")) return TOKEN_HLSL_INT2X4;
            if (tokencmp("int3x1")) return TOKEN_HLSL_INT3X1;
            if (tokencmp("int3x2")) return TOKEN_HLSL_INT3X2;
            if (tokencmp("int3x3")) return TOKEN_HLSL_INT3X3;
            if (tokencmp("int3x4")) return TOKEN_HLSL_INT3X4;
            if (tokencmp("int4x1")) return TOKEN_HLSL_INT4X1;
            if (tokencmp("int4x2")) return TOKEN_HLSL_INT4X2;
            if (tokencmp("int4x3")) return TOKEN_HLSL_INT4X3;
            if (tokencmp("int4x4")) return TOKEN_HLSL_INT4X4;
            if (tokencmp("uint1x1")) return TOKEN_HLSL_UINT1X1;
            if (tokencmp("uint1x2")) return TOKEN_HLSL_UINT1X2;
            if (tokencmp("uint1x3")) return TOKEN_HLSL_UINT1X3;
            if (tokencmp("uint1x4")) return TOKEN_HLSL_UINT1X4;
            if (tokencmp("uint2x1")) return TOKEN_HLSL_UINT2X1;
            if (tokencmp("uint2x2")) return TOKEN_HLSL_UINT2X2;
            if (tokencmp("uint2x3")) return TOKEN_HLSL_UINT2X3;
            if (tokencmp("uint2x4")) return TOKEN_HLSL_UINT2X4;
            if (tokencmp("uint3x1")) return TOKEN_HLSL_UINT3X1;
            if (tokencmp("uint3x2")) return TOKEN_HLSL_UINT3X2;
            if (tokencmp("uint3x3")) return TOKEN_HLSL_UINT3X3;
            if (tokencmp("uint3x4")) return TOKEN_HLSL_UINT3X4;
            if (tokencmp("uint4x1")) return TOKEN_HLSL_UINT4X1;
            if (tokencmp("uint4x2")) return TOKEN_HLSL_UINT4X2;
            if (tokencmp("uint4x3")) return TOKEN_HLSL_UINT4X3;
            if (tokencmp("uint4x4")) return TOKEN_HLSL_UINT4X4;
            if (tokencmp("half1x1")) return TOKEN_HLSL_HALF1X1;
            if (tokencmp("half1x2")) return TOKEN_HLSL_HALF1X2;
            if (tokencmp("half1x3")) return TOKEN_HLSL_HALF1X3;
            if (tokencmp("half1x4")) return TOKEN_HLSL_HALF1X4;
            if (tokencmp("half2x1")) return TOKEN_HLSL_HALF2X1;
            if (tokencmp("half2x2")) return TOKEN_HLSL_HALF2X2;
            if (tokencmp("half2x3")) return TOKEN_HLSL_HALF2X3;
            if (tokencmp("half2x4")) return TOKEN_HLSL_HALF2X4;
            if (tokencmp("half3x1")) return TOKEN_HLSL_HALF3X1;
            if (tokencmp("half3x2")) return TOKEN_HLSL_HALF3X2;
            if (tokencmp("half3x3")) return TOKEN_HLSL_HALF3X3;
            if (tokencmp("half3x4")) return TOKEN_HLSL_HALF3X4;
            if (tokencmp("half4x1")) return TOKEN_HLSL_HALF4X1;
            if (tokencmp("half4x2")) return TOKEN_HLSL_HALF4X2;
            if (tokencmp("half4x3")) return TOKEN_HLSL_HALF4X3;
            if (tokencmp("half4x4")) return TOKEN_HLSL_HALF4X4;
            if (tokencmp("float1x1")) return TOKEN_HLSL_FLOAT1X1;
            if (tokencmp("float1x2")) return TOKEN_HLSL_FLOAT1X2;
            if (tokencmp("float1x3")) return TOKEN_HLSL_FLOAT1X3;
            if (tokencmp("float1x4")) return TOKEN_HLSL_FLOAT1X4;
            if (tokencmp("float2x1")) return TOKEN_HLSL_FLOAT2X1;
            if (tokencmp("float2x2")) return TOKEN_HLSL_FLOAT2X2;
            if (tokencmp("float2x3")) return TOKEN_HLSL_FLOAT2X3;
            if (tokencmp("float2x4")) return TOKEN_HLSL_FLOAT2X4;
            if (tokencmp("float3x1")) return TOKEN_HLSL_FLOAT3X1;
            if (tokencmp("float3x2")) return TOKEN_HLSL_FLOAT3X2;
            if (tokencmp("float3x3")) return TOKEN_HLSL_FLOAT3X3;
            if (tokencmp("float3x4")) return TOKEN_HLSL_FLOAT3X4;
            if (tokencmp("float4x1")) return TOKEN_HLSL_FLOAT4X1;
            if (tokencmp("float4x2")) return TOKEN_HLSL_FLOAT4X2;
            if (tokencmp("float4x3")) return TOKEN_HLSL_FLOAT4X3;
            if (tokencmp("float4x4")) return TOKEN_HLSL_FLOAT4X4;
            if (tokencmp("double1x1")) return TOKEN_HLSL_DOUBLE1X1;
            if (tokencmp("double1x2")) return TOKEN_HLSL_DOUBLE1X2;
            if (tokencmp("double1x3")) return TOKEN_HLSL_DOUBLE1X3;
            if (tokencmp("double1x4")) return TOKEN_HLSL_DOUBLE1X4;
            if (tokencmp("double2x1")) return TOKEN_HLSL_DOUBLE2X1;
            if (tokencmp("double2x2")) return TOKEN_HLSL_DOUBLE2X2;
            if (tokencmp("double2x3")) return TOKEN_HLSL_DOUBLE2X3;
            if (tokencmp("double2x4")) return TOKEN_HLSL_DOUBLE2X4;
            if (tokencmp("double3x1")) return TOKEN_HLSL_DOUBLE3X1;
            if (tokencmp("double3x2")) return TOKEN_HLSL_DOUBLE3X2;
            if (tokencmp("double3x3")) return TOKEN_HLSL_DOUBLE3X3;
            if (tokencmp("double3x4")) return TOKEN_HLSL_DOUBLE3X4;
            if (tokencmp("double4x1")) return TOKEN_HLSL_DOUBLE4X1;
            if (tokencmp("double4x2")) return TOKEN_HLSL_DOUBLE4X2;
            if (tokencmp("double4x3")) return TOKEN_HLSL_DOUBLE4X3;
            if (tokencmp("double4x4")) return TOKEN_HLSL_DOUBLE4X4;
            if (tokencmp("break")) return TOKEN_HLSL_BREAK;
            if (tokencmp("continue")) return TOKEN_HLSL_CONTINUE;
            if (tokencmp("discard")) return TOKEN_HLSL_DISCARD;
            if (tokencmp("return")) return TOKEN_HLSL_RETURN;
            if (tokencmp("while")) return TOKEN_HLSL_WHILE;
            if (tokencmp("for")) return TOKEN_HLSL_FOR;
            if (tokencmp("unroll")) return TOKEN_HLSL_UNROLL;
            if (tokencmp("loop")) return TOKEN_HLSL_LOOP;
            if (tokencmp("do")) return TOKEN_HLSL_DO;
            if (tokencmp("if")) return TOKEN_HLSL_IF;
            if (tokencmp("branch")) return TOKEN_HLSL_BRANCH;
            if (tokencmp("flatten")) return TOKEN_HLSL_FLATTEN;
            if (tokencmp("switch")) return TOKEN_HLSL_SWITCH;
            if (tokencmp("forcecase")) return TOKEN_HLSL_FORCECASE;
            if (tokencmp("call")) return TOKEN_HLSL_CALL;
            if (tokencmp("case")) return TOKEN_HLSL_CASE;
            if (tokencmp("default")) return TOKEN_HLSL_DEFAULT;
            if (tokencmp("sampler")) return TOKEN_HLSL_SAMPLER;
            if (tokencmp("sampler1D")) return TOKEN_HLSL_SAMPLER1D;
            if (tokencmp("sampler2D")) return TOKEN_HLSL_SAMPLER2D;
            if (tokencmp("sampler3D")) return TOKEN_HLSL_SAMPLER3D;
            if (tokencmp("samplerCUBE")) return TOKEN_HLSL_SAMPLERCUBE;
            if (tokencmp("sampler_state")) return TOKEN_HLSL_SAMPLER_STATE;
            if (tokencmp("SamplerState")) return TOKEN_HLSL_SAMPLERSTATE;
            if (tokencmp("SamplerComparisonState")) return TOKEN_HLSL_SAMPLERCOMPARISONSTATE;
            if (tokencmp("isolate")) return TOKEN_HLSL_ISOLATE;
            if (tokencmp("maxInstructionCount")) return TOKEN_HLSL_MAXINSTRUCTIONCOUNT;
            if (tokencmp("noExpressionOptimizations")) return TOKEN_HLSL_NOEXPRESSIONOPTIMIZATIONS;
            if (tokencmp("unused")) return TOKEN_HLSL_UNUSED;
            if (tokencmp("xps")) return TOKEN_HLSL_XPS;

            #undef tokencmp

            if (is_semantic(ctx, token, tokenlen))
                return TOKEN_HLSL_SEMANTIC;
            else if (is_usertype(ctx, token, tokenlen))
                return TOKEN_HLSL_USERTYPE;
            return TOKEN_HLSL_IDENTIFIER;

        case TOKEN_EOI: return 0;
        case TOKEN_BAD_CHARS: printf("bad chars from lexer\n"); return 0;
        case TOKEN_PREPROCESSING_ERROR: printf("error from lexer\n"); return 0;
        default: assert(0 && "unexpected token from lexer\n"); return 0;
    } // switch

    return 0;
} // convert_to_lemon_token


void MOJOSHADER_compile(const char *filename,
                        const char *source, unsigned int sourcelen,
                        const MOJOSHADER_preprocessorDefine *defines,
                        unsigned int define_count,
                        MOJOSHADER_includeOpen include_open,
                        MOJOSHADER_includeClose include_close,
                        MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    Context ctx;
    TokenData data;
    unsigned int tokenlen;
    Token tokenval;
    const char *token;
    int lemon_token;

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    memset(&ctx, '\0', sizeof (Context));
    ctx.malloc = m;
    ctx.free = f;
    ctx.malloc_data = d;
    ctx.preprocessor = preprocessor_start(filename, source, sourcelen,
                                           include_open, include_close,
                                           defines, define_count, 0, m, f, d);

    void *pParser = ParseHLSLAlloc(m, d);

    #if DEBUG_COMPILER_PARSER
    ParseHLSLTrace(stdout, "COMPILER: ");
    #endif

    do {
        token = preprocessor_nexttoken(ctx.preprocessor, &tokenlen, &tokenval);
        lemon_token = convert_to_lemon_token(&ctx, token, tokenlen, tokenval);
        switch (lemon_token)
        {
            case TOKEN_HLSL_INT_CONSTANT:
                data.i64 = strtoi64(token, tokenlen);
                break;

            case TOKEN_HLSL_FLOAT_CONSTANT:
                data.dbl = strtodouble(token, tokenlen);
                break;

            case TOKEN_HLSL_SEMANTIC:
            case TOKEN_HLSL_USERTYPE:
            case TOKEN_HLSL_STRING_LITERAL:
            case TOKEN_HLSL_IDENTIFIER:
                data.string = cache_string(&ctx, token, tokenlen);
                break;

            default:
                data.i64 = 0;
                break;
        } // switch

        ParseHLSL(pParser, lemon_token, data, &ctx);
    } while ((!ctx.isfail) && (tokenval != TOKEN_EOI));

    ParseHLSLFree(pParser, f, d);
    // !!! FIXME: destruct (ctx) here.
    free_string_cache(&ctx);
} // MOJOSHADER_compile

// end of mojoshader_compiler.c ...

