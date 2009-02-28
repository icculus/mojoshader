#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef struct Context
{
    Preprocessor *preprocessor;
    const char *token;      // assembler token!
    unsigned int tokenlen;  // assembler token!
    Token tokenval;         // assembler token!
} Context;

#define __MOJOSHADER_HLSL_COMPILER__ 1
#include "mojoshader_parser_hlsl.h"
#include "mojoshader_parser_hlsl.c"

static int ConvertToLemonToken(const Context *ctx)
{
    switch (ctx->tokenval)
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
        case ((Token) TOKEN_INT_LITERAL): return TOKEN_HLSL_CONSTANT;
        case ((Token) TOKEN_FLOAT_LITERAL): return TOKEN_HLSL_CONSTANT;
        case ((Token) TOKEN_STRING_LITERAL): return TOKEN_HLSL_STRING_LITERAL;
        case ((Token) ':'): return TOKEN_HLSL_COLON;
        case ((Token) ';'): return TOKEN_HLSL_SEMICOLON;
        case ((Token) '{'): return TOKEN_HLSL_LBRACE;
        case ((Token) '}'): return TOKEN_HLSL_RBRACE;

        case ((Token) TOKEN_IDENTIFIER):
            #define tokencmp(t) ((ctx->tokenlen == strlen(t)) && (memcmp(ctx->token, t, ctx->tokenlen) == 0))
            //case ((Token) ''): return TOKEN_HLSL_TYPECAST
            //if (tokencmp("")) return TOKEN_HLSL_TYPE_NAME
            //if (tokencmp("...")) return TOKEN_HLSL_ELIPSIS
            if (tokencmp("else")) return TOKEN_HLSL_ELSE;
            if (tokencmp("sizeof")) return TOKEN_HLSL_SIZEOF;
            if (tokencmp("typedef")) return TOKEN_HLSL_TYPEDEF;
            if (tokencmp("extern")) return TOKEN_HLSL_EXTERN;
            if (tokencmp("static")) return TOKEN_HLSL_STATIC;
            if (tokencmp("auto")) return TOKEN_HLSL_AUTO;
            if (tokencmp("register")) return TOKEN_HLSL_REGISTER;
            if (tokencmp("char")) return TOKEN_HLSL_CHAR;
            if (tokencmp("short")) return TOKEN_HLSL_SHORT;
            if (tokencmp("int")) return TOKEN_HLSL_INT;
            if (tokencmp("long")) return TOKEN_HLSL_LONG;
            if (tokencmp("signed")) return TOKEN_HLSL_SIGNED;
            if (tokencmp("unsigned")) return TOKEN_HLSL_UNSIGNED;
            if (tokencmp("float")) return TOKEN_HLSL_FLOAT;
            if (tokencmp("double")) return TOKEN_HLSL_DOUBLE;
            if (tokencmp("const")) return TOKEN_HLSL_CONST;
            if (tokencmp("volatile")) return TOKEN_HLSL_VOLATILE;
            if (tokencmp("void")) return TOKEN_HLSL_VOID;
            if (tokencmp("struct")) return TOKEN_HLSL_STRUCT;
            if (tokencmp("union")) return TOKEN_HLSL_UNION;
            if (tokencmp("case")) return TOKEN_HLSL_CASE;
            if (tokencmp("default")) return TOKEN_HLSL_DEFAULT;
            if (tokencmp("discard")) return TOKEN_HLSL_DISCARD;
            if (tokencmp("if")) return TOKEN_HLSL_IF;
            if (tokencmp("switch")) return TOKEN_HLSL_SWITCH;
            if (tokencmp("while")) return TOKEN_HLSL_WHILE;
            if (tokencmp("do")) return TOKEN_HLSL_DO;
            if (tokencmp("for")) return TOKEN_HLSL_FOR;
            if (tokencmp("continue")) return TOKEN_HLSL_CONTINUE;
            if (tokencmp("break")) return TOKEN_HLSL_BREAK;
            if (tokencmp("return")) return TOKEN_HLSL_RETURN;
            #undef tokencmp
            return TOKEN_HLSL_IDENTIFIER;

        case TOKEN_EOI: return 0;
        case TOKEN_BAD_CHARS: printf("bad chars from lexer\n"); return 0;
        case TOKEN_PREPROCESSING_ERROR: printf("error from lexer\n"); return 0;
        default: assert(0 && "unexpected token from lexer\n"); return 0;
    } // switch

    return 0;
}


void MOJOSHADER_compile(const char *filename,
                             const char *source, unsigned int sourcelen,
                             const MOJOSHADER_preprocessorDefine *defines,
                             unsigned int define_count,
                             MOJOSHADER_includeOpen include_open,
                             MOJOSHADER_includeClose include_close,
                             MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    Context ctx;
    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;
    
    ctx.preprocessor = preprocessor_start(filename, source, sourcelen,
                                           include_open, include_close,
                                           defines, define_count, 0, m, f, d);

    void *pParser = ParseHLSLAlloc(m, d);
    ParseHLSLTrace(stdout, "TRACE: ");

    do {
        ctx.token = preprocessor_nexttoken(ctx.preprocessor,
                                                &ctx.tokenlen,
                                                &ctx.tokenval);
        ParseHLSL(pParser, ConvertToLemonToken(&ctx), 0, 0);
    } while (ctx.tokenval != TOKEN_EOI);
    ParseHLSLFree(pParser, f, d);
}

