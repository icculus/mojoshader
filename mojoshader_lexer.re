/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// This was originally based on examples/pp-c.re from re2c: http://re2c.org/
//   re2c is public domain code.
//
// You build mojoshader_lexer_preprocessor.c from the .re file with re2c...
// re2c -is -o mojoshader_lexer_preprocessor.c mojoshader_lexer_preprocessor.re
//
// Changes to the lexer are done to the .re file, not the C code!
//
// Please note that this isn't a perfect C lexer, since it is used for both
//  HLSL and shader assembly language, and follows the quirks of Microsoft's
//  tools.

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

typedef unsigned char uchar;

#define RET(t) do { update_state(s, cursor, token); return t; } while (0)
#define YYCTYPE uchar
#define YYCURSOR cursor
#define YYLIMIT limit
#define YYMARKER s->lexer_marker
#define YYFILL(n) { if ((n) == 1) { RET(TOKEN_EOI); } }

static void update_state(IncludeState *s, const uchar *cur, const uchar *tok)
{
    s->bytes_left -= (unsigned int) (cur - ((const uchar *) s->source));
    s->source = (const char *) cur;
    s->token = (const char *) tok;
} // update_state

Token preprocessor_internal_lexer(IncludeState *s)
{
    const uchar *cursor = (const uchar *) s->source;
    const uchar *token;
    const uchar *limit = cursor + s->bytes_left;
    int saw_newline = 0;

scanner_loop:
    token = cursor;

    if (YYLIMIT == YYCURSOR)
        RET(TOKEN_EOI);

/*!re2c
    any = [\000-\377];
    O = [0-7];
    D = [0-9];
    L = [a-zA-Z_];
    H = [a-fA-F0-9];
    E = [Ee] [+-]? D+;
    FS = [fFlL];
    IS = [uUlL]*;
    ESC = [\\] ([abfnrtv?'"\\] | "x" H+ | O+);
    PP = "#" [ \t]*;
    NEWLINE = "\r\n" | "\r" | "\n";
    WHITESPACE = [ \t\v\f]+;
*/

/*!re2c
    "/*"            { goto multilinecomment; }
    "//"            { goto singlelinecomment; }

    L (L|D)*        { RET(TOKEN_IDENTIFIER); }
    
    ("0" [xX] H+ IS?) | ("0" D+ IS?) | (D+ IS?) |
    (['] (ESC|any\[\n\\'])* ['])
                    { RET(TOKEN_INT_LITERAL); }
    
    (D+ E FS?) | (D* "." D+ E? FS?) | (D+ "." D* E? FS?)
                    { RET(TOKEN_FLOAT_LITERAL); }
    
    (["] (ESC|any\[\n\\"])* ["])
                    { RET(TOKEN_STRING_LITERAL); }
    
    ">>="           { RET(TOKEN_RSHIFTASSIGN); }
    "<<="           { RET(TOKEN_LSHIFTASSIGN); }
    "+="            { RET(TOKEN_ADDASSIGN); }
    "-="            { RET(TOKEN_SUBASSIGN); }
    "*="            { RET(TOKEN_MULTASSIGN); }
    "/="            { RET(TOKEN_DIVASSIGN); }
    "%="            { RET(TOKEN_MODASSIGN); }
    "^="            { RET(TOKEN_XORASSIGN); }
    "&="            { RET(TOKEN_ANDASSIGN); }
    "|="            { RET(TOKEN_ORASSIGN); }
    "++"            { RET(TOKEN_INCREMENT); }
    "--"            { RET(TOKEN_DECREMENT); }
    ">>"            { RET(TOKEN_RSHIFT); }
    "<<"            { RET(TOKEN_LSHIFT); }
    "&&"            { RET(TOKEN_ANDAND); }
    "||"            { RET(TOKEN_OROR); }
    "<="            { RET(TOKEN_LEQ); }
    ">="            { RET(TOKEN_GEQ); }
    "=="            { RET(TOKEN_EQL); }
    "!="            { RET(TOKEN_NEQ); }
    "##"            { RET(TOKEN_HASHHASH); }
    "("             { RET('('); }
    ")"             { RET(')'); }
    "["             { RET('['); }
    "]"             { RET(']'); }
    "."             { RET('.'); }
    ","             { RET(','); }
    "&"             { RET('&'); }
    "!"             { RET('!'); }
    "~"             { RET('~'); }
    "-"             { RET('-'); }
    "+"             { RET('+'); }
    "*"             { RET('*'); }
    "/"             { RET('/'); }
    "%"             { RET('%'); }
    "<"             { RET('<'); }
    ">"             { RET('>'); }
    "^"             { RET('^'); }
    "|"             { RET('|'); }
    ":"             { RET(':'); }
    ";"             { RET(';'); }
    "{"             { RET('{'); }
    "}"             { RET('}'); }
    "="             { RET('='); }
    "?"             { RET('?'); }
    "\\"            { RET('\\'); }
    "#"             { RET('#'); }

    PP "include"    { RET(TOKEN_PP_INCLUDE); }
    PP "line"       { RET(TOKEN_PP_LINE); }
    PP "define"     { RET(TOKEN_PP_DEFINE); }
    PP "undef"      { RET(TOKEN_PP_UNDEF); }
    PP "if"         { RET(TOKEN_PP_IF); }
    PP "ifdef"      { RET(TOKEN_PP_IFDEF); }
    PP "ifndef"     { RET(TOKEN_PP_IFNDEF); }
    PP "else"       { RET(TOKEN_PP_ELSE); }
    PP "elif"       { RET(TOKEN_PP_ELIF); }
    PP "endif"      { RET(TOKEN_PP_ENDIF); }
    PP "error"      { RET(TOKEN_PP_ERROR); }

    WHITESPACE      { goto scanner_loop; }
    NEWLINE         { s->line++; RET('\n'); }
    any             { printf("bad char\n"); goto scanner_loop; }
*/

multilinecomment:
    if (YYLIMIT == YYCURSOR)
        RET(TOKEN_PP_INCOMPLETE_COMMENT);
// The "*\/" is just to avoid screwing up text editor syntax highlighting.
/*!re2c
    "*\/"           {
                        if (saw_newline)
                            RET('\n');
                        goto scanner_loop;
                    }
    NEWLINE         {
                        s->line++;
                        token = cursor-1;
                        saw_newline = 1;
                        goto multilinecomment;
                    }
    any             { goto multilinecomment; }
*/

singlelinecomment:
    if (YYLIMIT == YYCURSOR)
        RET(TOKEN_EOI);
/*!re2c
    NEWLINE         { s->line++; token = cursor-1; RET('\n'); }
    any             { goto singlelinecomment; }
*/

// !!! FIXME
/*
bad_chars:
    if (YYLIMIT == YYCURSOR)
        RET(TOKEN_BAD_TOKEN);
*/

/*!re2c
    NEWLINE         { s->line++; goto scanner_loop; }
    WHITESPACE      { goto scanner_loop; }
    any             { goto singlelinecomment; }
*/

    assert(0 && "Shouldn't hit this code");
    RET(TOKEN_UNKNOWN);
} // preprocessor_internal_lexer

// end of mojoshader_lexer_preprocessor.re (or .c) ...

