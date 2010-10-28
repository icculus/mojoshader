/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "mojoshader.h"

#ifndef _WIN32
#define stricmp(a,b) strcasecmp(a,b)
#endif

static const char **include_paths = NULL;
static unsigned int include_path_count = 0;

#define MOJOSHADER_DEBUG_MALLOC 0

#if MOJOSHADER_DEBUG_MALLOC
static void *Malloc(int len, void *d)
{
    void *ptr = malloc(len + sizeof (int));
    int *store = (int *) ptr;
    printf("malloc() %d bytes (%p)\n", len, ptr);
    if (ptr == NULL) return NULL;
    *store = len;
    return (void *) (store + 1);
} // Malloc


static void Free(void *_ptr, void *d)
{
    int *ptr = (((int *) _ptr) - 1);
    int len = *ptr;
    printf("free() %d bytes (%p)\n", len, ptr);
    free(ptr);
} // Free
#else
#define Malloc NULL
#define Free NULL
#endif


static void fail(const char *err)
{
    printf("%s.\n", err);
    exit(1);
} // fail

static void print_unroll_attr(const int unroll)
{
    if (unroll == 0)
        printf("[loop] ");
    else if (unroll < 0)
        printf("[unroll] ");
    else
        printf("[unroll(%d)] ", unroll);
} // print_unroll_attr

// !!! FIXME: this screws up on order of operations.
static void print_ast(const int substmt, const void *_ast)
{
    const MOJOSHADER_astNode *ast = (const MOJOSHADER_astNode *) _ast;
    const char *nl = substmt ? "" : "\n";
    int typeint = 0;
    static int indent = 0;
    int isblock = 0;
    int i;

    // These _HAVE_ to be in the same order as MOJOSHADER_astNodeType!
    static const char *binary[] =
    {
        ",", "*", "/", "%", "+", "-", "<<", ">>", "<", ">", "<=", ">=", "==",
        "!=", "&", "^", "|", "&&", "||", "=", "*=", "/=", "%=", "+=", "-=",
        "<<=", ">>=", "&=", "^=", "|="
    };

    static const char *pre_unary[] = { "++", "--", "-", "~", "!" };
    static const char *post_unary[] = { "++", "--" };
    static const char *simple_stmt[] = { "", "break", "continue", "discard" };
    static const char *inpmod[] = { "", "in ", "out ", "in out ", "uniform " };
    static const char *fnstorage[] = { "", "inline " };

    static const char *interpmod[] = {
        "", " linear", " centroid", " nointerpolation",
        " noperspective", " sample"
    };

    if (!ast) return;

    typeint = (int) ast->ast.type;

    #define DO_INDENT do { \
        if (!substmt) { for (i = 0; i < indent; i++) printf("    "); } \
    } while (0)

    switch (ast->ast.type)
    {
        case MOJOSHADER_AST_OP_PREINCREMENT:
        case MOJOSHADER_AST_OP_PREDECREMENT:
        case MOJOSHADER_AST_OP_NEGATE:
        case MOJOSHADER_AST_OP_COMPLEMENT:
        case MOJOSHADER_AST_OP_NOT:
            printf("%s", pre_unary[(typeint-MOJOSHADER_AST_OP_START_RANGE_UNARY)-1]);
            print_ast(0, ast->unary.operand);
            break;

        case MOJOSHADER_AST_OP_POSTINCREMENT:
        case MOJOSHADER_AST_OP_POSTDECREMENT:
            print_ast(0, ast->unary.operand);
            printf("%s", post_unary[typeint-MOJOSHADER_AST_OP_POSTINCREMENT]);
            break;

        case MOJOSHADER_AST_OP_MULTIPLY:
        case MOJOSHADER_AST_OP_DIVIDE:
        case MOJOSHADER_AST_OP_MODULO:
        case MOJOSHADER_AST_OP_ADD:
        case MOJOSHADER_AST_OP_SUBTRACT:
        case MOJOSHADER_AST_OP_LSHIFT:
        case MOJOSHADER_AST_OP_RSHIFT:
        case MOJOSHADER_AST_OP_LESSTHAN:
        case MOJOSHADER_AST_OP_GREATERTHAN:
        case MOJOSHADER_AST_OP_LESSTHANOREQUAL:
        case MOJOSHADER_AST_OP_GREATERTHANOREQUAL:
        case MOJOSHADER_AST_OP_EQUAL:
        case MOJOSHADER_AST_OP_NOTEQUAL:
        case MOJOSHADER_AST_OP_BINARYAND:
        case MOJOSHADER_AST_OP_BINARYXOR:
        case MOJOSHADER_AST_OP_BINARYOR:
        case MOJOSHADER_AST_OP_LOGICALAND:
        case MOJOSHADER_AST_OP_LOGICALOR:
        case MOJOSHADER_AST_OP_ASSIGN:
        case MOJOSHADER_AST_OP_MULASSIGN:
        case MOJOSHADER_AST_OP_DIVASSIGN:
        case MOJOSHADER_AST_OP_MODASSIGN:
        case MOJOSHADER_AST_OP_ADDASSIGN:
        case MOJOSHADER_AST_OP_SUBASSIGN:
        case MOJOSHADER_AST_OP_LSHIFTASSIGN:
        case MOJOSHADER_AST_OP_RSHIFTASSIGN:
        case MOJOSHADER_AST_OP_ANDASSIGN:
        case MOJOSHADER_AST_OP_XORASSIGN:
        case MOJOSHADER_AST_OP_ORASSIGN:
            printf(" ");  // then fall through! (no space before the comma).
        case MOJOSHADER_AST_OP_COMMA:
            print_ast(0, ast->binary.left);
            printf("%s ", binary[
                (typeint - MOJOSHADER_AST_OP_START_RANGE_BINARY) - 1]);
            print_ast(0, ast->binary.right);
            break;

        case MOJOSHADER_AST_OP_DEREF_ARRAY:
            print_ast(0, ast->binary.left);
            printf("[");
            print_ast(0, ast->binary.right);
            printf("]");
            break;

        case MOJOSHADER_AST_OP_DEREF_STRUCT:
            print_ast(0, ast->derefstruct.identifier);
            printf(".");
            printf("%s", ast->derefstruct.member);
            break;

        case MOJOSHADER_AST_OP_CONDITIONAL:
            print_ast(0, ast->ternary.left);
            printf(" ? ");
            print_ast(0, ast->ternary.center);
            printf(" : ");
            print_ast(0, ast->ternary.right);
            break;

        case MOJOSHADER_AST_OP_IDENTIFIER:
            printf("%s", ast->identifier.identifier);
            break;

        case MOJOSHADER_AST_OP_INT_LITERAL:
            printf("%d", ast->intliteral.value);
            break;

        case MOJOSHADER_AST_OP_FLOAT_LITERAL:
        {
            const float f = ast->floatliteral.value;
            const long long flr = (long long) f;
            if (((float) flr) == f)
                printf("%lld.0", flr);
            else
                printf("%.16g", f);
            break;
        } // case

        case MOJOSHADER_AST_OP_STRING_LITERAL:
            printf("\"%s\"", ast->stringliteral.string);
            break;

        case MOJOSHADER_AST_OP_BOOLEAN_LITERAL:
            printf("%s", ast->boolliteral.value ? "true" : "false");
            break;

        case MOJOSHADER_AST_ARGUMENTS:
            print_ast(0, ast->arguments.argument);
            if (ast->arguments.next != NULL)
            {
                printf(", ");
                print_ast(0, ast->arguments.next);
            } // if
            break;

        case MOJOSHADER_AST_OP_CALLFUNC:
            print_ast(0, ast->callfunc.identifier);
            printf("(");
            print_ast(0, ast->callfunc.args);
            printf(")");
            break;

        case MOJOSHADER_AST_OP_CONSTRUCTOR:
            printf("%s(", ast->constructor.datatype);
            print_ast(0, ast->constructor.args);
            printf(")");
            break;

        case MOJOSHADER_AST_OP_CAST:
            printf("(%s) (", ast->cast.datatype);
            print_ast(0, ast->cast.operand);
            printf(")");
            break;

        case MOJOSHADER_AST_STATEMENT_EXPRESSION:
            DO_INDENT;
            print_ast(0, ast->exprstmt.expr);  // !!! FIXME: This is named badly...
            printf(";%s", nl);
            print_ast(0, ast->exprstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_IF:
            DO_INDENT;
            printf("if (");
            print_ast(0, ast->ifstmt.expr);
            printf(")\n");
            isblock = ast->ifstmt.statement->ast.type == MOJOSHADER_AST_STATEMENT_BLOCK;
            if (!isblock) indent++;
            print_ast(0, ast->ifstmt.statement);
            if (!isblock) indent--;
            print_ast(0, ast->ifstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_TYPEDEF:
            DO_INDENT;
            print_ast(1, ast->typedefstmt.type_info);
            printf("%s", nl);
            print_ast(0, ast->typedefstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_SWITCH:
            DO_INDENT;
            switch ( ast->switchstmt.attributes )
            {
                case MOJOSHADER_AST_SWITCHATTR_NONE: break;
                case MOJOSHADER_AST_SWITCHATTR_FLATTEN: printf("[flatten] "); break;
                case MOJOSHADER_AST_SWITCHATTR_BRANCH: printf("[branch] "); break;
                case MOJOSHADER_AST_SWITCHATTR_FORCECASE: printf("[forcecase] "); break;
                case MOJOSHADER_AST_SWITCHATTR_CALL: printf("[call] "); break;
            } // switch

            printf("switch (");
            print_ast(0, ast->switchstmt.expr);
            printf(")\n");
            DO_INDENT;
            printf("{\n");
            indent++;
            print_ast(0, ast->switchstmt.cases);
            indent--;
            printf("\n");
            DO_INDENT;
            printf("}\n");
            print_ast(0, ast->switchstmt.next);
            break;

        case MOJOSHADER_AST_SWITCH_CASE:
            DO_INDENT;
            printf("case ");
            print_ast(0, ast->cases.expr);
            printf(":\n");
            isblock = ast->cases.statement->ast.type == MOJOSHADER_AST_STATEMENT_BLOCK;
            if (!isblock) indent++;
            print_ast(0, ast->cases.statement);
            if (!isblock) indent--;
            print_ast(0, ast->cases.next);
            break;

        case MOJOSHADER_AST_STATEMENT_STRUCT:
            DO_INDENT;
            print_ast(0, ast->structstmt.struct_info);
            printf(";%s%s", nl, nl);  // always space these out.
            print_ast(0, ast->structstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_VARDECL:
            DO_INDENT;
            print_ast(1, ast->vardeclstmt.declaration);
            printf(";%s", nl);
            print_ast(0, ast->vardeclstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_BLOCK:
            DO_INDENT;
            printf("{\n");
            indent++;
            print_ast(0, ast->blockstmt.statements);
            indent--;
            DO_INDENT;
            printf("}\n");
            print_ast(0, ast->blockstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_FOR:
            DO_INDENT;
            print_unroll_attr(ast->forstmt.unroll);
            printf("for (");
            print_ast(1, ast->forstmt.var_decl);
            if (ast->forstmt.initializer != NULL)
            {
                printf(" = ");
                print_ast(1, ast->forstmt.initializer);
            } // if
            printf("; ");
            print_ast(1, ast->forstmt.looptest);
            printf("; ");
            print_ast(1, ast->forstmt.counter);

            printf(")\n");
            isblock = ast->forstmt.statement->ast.type == MOJOSHADER_AST_STATEMENT_BLOCK;
            if (!isblock) indent++;
            print_ast(0, ast->forstmt.statement);
            if (!isblock) indent--;

            print_ast(0, ast->forstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_DO:
            DO_INDENT;
            print_unroll_attr(ast->dostmt.unroll);
            printf("do\n");

            isblock = ast->dostmt.statement->ast.type == MOJOSHADER_AST_STATEMENT_BLOCK;
            if (!isblock) indent++;
            print_ast(0, ast->dostmt.statement);
            if (!isblock) indent--;

            DO_INDENT;
            printf("while (");
            print_ast(0, ast->dostmt.expr);
            printf(");\n");

            print_ast(0, ast->dostmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_WHILE:
            DO_INDENT;
            print_unroll_attr(ast->whilestmt.unroll);
            printf("while (");
            print_ast(0, ast->whilestmt.expr);
            printf(")\n");

            isblock = ast->whilestmt.statement->ast.type == MOJOSHADER_AST_STATEMENT_BLOCK;
            if (!isblock) indent++;
            print_ast(0, ast->whilestmt.statement);
            if (!isblock) indent--;

            print_ast(0, ast->whilestmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_RETURN:
            DO_INDENT;
            printf("return");
            if (ast->returnstmt.expr)
            {
                printf(" ");
                print_ast(0, ast->returnstmt.expr);
            } // if
            printf(";%s", nl);
            print_ast(0, ast->returnstmt.next);
            break;

        case MOJOSHADER_AST_STATEMENT_EMPTY:
        case MOJOSHADER_AST_STATEMENT_BREAK:
        case MOJOSHADER_AST_STATEMENT_CONTINUE:
        case MOJOSHADER_AST_STATEMENT_DISCARD:
            DO_INDENT;
            printf("%s;%s",
                simple_stmt[(typeint-MOJOSHADER_AST_STATEMENT_START_RANGE)-1],
                nl);
            print_ast(0, ast->stmt.next);
            break;

        case MOJOSHADER_AST_COMPUNIT_FUNCTION:
            DO_INDENT;
            print_ast(0, ast->funcunit.declaration);
            if (ast->funcunit.definition == NULL)
                printf(";%s", nl);
            else
            {
                printf("%s", nl);
                print_ast(0, ast->funcunit.definition);
                printf("%s", nl);
            } // else
            print_ast(0, ast->funcunit.next);
            break;

        case MOJOSHADER_AST_COMPUNIT_TYPEDEF:
            DO_INDENT;
            print_ast(0, ast->typedefunit.type_info);
            printf("%s", nl);
            print_ast(0, ast->typedefunit.next);
            break;

        case MOJOSHADER_AST_COMPUNIT_STRUCT:
            DO_INDENT;
            print_ast(0, ast->structunit.struct_info);
            printf(";%s%s", nl, nl);  // always space these out.
            print_ast(0, ast->structunit.next);
            break;

        case MOJOSHADER_AST_COMPUNIT_VARIABLE:
            DO_INDENT;
            print_ast(1, ast->varunit.declaration);
            printf(";%s", nl);
            if (ast->varunit.next &&
                ast->varunit.next->ast.type!=MOJOSHADER_AST_COMPUNIT_VARIABLE)
            {
                printf("%s", nl);  // group vars together, and space out other things.
            } // if
            print_ast(0, ast->varunit.next);
            break;

        case MOJOSHADER_AST_SCALAR_OR_ARRAY:
            printf("%s", ast->soa.identifier);
            if (ast->soa.isarray)
            {
                printf("[");
                print_ast(0, ast->soa.dimension);
                printf("]");
            } // if
            break;

        case MOJOSHADER_AST_TYPEDEF:
            DO_INDENT;
            printf("typedef %s%s ",
                   ast->typdef.isconst ? "const " : "", ast->typdef.datatype);
            print_ast(0, ast->typdef.details);
            printf(";%s", nl);
            break;

        case MOJOSHADER_AST_FUNCTION_PARAMS:
            printf("%s", inpmod[(int) ast->params.input_modifier]);
            printf("%s %s", ast->params.datatype, ast->params.identifier);
            if (ast->params.semantic)
                printf(" : %s", ast->params.semantic);
            printf("%s", interpmod[(int) ast->params.interpolation_modifier]);

            if (ast->params.initializer)
            {
                printf(" = ");
                print_ast(0, ast->params.initializer);
            } // if

            if (ast->params.next)
            {
                printf(", ");
                print_ast(0, ast->params.next);
            } // if
            break;

        case MOJOSHADER_AST_FUNCTION_SIGNATURE:
            printf("%s", fnstorage[(int) ast->funcsig.storage_class]);
            printf("%s %s(",
                    ast->funcsig.datatype ? ast->funcsig.datatype : "void",
                    ast->funcsig.identifier);
            print_ast(0, ast->funcsig.params);
            printf(")");
            if (ast->funcsig.semantic)
                printf(" : %s", ast->funcsig.semantic);
            break;

        case MOJOSHADER_AST_STRUCT_DECLARATION:
            printf("struct %s\n", ast->structdecl.name);
            DO_INDENT;
            printf("{\n");
            indent++;
            print_ast(0, ast->structdecl.members);
            indent--;
            DO_INDENT;
            printf("}");
            break;

        case MOJOSHADER_AST_STRUCT_MEMBER:
            DO_INDENT;
            printf("%s", interpmod[(int)ast->structmembers.interpolation_mod]);
            printf("%s ", ast->structmembers.datatype);
            print_ast(0, ast->structmembers.details);
            if (ast->structmembers.semantic)
                printf(" : %s", ast->structmembers.semantic);
            printf(";%s", nl);
            print_ast(0, ast->structmembers.next);
            break;

        case MOJOSHADER_AST_VARIABLE_DECLARATION:
            DO_INDENT;
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_EXTERN)
                printf("extern ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_NOINTERPOLATION)
                printf("nointerpolation ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_SHARED)
                printf("shared");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_STATIC)
                printf("static ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_UNIFORM)
                printf("uniform ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_VOLATILE)
                printf("nointerpolation ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_CONST)
                printf("const ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_ROWMAJOR)
                printf("rowmajor ");
            if (ast->vardecl.attributes & MOJOSHADER_AST_VARATTR_COLUMNMAJOR)
                printf("columnmajor ");

            if (ast->vardecl.datatype)
                printf("%s", ast->vardecl.datatype);
            else
                print_ast(0, ast->vardecl.anonymous_datatype);
            printf(" ");
            print_ast(0, ast->vardecl.details);
            if (ast->vardecl.semantic)
                printf(" : %s", ast->vardecl.semantic);
            if (ast->vardecl.annotations)
            {
                printf(" ");
                print_ast(0, ast->vardecl.annotations);
            } // if
            if (ast->vardecl.initializer != NULL)
            {
                printf(" = ");
                print_ast(0, ast->vardecl.initializer);
            } // if
            print_ast(0, ast->vardecl.lowlevel);

            if (ast->vardecl.next == NULL)
                printf("%s", nl);
            else
            {
                const int attr = ast->vardecl.next->attributes;
                printf(", ");
                ast->vardecl.next->attributes = 0;
                print_ast(1, ast->vardecl.next);
                ast->vardecl.next->attributes = attr;
            } // if
            break;

        case MOJOSHADER_AST_PACK_OFFSET:
            printf(" : packoffset(%s%s%s)", ast->packoffset.ident1,
                    ast->packoffset.ident2 ? "." : "",
                    ast->packoffset.ident2 ? ast->packoffset.ident2 : "");
            break;

        case MOJOSHADER_AST_VARIABLE_LOWLEVEL:
            print_ast(0, ast->varlowlevel.packoffset);
            if (ast->varlowlevel.register_name)
                printf(" : register(%s)", ast->varlowlevel.register_name);
            break;

        case MOJOSHADER_AST_ANNOTATION:
        {
            const MOJOSHADER_astAnnotations *a = &ast->annotations;
            printf("<");
            while (a)
            {
                printf(" %s ", a->datatype);
                if (a->initializer != NULL)
                {
                    printf(" = ");
                    print_ast(0, a->initializer);
                } // if
                if (a->next)
                    printf(",");
                a = a->next;
            } // while
            printf(" >");
            break;
        } // case

        default:
            assert(0 && "unexpected type");
            break;
    } // switch

    #undef DO_INDENT
} // print_ast


static int open_include(MOJOSHADER_includeType inctype, const char *fname,
                        const char *parent, const char **outdata,
                        unsigned int *outbytes, MOJOSHADER_malloc m,
                        MOJOSHADER_free f, void *d)
{
    int i;
    for (i = 0; i < include_path_count; i++)
    {
        const char *path = include_paths[i];
        const size_t len = strlen(path) + strlen(fname) + 2;
        char *buf = (char *) m(len, d);
        if (buf == NULL)
            return 0;

        snprintf(buf, len, "%s/%s", path, fname);
        FILE *io = fopen(buf, "rb");
        f(buf, d);
        if (io == NULL)
            continue;

        if (fseek(io, 0, SEEK_END) != -1)
        {
            const long fsize = ftell(io);
            if ((fsize == -1) || (fseek(io, 0, SEEK_SET) == -1))
            {
                fclose(io);
                return 0;
            } // if

            char *data = (char *) m(fsize, d);
            if (data == NULL)
            {
                fclose(io);
                return 0;
            } // if

            if (fread(data, fsize, 1, io) != 1)
            {
                f(data, d);
                fclose(io);
                return 0;
            } // if

            fclose(io);
            *outdata = data;
            *outbytes = (unsigned int) fsize;
            return 1;
        } // if
    } // for

    return 0;
} // open_include


static void close_include(const char *data, MOJOSHADER_malloc m,
                          MOJOSHADER_free f, void *d)
{
    f((void *) data, d);
} // close_include


static int preprocess(const char *fname, const char *buf, int len,
                      const char *outfile,
                      const MOJOSHADER_preprocessorDefine *defs,
                      unsigned int defcount, FILE *io)
{
    const MOJOSHADER_preprocessData *pd;
    int retval = 0;

    pd = MOJOSHADER_preprocess(fname, buf, len, defs, defcount, open_include,
                               close_include, Malloc, Free, NULL);

    if (pd->error_count > 0)
    {
        int i;
        for (i = 0; i < pd->error_count; i++)
        {
            fprintf(stderr, "%s:%d: ERROR: %s\n",
                    pd->errors[i].filename ? pd->errors[i].filename : "???",
                    pd->errors[i].error_position,
                    pd->errors[i].error);
        } // for
    } // if
    else
    {
        if (pd->output != NULL)
        {
            const int len = pd->output_len;
            if ((len) && (fwrite(pd->output, len, 1, io) != 1))
                printf(" ... fwrite('%s') failed.\n", outfile);
            else if ((outfile != NULL) && (fclose(io) == EOF))
                printf(" ... fclose('%s') failed.\n", outfile);
            else
                retval = 1;
        } // if
    } // else
    MOJOSHADER_freePreprocessData(pd);

    return retval;
} // preprocess


static int assemble(const char *fname, const char *buf, int len,
                    const char *outfile,
                    const MOJOSHADER_preprocessorDefine *defs,
                    unsigned int defcount, FILE *io)
{
    const MOJOSHADER_parseData *pd;
    int retval = 0;

    pd = MOJOSHADER_assemble(fname, buf, len, NULL, 0, NULL, 0,
                             defs, defcount, open_include, close_include,
                             Malloc, Free, NULL);

    if (pd->error_count > 0)
    {
        int i;
        for (i = 0; i < pd->error_count; i++)
        {
            fprintf(stderr, "%s:%d: ERROR: %s\n",
                    pd->errors[i].filename ? pd->errors[i].filename : "???",
                    pd->errors[i].error_position,
                    pd->errors[i].error);
        } // for
    } // if
    else
    {
        if (pd->output != NULL)
        {
            const int len = pd->output_len;
            if ((len) && (fwrite(pd->output, len, 1, io) != 1))
                printf(" ... fwrite('%s') failed.\n", outfile);
            else if ((outfile != NULL) && (fclose(io) == EOF))
                printf(" ... fclose('%s') failed.\n", outfile);
            else
                retval = 1;
        } // if
    } // else
    MOJOSHADER_freeParseData(pd);

    return retval;
} // assemble

static int ast(const char *fname, const char *buf, int len,
               const char *outfile, const MOJOSHADER_preprocessorDefine *defs,
               unsigned int defcount, FILE *io)
{
    // !!! FIXME: write me.
    //const MOJOSHADER_parseData *pd;
    //int retval = 0;

    MOJOSHADER_parseAst(MOJOSHADER_SRC_PROFILE_HLSL_PS_1_1,  // !!! FIXME
                        fname, buf, len, defs, defcount,
                        open_include, close_include, Malloc, Free, NULL);
    return 1;
} // ast

static int compile(const char *fname, const char *buf, int len,
                    const char *outfile,
                    const MOJOSHADER_preprocessorDefine *defs,
                    unsigned int defcount, FILE *io)
{
    // !!! FIXME: write me.
    //const MOJOSHADER_parseData *pd;
    //int retval = 0;

    MOJOSHADER_compile(MOJOSHADER_SRC_PROFILE_HLSL_PS_1_1,  // !!! FIXME
                        fname, buf, len, defs, defcount,
                             open_include, close_include,
                             Malloc, Free, NULL);
    return 1;
} // compile

typedef enum
{
    ACTION_UNKNOWN,
    ACTION_VERSION,
    ACTION_PREPROCESS,
    ACTION_ASSEMBLE,
    ACTION_AST,
    ACTION_COMPILE,
} Action;


int main(int argc, char **argv)
{
    Action action = ACTION_UNKNOWN;
    int retval = 1;
    const char *infile = NULL;
    const char *outfile = NULL;
    int i;

    MOJOSHADER_preprocessorDefine *defs = NULL;
    unsigned int defcount = 0;

    include_paths = (const char **) malloc(sizeof (char *));
    include_paths[0] = ".";
    include_path_count = 1;

    // !!! FIXME: clean this up.
    for (i = 1; i < argc; i++)
    {
        const char *arg = argv[i];

        if (strcmp(arg, "-P") == 0)
        {
            if ((action != ACTION_UNKNOWN) && (action != ACTION_PREPROCESS))
                fail("Multiple actions specified");
            action = ACTION_PREPROCESS;
        } // if

        else if (strcmp(arg, "-A") == 0)
        {
            if ((action != ACTION_UNKNOWN) && (action != ACTION_ASSEMBLE))
                fail("Multiple actions specified");
            action = ACTION_ASSEMBLE;
        } // else if

        else if (strcmp(arg, "-T") == 0)
        {
            if ((action != ACTION_UNKNOWN) && (action != ACTION_AST))
                fail("Multiple actions specified");
            action = ACTION_AST;
        } // else if

        else if (strcmp(arg, "-C") == 0)
        {
            if ((action != ACTION_UNKNOWN) && (action != ACTION_COMPILE))
                fail("Multiple actions specified");
            action = ACTION_COMPILE;
        } // else if

        else if ((strcmp(arg, "-V") == 0) || (strcmp(arg, "--version") == 0))
        {
            if ((action != ACTION_UNKNOWN) && (action != ACTION_VERSION))
                fail("Multiple actions specified");
            action = ACTION_VERSION;
        } // else if

        else if (strcmp(arg, "-o") == 0)
        {
            if (outfile != NULL)
                fail("multiple output files specified");

            arg = argv[++i];
            if (arg == NULL)
                fail("no filename after '-o'");
            outfile = arg;
        } // if

        else if (strcmp(arg, "-I") == 0)
        {
            arg = argv[++i];
            if (arg == NULL)
                fail("no path after '-I'");

            include_paths = (const char **) realloc(include_paths,
                       (include_path_count+1) * sizeof (char *));
            include_paths[include_path_count] = arg;
            include_path_count++;
        } // if

        else if (strncmp(arg, "-D", 2) == 0)
        {
            arg += 2;
            char *ident = strdup(arg);
            char *ptr = strchr(ident, '=');
            const char *val = "";
            if (ptr)
            {
                *ptr = '\0';
                val = ptr+1;
            } // if

            defs = (MOJOSHADER_preprocessorDefine *) realloc(defs,
                       (defcount+1) * sizeof (MOJOSHADER_preprocessorDefine));
            defs[defcount].identifier = ident;
            defs[defcount].definition = val;
            defcount++;
        } // else if

        else
        {
            if (infile != NULL)
                fail("multiple input files specified");
            infile = arg;
        } // else
    } // for

    if (action == ACTION_UNKNOWN)
        action = ACTION_ASSEMBLE;

    if (action == ACTION_VERSION)
    {
        printf("mojoshader-compiler, changeset %s\n", MOJOSHADER_CHANGESET);
        return 0;
    } // if

    if (infile == NULL)
        fail("no input file specified");

    FILE *io = fopen(infile, "rb");
    if (io == NULL)
        fail("failed to open input file");

    fseek(io, 0, SEEK_END);
    long fsize = ftell(io);
    fseek(io, 0, SEEK_SET);
    if (fsize == -1)
        fsize = 1000000;
    char *buf = (char *) malloc(fsize);
    const int rc = fread(buf, 1, fsize, io);
    fclose(io);
    if (rc == EOF)
        fail("failed to read input file");

    FILE *outio = outfile ? fopen(outfile, "wb") : stdout;
    if (outio == NULL)
        fail("failed to open output file");


    if (action == ACTION_PREPROCESS)
        retval = (!preprocess(infile, buf, rc, outfile, defs, defcount, outio));
    else if (action == ACTION_ASSEMBLE)
        retval = (!assemble(infile, buf, rc, outfile, defs, defcount, outio));
    else if (action == ACTION_AST)
        retval = (!ast(infile, buf, rc, outfile, defs, defcount, outio));
    else if (action == ACTION_COMPILE)
        retval = (!compile(infile, buf, rc, outfile, defs, defcount, outio));

    if ((retval != 0) && (outfile != NULL))
        remove(outfile);

    free(buf);

    for (i = 0; i < defcount; i++)
        free((void *) defs[i].identifier);
    free(defs);

    free(include_paths);

    return retval;
} // main

// end of mojoshader-compiler.c ...

