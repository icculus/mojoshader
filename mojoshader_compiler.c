/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

// !!! FIXME: this needs to be split into separate source files:
// !!! FIXME:  parse, AST, IR, etc. The problem is we need to deal with the
// !!! FIXME:  "Context" struct being passed around everywhere.

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"

#if DEBUG_COMPILER_PARSER
#define LEMON_SUPPORT_TRACING 1
#endif

// !!! FIXME: I'd like to lose this. It's really inefficient. Just keep a
// !!! FIXME:  (tail) on these list structures instead?
#define REVERSE_LINKED_LIST(typ, head) { \
    if ((head) && (head->next)) { \
        typ *tmp = NULL; \
        typ *tmp1 = NULL; \
        while (head != NULL) { \
            tmp = head; \
            head = head->next; \
            tmp->next = tmp1; \
            tmp1 = tmp; \
        } \
        head = tmp; \
    } \
}

static inline int operator_is_unary(const MOJOSHADER_astNodeType op)
{
    return ( (op > MOJOSHADER_AST_OP_START_RANGE_UNARY) &&
             (op < MOJOSHADER_AST_OP_END_RANGE_UNARY) );
} // operator_is_unary

static inline int operator_is_binary(const MOJOSHADER_astNodeType op)
{
    return ( (op > MOJOSHADER_AST_OP_START_RANGE_BINARY) &&
             (op < MOJOSHADER_AST_OP_END_RANGE_BINARY) );
} // operator_is_binary

static inline int operator_is_ternary(const MOJOSHADER_astNodeType op)
{
    return ( (op > MOJOSHADER_AST_OP_START_RANGE_TERNARY) &&
             (op < MOJOSHADER_AST_OP_END_RANGE_TERNARY) );
} // operator_is_ternary


typedef union TokenData
{
    int64 i64;
    double dbl;
    const char *string;
} TokenData;


// This tracks data types and variables, and notes when they enter/leave scope.

typedef struct SymbolScope
{
    const char *symbol;
    const char *datatype;
    struct SymbolScope *next;
} SymbolScope;

typedef struct SymbolMap
{
    HashTable *hash;
    SymbolScope *scope;
} SymbolMap;


// Compile state, passed around all over the place.

typedef struct Context
{
    int isfail;
    int out_of_memory;
    MOJOSHADER_malloc malloc;
    MOJOSHADER_free free;
    void *malloc_data;
    int error_count;
    ErrorList *errors;
    StringCache *strcache;
    const char *sourcefile;  // current source file that we're parsing.
    unsigned int sourceline; // current line in sourcefile that we're parsing.
    SymbolMap usertypes;
    SymbolMap variables;
    MOJOSHADER_astNode *ast;  // Abstract Syntax Tree
    const char *source_profile;

    // These are entries allocated in the strcache, so these pointers are
    //  valid from shortly after we create the cache until we destroy it
    //  with the rest of the context. This makes it so we can compare common
    //  strings by pointer without having to hash them every time, so long as
    //  we're comparing a string pointer we know came from this string cache.
    // The first batch are simplifed datatype strings ("b" == bool, etc).
    const char *str_b;  // "b"
    const char *str_f;  // "f"
    const char *str_i;  // "i"
    const char *str_u;  // "u"
    const char *str_h;  // "h"
    const char *str_d;  // "d"
    const char *str_s;  // "s"
    const char *str_S;  // "S"
    const char *str_s1;  // "s1"
    const char *str_s2;  // "s2"
    const char *str_s3;  // "s3"
    const char *str_sc;  // "sc"
    const char *str_ss;  // "ss"
    const char *str_sS;  // "sS"
    const char *str_Fs;  // "Fs"
    const char *str_Fu;  // "Fu"
    const char *str_ns;  // "ns"
    const char *str_nu;  // "nu"
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

static void failf(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
static void failf(Context *ctx, const char *fmt, ...)
{
    const char *fname = ctx->sourcefile;
    const unsigned int error_position = ctx->sourceline;

    ctx->isfail = 1;

    const int MAX_ERROR_COUNT = 128;
    if (ctx->error_count == (MAX_ERROR_COUNT-1))
        fmt = "Too many errors, not reporting any more.";
    else if (ctx->error_count >= MAX_ERROR_COUNT)
        return;

    ErrorList *error = (ErrorList *) Malloc(ctx, sizeof (ErrorList));
    if (error == NULL)
        return;

    char scratch = 0;
    va_list ap;
    va_start(ap, fmt);
    const int len = vsnprintf(&scratch, sizeof (scratch), fmt, ap);
    va_end(ap);

    char *failstr = (char *) Malloc(ctx, len + 1);
    if (failstr == NULL)
        Free(ctx, error);
    else
    {
        va_start(ap, fmt);
        vsnprintf(failstr, len + 1, fmt, ap);  // rebuild it.
        va_end(ap);

        error->error.error = failstr;
        error->error.filename = fname ? StrDup(ctx, fname) : NULL;
        error->error.error_position = error_position;
        error->next = NULL;

        ErrorList *prev = NULL;
        ErrorList *item = ctx->errors;
        while (item != NULL)
        {
            prev = item;
            item = item->next;
        } // while

        if (prev == NULL)
            ctx->errors = error;
        else
            prev->next = error;

        ctx->error_count++;
    } // else
} // failf

static inline void fail(Context *ctx, const char *reason)
{
    failf(ctx, "%s", reason);
} // fail

static inline int isfail(const Context *ctx)
{
    return ctx->isfail;
} // isfail


static void symbolmap_nuke(const void *k, const void *v, void *d) {/*no-op*/}

static int create_symbolmap(Context *ctx, SymbolMap *map)
{
    // !!! FIXME: should compare string pointer, with string in cache.
    map->scope = NULL;
    map->hash = hash_create(ctx, hash_hash_string, hash_keymatch_string,
                            symbolmap_nuke, 1, ctx->malloc, ctx->free,
                            ctx->malloc_data);
    if (!map->hash)
    {
        out_of_memory(ctx);
        return 0;
    } // if

    return 1;
} // create_symbolmap


static void push_symbol(Context *ctx, SymbolMap *map,
                        const char *sym, const char *datatype)
{
    // !!! FIXME: decide if this symbol is defined, and if so, if it's in
    // !!! FIXME:  the current scope.

    SymbolScope *item = (SymbolScope *) Malloc(ctx, sizeof (SymbolScope));
    if (item == NULL)
        return;

    if (sym != NULL)
    {
        if (hash_insert(map->hash, sym, datatype) == -1)
        {
            Free(ctx, item);
            return;
        } // if
    } // if

    item->symbol = sym;  // cached strings, don't copy.
    item->datatype = datatype;
    item->next = map->scope;
    map->scope = item;
} // push_symbol

static inline void push_usertype(Context *ctx, const char *sym, const char *dt)
{
    push_symbol(ctx, &ctx->usertypes, sym, dt);
} // push_usertype

static inline void push_variable(Context *ctx, const char *sym, const char *dt)
{
    push_symbol(ctx, &ctx->variables, sym, dt);
} // push_variable

static inline void push_scope(Context *ctx)
{
    push_usertype(ctx, NULL, NULL);
    push_variable(ctx, NULL, NULL);
} // push_scope


static void pop_symbol(Context *ctx, SymbolMap *map)
{
    SymbolScope *item = map->scope;
    if (!item)
        return;
    if (item->symbol)
        hash_remove(map->hash, item->symbol);
    map->scope = item->next;
    Free(ctx, item);
} // pop_symbol

static void pop_symbol_scope(Context *ctx, SymbolMap *map)
{
    while ((map->scope) && (map->scope->symbol))
        pop_symbol(ctx, map);

    assert(map->scope != NULL);
    assert(map->scope->symbol == NULL);
    pop_symbol(ctx, map);
} // pop_symbol_scope

static inline void pop_scope(Context *ctx)
{
    pop_symbol_scope(ctx, &ctx->usertypes);
    pop_symbol_scope(ctx, &ctx->variables);
} // push_scope

static const char *find_symbol(Context *ctx, SymbolMap *map, const char *sym)
{
    const void *value = NULL;
    hash_find(map->hash, sym, &value);
    return (const char *) value;
} // find_symbol

static inline const char *find_usertype(Context *ctx, const char *sym)
{
    return find_symbol(ctx, &ctx->usertypes, sym);
} // find_usertype

static inline const char *find_variable(Context *ctx, const char *sym)
{
    return find_symbol(ctx, &ctx->variables, sym);
} // find_variable

static void destroy_symbolmap(Context *ctx, SymbolMap *map)
{
    while (map->scope)
        pop_symbol_scope(ctx, map);
    hash_destroy(map->hash);
} // destroy_symbolmap


// These functions are mostly for construction and cleanup of nodes in the
//  parse tree. Mostly this is simple allocation and initialization, so we
//  can do as little in the lemon code as possible, and then sort it all out
//  afterwards.

#define NEW_AST_NODE(retval, cls, typ) \
    cls *retval = (cls *) Malloc(ctx, sizeof (cls)); \
    do { \
        if (retval == NULL) { return NULL; } \
        retval->ast.type = typ; \
        retval->ast.filename = ctx->sourcefile; \
        retval->ast.line = ctx->sourceline; \
    } while (0)

#define DELETE_AST_NODE(cls) do { \
    if (!cls) return; \
} while (0)

static void delete_compilation_unit(Context*, MOJOSHADER_astCompilationUnit*);
static void delete_statement(Context *ctx, MOJOSHADER_astStatement *stmt);

static MOJOSHADER_astExpression *new_callfunc_expr(Context *ctx,
                                        MOJOSHADER_astExpression *identifier,
                                        MOJOSHADER_astArguments *args)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionCallFunction,
                 MOJOSHADER_AST_OP_CALLFUNC);
    retval->identifier = identifier;
    retval->args = args;
    return (MOJOSHADER_astExpression *) retval;
} // new_callfunc_expr

static MOJOSHADER_astExpression *new_constructor_expr(Context *ctx,
                                                const char *datatype,
                                                MOJOSHADER_astArguments *args)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionConstructor,
                 MOJOSHADER_AST_OP_CONSTRUCTOR);
    retval->datatype = datatype;
    retval->args = args;
    return (MOJOSHADER_astExpression *) retval;
} // new_constructor_expr

static MOJOSHADER_astExpression *new_cast_expr(Context *ctx,
                                            const char *datatype,
                                            MOJOSHADER_astExpression *operand)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionCast, MOJOSHADER_AST_OP_CAST);
    retval->datatype = datatype;
    retval->operand = operand;
    return (MOJOSHADER_astExpression *) retval;
} // new_cast_expr

static MOJOSHADER_astExpression *new_unary_expr(Context *ctx,
                                            const MOJOSHADER_astNodeType op,
                                            MOJOSHADER_astExpression *operand)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionUnary, op);
    assert(operator_is_unary(op));
    retval->operand = operand;
    return (MOJOSHADER_astExpression *) retval;
} // new_unary_expr

static MOJOSHADER_astExpression *new_binary_expr(Context *ctx,
                                            const MOJOSHADER_astNodeType op,
                                            MOJOSHADER_astExpression *left,
                                            MOJOSHADER_astExpression *right)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionBinary, op);
    assert(operator_is_binary(op));
    retval->left = left;
    retval->right = right;
    return (MOJOSHADER_astExpression *) retval;
} // new_binary_expr

static MOJOSHADER_astExpression *new_ternary_expr(Context *ctx,
                                            const MOJOSHADER_astNodeType op,
                                            MOJOSHADER_astExpression *left,
                                            MOJOSHADER_astExpression *center,
                                            MOJOSHADER_astExpression *right)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionTernary, op);
    assert(operator_is_ternary(op));
    retval->left = left;
    retval->center = center;
    retval->right = right;
    return (MOJOSHADER_astExpression *) retval;
} // new_ternary_expr

static MOJOSHADER_astExpression *new_deref_struct_expr(Context *ctx,
                                        MOJOSHADER_astExpression *identifier,
                                        const char *member)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionDerefStruct,
                 MOJOSHADER_AST_OP_DEREF_STRUCT);
    retval->identifier = identifier;
    retval->member = member;  // cached; don't copy string.
    return (MOJOSHADER_astExpression *) retval;
} // new_deref_struct_expr

static MOJOSHADER_astExpression *new_identifier_expr(Context *ctx,
                                                     const char *string)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionIdentifier,
                 MOJOSHADER_AST_OP_IDENTIFIER);
    retval->identifier = string;  // cached; don't copy string.
    return (MOJOSHADER_astExpression *) retval;
} // new_identifier_expr

static MOJOSHADER_astExpression *new_literal_int_expr(Context *ctx,
                                                       const int value)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionIntLiteral,
                 MOJOSHADER_AST_OP_INT_LITERAL);
    retval->value = value;
    return (MOJOSHADER_astExpression *) retval;
} // new_literal_int_expr

static MOJOSHADER_astExpression *new_literal_float_expr(Context *ctx,
                                                        const double dbl)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionFloatLiteral,
                 MOJOSHADER_AST_OP_FLOAT_LITERAL);
    retval->value = dbl;
    return (MOJOSHADER_astExpression *) retval;
} // new_literal_float_expr

static MOJOSHADER_astExpression *new_literal_string_expr(Context *ctx,
                                                         const char *string)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionStringLiteral,
                 MOJOSHADER_AST_OP_STRING_LITERAL);
    retval->string = string;  // cached; don't copy string.
    return (MOJOSHADER_astExpression *) retval;
} // new_literal_string_expr

static MOJOSHADER_astExpression *new_literal_boolean_expr(Context *ctx,
                                                          const int value)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionBooleanLiteral,
                 MOJOSHADER_AST_OP_BOOLEAN_LITERAL);
    retval->value = value;
    return (MOJOSHADER_astExpression *) retval;
} // new_literal_boolean_expr

static void delete_arguments(Context *ctx, MOJOSHADER_astArguments *args);

static void delete_expr(Context *ctx, MOJOSHADER_astExpression *_expr)
{
    MOJOSHADER_astNode *expr = (MOJOSHADER_astNode *) _expr;

    DELETE_AST_NODE(expr);

    if (expr->ast.type == MOJOSHADER_AST_OP_CAST)
        delete_expr(ctx, expr->cast.operand);

    else if (expr->ast.type == MOJOSHADER_AST_OP_CONSTRUCTOR)
        delete_arguments(ctx, expr->constructor.args);

    else if (expr->ast.type == MOJOSHADER_AST_OP_DEREF_STRUCT)
        delete_expr(ctx, expr->derefstruct.identifier);

    else if (operator_is_unary(expr->ast.type))
        delete_expr(ctx, expr->unary.operand);

    else if (operator_is_binary(expr->ast.type))
    {
        delete_expr(ctx, expr->binary.left);
        delete_expr(ctx, expr->binary.right);
    } // else if

    else if (operator_is_ternary(expr->ast.type))
    {
        delete_expr(ctx, expr->ternary.left);
        delete_expr(ctx, expr->ternary.center);
        delete_expr(ctx, expr->ternary.right);
    } // else if

    else if (expr->ast.type == MOJOSHADER_AST_OP_CALLFUNC)
    {
        delete_expr(ctx, expr->callfunc.identifier);
        delete_arguments(ctx, expr->callfunc.args);
    } // else if

    // rest of operators don't have extra data to free.

    Free(ctx, expr);
} // delete_expr

static MOJOSHADER_astArguments *new_argument(Context *ctx,
                                             MOJOSHADER_astExpression *arg)
{
    NEW_AST_NODE(retval, MOJOSHADER_astArguments, MOJOSHADER_AST_ARGUMENTS);
    retval->argument = arg;
    retval->next = NULL;
    return retval;
} // new_argument

static void delete_arguments(Context *ctx, MOJOSHADER_astArguments *args)
{
    DELETE_AST_NODE(args);
    delete_arguments(ctx, args->next);
    delete_expr(ctx, args->argument);
    Free(ctx, args);
} // delete_arguments

static MOJOSHADER_astFunctionParameters *new_function_param(Context *ctx,
                        const MOJOSHADER_astInputModifier inputmod,
                        const char *datatype, const char *identifier,
                        const char *semantic,
                        const MOJOSHADER_astInterpolationModifier interpmod,
                        MOJOSHADER_astExpression *initializer)
{
    NEW_AST_NODE(retval, MOJOSHADER_astFunctionParameters,
                 MOJOSHADER_AST_FUNCTION_PARAMS);
    retval->input_modifier = inputmod;
    retval->datatype = datatype;
    retval->identifier = identifier;
    retval->semantic = semantic;
    retval->interpolation_modifier = interpmod;
    retval->initializer = initializer;
    retval->next = NULL;
    return retval;
} // new_function_param

static void delete_function_params(Context *ctx,
                                   MOJOSHADER_astFunctionParameters *params)
{
    DELETE_AST_NODE(params);
    delete_function_params(ctx, params->next);
    delete_expr(ctx, params->initializer);
    Free(ctx, params);
} // delete_function_params

static MOJOSHADER_astFunctionSignature *new_function_signature(Context *ctx,
                                    const char *datatype,
                                    const char *identifier,
                                    MOJOSHADER_astFunctionParameters *params)
{
    NEW_AST_NODE(retval, MOJOSHADER_astFunctionSignature,
                 MOJOSHADER_AST_FUNCTION_SIGNATURE);
    retval->datatype = datatype;
    retval->identifier = identifier;
    retval->params = params;
    retval->storage_class = MOJOSHADER_AST_FNSTORECLS_NONE;
    retval->semantic = NULL;
    return retval;
} // new_function_signature

static void delete_function_signature(Context *ctx,
                                      MOJOSHADER_astFunctionSignature *sig)
{
    DELETE_AST_NODE(sig);
    delete_function_params(ctx, sig->params);
    Free(ctx, sig);
} // delete_function_signature

static MOJOSHADER_astCompilationUnit *new_function(Context *ctx,
                                MOJOSHADER_astFunctionSignature *declaration,
                                MOJOSHADER_astStatement *definition)
{
    NEW_AST_NODE(retval, MOJOSHADER_astCompilationUnitFunction,
                 MOJOSHADER_AST_COMPUNIT_FUNCTION);
    retval->next = NULL;
    retval->declaration = declaration;
    retval->definition = definition;
    return (MOJOSHADER_astCompilationUnit *) retval;
} // new_function

static void delete_function(Context *ctx,
                            MOJOSHADER_astCompilationUnitFunction *unitfn)
{
    DELETE_AST_NODE(unitfn);
    delete_compilation_unit(ctx, unitfn->next);
    delete_function_signature(ctx, unitfn->declaration);
    delete_statement(ctx, unitfn->definition);
    Free(ctx, unitfn);
} // delete_function

static MOJOSHADER_astScalarOrArray *new_scalar_or_array(Context *ctx,
                                          const char *ident, const int isvec,
                                          MOJOSHADER_astExpression *dim)
{
    NEW_AST_NODE(retval, MOJOSHADER_astScalarOrArray,
                 MOJOSHADER_AST_SCALAR_OR_ARRAY);
    retval->identifier = ident;
    retval->isarray = isvec;
    retval->dimension = dim;
    return retval;
} // new_scalar_or_array

static void delete_scalar_or_array(Context *ctx,MOJOSHADER_astScalarOrArray *s)
{
    DELETE_AST_NODE(s);
    delete_expr(ctx, s->dimension);
    Free(ctx, s);
} // delete_scalar_or_array

static MOJOSHADER_astTypedef *new_typedef(Context *ctx, const int isconst,
                                          const char *datatype,
                                          MOJOSHADER_astScalarOrArray *soa)
{
    // we correct this datatype to the final string during semantic analysis.
    NEW_AST_NODE(retval, MOJOSHADER_astTypedef, MOJOSHADER_AST_TYPEDEF);
    retval->isconst = isconst;
    retval->datatype = datatype;
    retval->details = soa;
    return retval;
} // new_typedef

static void delete_typedef(Context *ctx, MOJOSHADER_astTypedef *td)
{
    DELETE_AST_NODE(td);
    delete_scalar_or_array(ctx, td->details);
    Free(ctx, td);
} // delete_typedef

static MOJOSHADER_astPackOffset *new_pack_offset(Context *ctx,
                                                 const char *a, const char *b)
{
    NEW_AST_NODE(retval, MOJOSHADER_astPackOffset, MOJOSHADER_AST_PACK_OFFSET);
    retval->ident1 = a;
    retval->ident2 = b;
    return retval;
} // new_pack_offset

static void delete_pack_offset(Context *ctx, MOJOSHADER_astPackOffset *o)
{
    DELETE_AST_NODE(o);
    Free(ctx, o);
} // delete_pack_offset

static MOJOSHADER_astVariableLowLevel *new_variable_lowlevel(Context *ctx,
                                               MOJOSHADER_astPackOffset *po,
                                               const char *reg)
{
    NEW_AST_NODE(retval, MOJOSHADER_astVariableLowLevel,
                 MOJOSHADER_AST_VARIABLE_LOWLEVEL);
    retval->packoffset = po;
    retval->register_name = reg;
    return retval;
} // new_variable_lowlevel

static void delete_variable_lowlevel(Context *ctx,
                                     MOJOSHADER_astVariableLowLevel *vll)
{
    DELETE_AST_NODE(vll);
    delete_pack_offset(ctx, vll->packoffset);
    Free(ctx, vll);
} // delete_variable_lowlevel

static MOJOSHADER_astAnnotations *new_annotation(Context *ctx,
                                        const char *datatype,
                                        MOJOSHADER_astExpression *initializer)
{
    NEW_AST_NODE(retval, MOJOSHADER_astAnnotations, MOJOSHADER_AST_ANNOTATION);
    retval->datatype = datatype;
    retval->initializer = initializer;
    retval->next = NULL;
    return retval;
} // new_annotation

static void delete_annotation(Context *ctx, MOJOSHADER_astAnnotations *annos)
{
    DELETE_AST_NODE(annos);
    delete_annotation(ctx, annos->next);
    delete_expr(ctx, annos->initializer);
    Free(ctx, annos);
} // delete_annotation

static MOJOSHADER_astVariableDeclaration *new_variable_declaration(
                            Context *ctx, MOJOSHADER_astScalarOrArray *soa,
                            const char *semantic,
                            MOJOSHADER_astAnnotations *annotations,
                            MOJOSHADER_astExpression *init,
                            MOJOSHADER_astVariableLowLevel *vll)
{
    NEW_AST_NODE(retval, MOJOSHADER_astVariableDeclaration,
                 MOJOSHADER_AST_VARIABLE_DECLARATION);
    retval->attributes = 0;
    retval->datatype = NULL;
    retval->anonymous_datatype = NULL;
    retval->details = soa;
    retval->semantic = semantic;
    retval->annotations = annotations;
    retval->initializer = init;
    retval->lowlevel = vll;
    retval->next = NULL;
    return retval;
} // new_variable_declaration

static void delete_variable_declaration(Context *ctx,
                                        MOJOSHADER_astVariableDeclaration *dcl)
{
    DELETE_AST_NODE(dcl);
    delete_variable_declaration(ctx, dcl->next);
    delete_scalar_or_array(ctx, dcl->details);
    delete_annotation(ctx, dcl->annotations);
    delete_expr(ctx, dcl->initializer);
    delete_variable_lowlevel(ctx, dcl->lowlevel);
    Free(ctx, dcl);
} // delete_variable_declaration

static MOJOSHADER_astCompilationUnit *new_global_variable(Context *ctx,
                                      MOJOSHADER_astVariableDeclaration *decl)
{
    NEW_AST_NODE(retval, MOJOSHADER_astCompilationUnitVariable,
                 MOJOSHADER_AST_COMPUNIT_VARIABLE);
    retval->next = NULL;
    retval->declaration = decl;
    return (MOJOSHADER_astCompilationUnit *) retval;
} // new_global_variable

static void delete_global_variable(Context *ctx,
                                   MOJOSHADER_astCompilationUnitVariable *var)
{
    DELETE_AST_NODE(var);
    delete_compilation_unit(ctx, var->next);
    delete_variable_declaration(ctx, var->declaration);
    Free(ctx, var);
} // delete_global_variable

static MOJOSHADER_astCompilationUnit *new_global_typedef(Context *ctx,
                                                     MOJOSHADER_astTypedef *td)
{
    NEW_AST_NODE(retval, MOJOSHADER_astCompilationUnitTypedef,
                 MOJOSHADER_AST_COMPUNIT_TYPEDEF);
    retval->next = NULL;
    retval->type_info = td;
    return (MOJOSHADER_astCompilationUnit *) retval;
} // new_global_typedef

static void delete_global_typedef(Context *ctx,
                                  MOJOSHADER_astCompilationUnitTypedef *unit)
{
    DELETE_AST_NODE(unit);
    delete_compilation_unit(ctx, unit->next);
    delete_typedef(ctx, unit->type_info);
    Free(ctx, unit);
} // delete_global_typedef

static MOJOSHADER_astStructMembers *new_struct_member(Context *ctx,
                                            MOJOSHADER_astScalarOrArray *soa,
                                            const char *semantic)
{
    NEW_AST_NODE(retval, MOJOSHADER_astStructMembers,
                 MOJOSHADER_AST_STRUCT_MEMBER);
    retval->datatype = NULL;
    retval->semantic = semantic;
    retval->details = soa;
    retval->interpolation_mod = MOJOSHADER_AST_INTERPMOD_NONE;
    retval->next = NULL;
    return retval;
} // new_struct_member

static void delete_struct_member(Context *ctx,
                                 MOJOSHADER_astStructMembers *member)
{
    DELETE_AST_NODE(member);
    delete_struct_member(ctx, member->next);
    delete_scalar_or_array(ctx, member->details);
    Free(ctx, member);
} // delete_struct_member

static MOJOSHADER_astStructDeclaration *new_struct_declaration(Context *ctx,
                                        const char *name,
                                        MOJOSHADER_astStructMembers *members)
{
    NEW_AST_NODE(retval, MOJOSHADER_astStructDeclaration,
                 MOJOSHADER_AST_STRUCT_DECLARATION);
    retval->name = name;
    retval->members = members;
    return retval;
} // new_struct_declaration

static void delete_struct_declaration(Context *ctx,
                                      MOJOSHADER_astStructDeclaration *decl)
{
    DELETE_AST_NODE(decl);
    delete_struct_member(ctx, decl->members);
    Free(ctx, decl);
} // delete_struct_declaration

static MOJOSHADER_astCompilationUnit *new_global_struct(Context *ctx,
                                           MOJOSHADER_astStructDeclaration *sd)
{
    NEW_AST_NODE(retval, MOJOSHADER_astCompilationUnitStruct,
                 MOJOSHADER_AST_COMPUNIT_STRUCT);
    retval->next = NULL;
    retval->struct_info = sd;
    return (MOJOSHADER_astCompilationUnit *) retval;
} // new_global_struct

static void delete_global_struct(Context *ctx,
                                 MOJOSHADER_astCompilationUnitStruct *unit)
{
    DELETE_AST_NODE(unit);
    delete_compilation_unit(ctx, unit->next);
    delete_struct_declaration(ctx, unit->struct_info);
    Free(ctx, unit);
} // delete_global_struct

static void delete_compilation_unit(Context *ctx,
                                    MOJOSHADER_astCompilationUnit *unit)
{
    if (!unit) return;

    // it's important to not recurse too deeply here, since you may have
    //  thousands of items in this linked list (each line of a massive
    //  function, for example). To avoid this, we iterate the list here,
    //  deleting all children and making them think they have no reason
    //  to recurse in their own delete methods.
    // Please note that everyone should _try_ to delete their "next" member,
    //  just in case, but hopefully this cleaned it out.

    MOJOSHADER_astCompilationUnit *i = unit->next;
    unit->next = NULL;
    while (i)
    {
        MOJOSHADER_astCompilationUnit *next = i->next;
        i->next = NULL;
        delete_compilation_unit(ctx, i);
        i = next;
    } // while

    switch (unit->ast.type)
    {
        #define DELETE_UNIT(typ, cls, fn) \
            case MOJOSHADER_AST_COMPUNIT_##typ: delete_##fn(ctx, (cls *) unit); break;
        DELETE_UNIT(FUNCTION, MOJOSHADER_astCompilationUnitFunction, function);
        DELETE_UNIT(TYPEDEF, MOJOSHADER_astCompilationUnitTypedef, global_typedef);
        DELETE_UNIT(VARIABLE, MOJOSHADER_astCompilationUnitVariable, global_variable);
        DELETE_UNIT(STRUCT, MOJOSHADER_astCompilationUnitStruct, global_struct);
        #undef DELETE_UNIT
        default: assert(0 && "missing cleanup code"); break;
    } // switch

    // don't free (unit) here, the class-specific functions do it.
} // delete_compilation_unit

static MOJOSHADER_astStatement *new_typedef_statement(Context *ctx,
                                                      MOJOSHADER_astTypedef *td)
{
    NEW_AST_NODE(retval, MOJOSHADER_astTypedefStatement,
                 MOJOSHADER_AST_STATEMENT_TYPEDEF);
    retval->next = NULL;
    retval->type_info = td;
    return (MOJOSHADER_astStatement *) retval;
} // new_typedef_statement

static void delete_typedef_statement(Context *ctx,
                                     MOJOSHADER_astTypedefStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_typedef(ctx, stmt->type_info);
    Free(ctx, stmt);
} // delete_typedef_statement

static MOJOSHADER_astStatement *new_return_statement(Context *ctx,
                                                MOJOSHADER_astExpression *expr)
{
    NEW_AST_NODE(retval, MOJOSHADER_astReturnStatement,
                 MOJOSHADER_AST_STATEMENT_RETURN);
    retval->next = NULL;
    retval->expr = expr;
    return (MOJOSHADER_astStatement *) retval;
} // new_return_statement

static void delete_return_statement(Context *ctx,
                                    MOJOSHADER_astReturnStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_return_statement

static MOJOSHADER_astStatement *new_block_statement(Context *ctx,
                                               MOJOSHADER_astStatement *stmts)
{
    NEW_AST_NODE(retval, MOJOSHADER_astBlockStatement,
                 MOJOSHADER_AST_STATEMENT_BLOCK);
    retval->next = NULL;
    retval->statements = stmts;
    return (MOJOSHADER_astStatement *) retval;
} // new_block_statement

static void delete_block_statement(Context *ctx,
                                   MOJOSHADER_astBlockStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->statements);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_statement_block

static MOJOSHADER_astStatement *new_for_statement(Context *ctx,
                                    MOJOSHADER_astVariableDeclaration *decl,
                                    MOJOSHADER_astExpression *initializer,
                                    MOJOSHADER_astExpression *looptest,
                                    MOJOSHADER_astExpression *counter,
                                    MOJOSHADER_astStatement *statement)
{
    NEW_AST_NODE(retval, MOJOSHADER_astForStatement,
                 MOJOSHADER_AST_STATEMENT_FOR);
    retval->next = NULL;
    retval->unroll = -1;
    retval->var_decl = decl;
    retval->initializer = initializer;
    retval->looptest = looptest;
    retval->counter = counter;
    retval->statement = statement;
    return (MOJOSHADER_astStatement *) retval;
} // new_for_statement

static void delete_for_statement(Context *ctx,MOJOSHADER_astForStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_variable_declaration(ctx, stmt->var_decl);
    delete_expr(ctx, stmt->initializer);
    delete_expr(ctx, stmt->looptest);
    delete_expr(ctx, stmt->counter);
    delete_statement(ctx, stmt->statement);
    Free(ctx, stmt);
} // delete_for_statement

static MOJOSHADER_astStatement *new_do_statement(Context *ctx,
                                                const int unroll,
                                                MOJOSHADER_astStatement *stmt,
                                                MOJOSHADER_astExpression *expr)
{
    NEW_AST_NODE(retval,MOJOSHADER_astDoStatement,MOJOSHADER_AST_STATEMENT_DO);
    retval->next = NULL;
    retval->unroll = unroll;
    retval->expr = expr;
    retval->statement = stmt;
    return (MOJOSHADER_astStatement *) retval;
} // new_do_statement

static void delete_do_statement(Context *ctx, MOJOSHADER_astDoStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_statement(ctx, stmt->statement);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_do_statement

static MOJOSHADER_astStatement *new_while_statement(Context *ctx,
                                                const int unroll,
                                                MOJOSHADER_astExpression *expr,
                                                MOJOSHADER_astStatement *stmt)
{
    NEW_AST_NODE(retval, MOJOSHADER_astWhileStatement,
                 MOJOSHADER_AST_STATEMENT_WHILE);
    retval->next = NULL;
    retval->unroll = unroll;
    retval->expr = expr;
    retval->statement = stmt;
    return (MOJOSHADER_astStatement *) retval;
} // new_while_statement

static void delete_while_statement(Context *ctx,
                                   MOJOSHADER_astWhileStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_statement(ctx, stmt->statement);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_while_statement

static MOJOSHADER_astStatement *new_if_statement(Context *ctx,
                                            const int attr,
                                            MOJOSHADER_astExpression *expr,
                                            MOJOSHADER_astStatement *stmt,
                                            MOJOSHADER_astStatement *elsestmt)
{
    NEW_AST_NODE(retval,MOJOSHADER_astIfStatement,MOJOSHADER_AST_STATEMENT_IF);
    retval->next = NULL;
    retval->attributes = attr;
    retval->expr = expr;
    retval->statement = stmt;
    retval->else_statement = elsestmt;
    return (MOJOSHADER_astStatement *) retval;
} // new_if_statement

static void delete_if_statement(Context *ctx, MOJOSHADER_astIfStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_expr(ctx, stmt->expr);
    delete_statement(ctx, stmt->statement);
    delete_statement(ctx, stmt->else_statement);
    Free(ctx, stmt);
} // delete_if_statement

static MOJOSHADER_astSwitchCases *new_switch_case(Context *ctx,
                                                MOJOSHADER_astExpression *expr,
                                                MOJOSHADER_astStatement *stmt)
{
    NEW_AST_NODE(retval, MOJOSHADER_astSwitchCases, MOJOSHADER_AST_SWITCH_CASE);
    retval->expr = expr;
    retval->statement = stmt;
    retval->next = NULL;
    return retval;
} // new_switch_case

static void delete_switch_case(Context *ctx, MOJOSHADER_astSwitchCases *sc)
{
    DELETE_AST_NODE(sc);
    delete_switch_case(ctx, sc->next);
    delete_expr(ctx, sc->expr);
    delete_statement(ctx, sc->statement);
    Free(ctx, sc);
} // delete_switch_case

static MOJOSHADER_astStatement *new_empty_statement(Context *ctx)
{
    NEW_AST_NODE(retval, MOJOSHADER_astEmptyStatement,
                 MOJOSHADER_AST_STATEMENT_EMPTY);
    retval->next = NULL;
    return (MOJOSHADER_astStatement *) retval;
} // new_empty_statement

static void delete_empty_statement(Context *ctx,
                                   MOJOSHADER_astEmptyStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_empty_statement

static MOJOSHADER_astStatement *new_break_statement(Context *ctx)
{
    NEW_AST_NODE(retval, MOJOSHADER_astBreakStatement,
                 MOJOSHADER_AST_STATEMENT_BREAK);
    retval->next = NULL;
    return (MOJOSHADER_astStatement *) retval;
} // new_break_statement

static void delete_break_statement(Context *ctx,
                                   MOJOSHADER_astBreakStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_break_statement

static MOJOSHADER_astStatement *new_continue_statement(Context *ctx)
{
    NEW_AST_NODE(retval, MOJOSHADER_astContinueStatement,
                 MOJOSHADER_AST_STATEMENT_CONTINUE);
    retval->next = NULL;
    return (MOJOSHADER_astStatement *) retval;
} // new_continue_statement

static void delete_continue_statement(Context *ctx,
                                      MOJOSHADER_astContinueStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_continue_statement

static MOJOSHADER_astStatement *new_discard_statement(Context *ctx)
{
    NEW_AST_NODE(retval, MOJOSHADER_astDiscardStatement,
                 MOJOSHADER_AST_STATEMENT_DISCARD);
    retval->next = NULL;
    return (MOJOSHADER_astStatement *) retval;
} // new_discard_statement

static void delete_discard_statement(Context *ctx,
                                     MOJOSHADER_astDiscardStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_discard_statement

static MOJOSHADER_astStatement *new_expr_statement(Context *ctx,
                                                MOJOSHADER_astExpression *expr)
{
    NEW_AST_NODE(retval, MOJOSHADER_astExpressionStatement,
                 MOJOSHADER_AST_STATEMENT_EXPRESSION);
    retval->next = NULL;
    retval->expr = expr;
    return (MOJOSHADER_astStatement *) retval;
} // new_expr_statement

static void delete_expr_statement(Context *ctx,
                                  MOJOSHADER_astExpressionStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_expr_statement

static MOJOSHADER_astStatement *new_switch_statement(Context *ctx,
                                            const int attr,
                                            MOJOSHADER_astExpression *expr,
                                            MOJOSHADER_astSwitchCases *cases)
{
    NEW_AST_NODE(retval, MOJOSHADER_astSwitchStatement,
                 MOJOSHADER_AST_STATEMENT_SWITCH);
    retval->next = NULL;
    retval->attributes = attr;
    retval->expr = expr;
    retval->cases = cases;
    return (MOJOSHADER_astStatement *) retval;
} // new_switch_statement

static void delete_switch_statement(Context *ctx,
                                    MOJOSHADER_astSwitchStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_expr(ctx, stmt->expr);
    delete_switch_case(ctx, stmt->cases);
    Free(ctx, stmt);
} // delete_switch_statement

static MOJOSHADER_astStatement *new_struct_statement(Context *ctx,
                                        MOJOSHADER_astStructDeclaration *sd)
{
    NEW_AST_NODE(retval, MOJOSHADER_astStructStatement,
                 MOJOSHADER_AST_STATEMENT_STRUCT);
    retval->next = NULL;
    retval->struct_info = sd;
    return (MOJOSHADER_astStatement *) retval;
} // new_struct_statement

static void delete_struct_statement(Context *ctx,
                                    MOJOSHADER_astStructStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_struct_declaration(ctx, stmt->struct_info);
    Free(ctx, stmt);
} // delete_struct_statement

static MOJOSHADER_astStatement *new_vardecl_statement(Context *ctx,
                                        MOJOSHADER_astVariableDeclaration *vd)
{
    NEW_AST_NODE(retval, MOJOSHADER_astVarDeclStatement,
                 MOJOSHADER_AST_STATEMENT_VARDECL);
    retval->next = NULL;
    retval->declaration = vd;
    return (MOJOSHADER_astStatement *) retval;
} // new_vardecl_statement

static void delete_vardecl_statement(Context *ctx,
                                     MOJOSHADER_astVarDeclStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_variable_declaration(ctx, stmt->declaration);
    Free(ctx, stmt);
} // delete_vardecl_statement

static void delete_statement(Context *ctx, MOJOSHADER_astStatement *stmt)
{
    if (!stmt) return;

    // it's important to not recurse too deeply here, since you may have
    //  thousands of items in this linked list (each line of a massive
    //  function, for example). To avoid this, we iterate the list here,
    //  deleting all children and making them think they have no reason
    //  to recurse in their own delete methods.
    // Please note that everyone should _try_ to delete their "next" member,
    //  just in case, but hopefully this cleaned it out.

    MOJOSHADER_astStatement *i = stmt->next;
    stmt->next = NULL;
    while (i)
    {
        MOJOSHADER_astStatement *next = i->next;
        i->next = NULL;
        delete_statement(ctx, i);
        i = next;
    } // while

    switch (stmt->ast.type)
    {
        #define DELETE_STATEMENT(typ, cls, fn) \
            case MOJOSHADER_AST_STATEMENT_##typ: \
                delete_##fn##_statement(ctx, (cls *) stmt); break;
        DELETE_STATEMENT(BLOCK, MOJOSHADER_astBlockStatement, block);
        DELETE_STATEMENT(EMPTY, MOJOSHADER_astEmptyStatement, empty);
        DELETE_STATEMENT(IF, MOJOSHADER_astIfStatement, if);
        DELETE_STATEMENT(SWITCH, MOJOSHADER_astSwitchStatement, switch);
        DELETE_STATEMENT(EXPRESSION, MOJOSHADER_astExpressionStatement, expr);
        DELETE_STATEMENT(FOR, MOJOSHADER_astForStatement, for);
        DELETE_STATEMENT(DO, MOJOSHADER_astDoStatement, do);
        DELETE_STATEMENT(WHILE, MOJOSHADER_astWhileStatement, while);
        DELETE_STATEMENT(RETURN, MOJOSHADER_astReturnStatement, return);
        DELETE_STATEMENT(BREAK, MOJOSHADER_astBreakStatement, break);
        DELETE_STATEMENT(CONTINUE, MOJOSHADER_astContinueStatement, continue);
        DELETE_STATEMENT(DISCARD, MOJOSHADER_astDiscardStatement, discard);
        DELETE_STATEMENT(TYPEDEF, MOJOSHADER_astTypedefStatement, typedef);
        DELETE_STATEMENT(STRUCT, MOJOSHADER_astStructStatement, struct);
        DELETE_STATEMENT(VARDECL, MOJOSHADER_astVarDeclStatement, vardecl);
        #undef DELETE_STATEMENT
        default: assert(0 && "missing cleanup code"); break;
    } // switch
    // don't free (stmt) here, the class-specific functions do it.
} // delete_statement

static const char *get_usertype(const Context *ctx, const char *token)
{
    const void *value;  // search all scopes.
    if (!hash_find(ctx->usertypes.hash, token, &value))
        return NULL;
    return (const char *) value;
} // get_usertype


// This is where the actual parsing happens. It's Lemon-generated!
#define __MOJOSHADER_HLSL_COMPILER__ 1
#include "mojoshader_parser_hlsl.h"


/*
 * datatype strings...
 *
 * "v" == void
 * "b" == bool
 * "i" == int
 * "u" == uint
 * "h" == half
 * "f" == float
 * "ns" == snorm float
 * "nu" == unorm float
 * "B{*}" == buffer
 * "V{#,*}" == vector
 * "M{#,#,*}" == matrix
 * "d" == double
 * "a{#,*}" == array
 * "S" == string
 * "X{*}" == struct
 * "s1" == sampler1D
 * "s2" == sampler2D
 * "s3" == sampler3D
 * "sc" == samplerCUBE
 * "ss" == sampler_state
 * "sS" == SamplerComparisonState
 * "U{*}" == user type.
 * "F{*,*}" == function
 */


static void require_numeric_datatype(Context *ctx, const char *datatype)
{
    if (datatype == ctx->str_f) return;  // float
    if (datatype == ctx->str_i) return;  // int
    if (datatype == ctx->str_b) return;  // bool
    if (datatype == ctx->str_u) return;  // uint
    if (datatype == ctx->str_h) return;  // half
    if (datatype == ctx->str_d) return;  // double
    fail(ctx, "Expected numeric type");  // !!! FIXME: fmt.
    // !!! FIXME: replace AST node with an AST_OP_INT_LITERAL zero, keep going.
} // require_numeric_datatype

static void require_integer_datatype(Context *ctx, const char *datatype)
{
    if (datatype == ctx->str_i) return;  // int
    if (datatype == ctx->str_u) return;  // uint
    fail(ctx, "Expected integer type");  // !!! FIXME: fmt.
    // !!! FIXME: replace AST node with an AST_OP_INT_LITERAL zero, keep going.
} // require_integer_datatype

static void require_boolean_datatype(Context *ctx, const char *datatype)
{
    if (datatype == ctx->str_b) return;  // bool
    if (datatype == ctx->str_i) return;  // int
    if (datatype == ctx->str_u) return;  // uint
    fail(ctx, "Expected boolean type");  // !!! FIXME: fmt.
    // !!! FIXME: replace AST node with an AST_OP_BOOLEAN_LITERAL false, keep going.
} // require_numeric_datatype


static void require_array_datatype(Context *ctx, const char *datatype)
{
    if (datatype[0] != 'a')
        fail(ctx, "expected array");
        // !!! FIXME: delete array dereference for further processing.
} // require_array_datatype


static void require_struct_datatype(Context *ctx, const char *datatype)
{
    if (datatype[0] != 'X')
        fail(ctx, "expected struct");
        // !!! FIXME: delete struct dereference for further processing.
} // require_array_datatype


static const char *require_function_datatype(Context *ctx, const char *datatype)
{
    if (datatype[0] != 'F')
    {
        fail(ctx, "expected function");
        // !!! FIXME: delete function call for further processing.
        return ctx->str_i;
    } // if

    assert(datatype[1] == '{');
    datatype += 2;
    const char *ptr = strchr(datatype, ',');
    assert(ptr != NULL);
    return stringcache_len(ctx->strcache, datatype, (unsigned int) (ptr-datatype));
} // require_function_datatype


// Extract the individual element type from an array datatype.
static const char *array_element_datatype(Context *ctx, const char *datatype)
{
    const char *ptr;
    const char *ptr2;
    unsigned int depth = 1;
    assert(datatype[0] == 'a');
    assert(datatype[1] == '{');
    ptr = strchr(datatype+2, ',');
    assert(ptr != NULL);
    ptr++;
    for (ptr2 = ptr; depth > 0; ptr2++)
    {
        const char ch = *ptr2;
        assert(ch != '\0');
        if (ch == '{')
            depth++;
        else if (ch == '}')
            depth--;
    } // for

    return stringcache_len(ctx->strcache, ptr, (unsigned int) (ptr2 - ptr));
} // array_element_datatype


// This tests two datatypes to see if they are compatible, and adds cast
//  operator nodes to the AST if the program was relying on implicit
//  casts between then. Will fail() if the datatypes can't be coerced
//  with a cast at all. (left) can be NULL to say that its datatype is
//  set in stone (an lvalue, for example). No other NULLs are allowed.
// Returns final datatype used once implicit casting is complete.
// The datatypes must be pointers from the string cache.
static const char *add_type_coercion(Context *ctx,
                                     MOJOSHADER_astExpression **left,
                                     const char *ldatatype,
                                     MOJOSHADER_astExpression **right,
                                     const char *rdatatype)
{
    // !!! FIXME: this whole function is probably naive at best.

    if (ldatatype == rdatatype)
        return ldatatype;   // they already match, so we're done.

    struct {
        const char *datatype;
        const int bits;
        const int is_unsigned;
        const int floating;
    } typeinf[] = {
        { ctx->str_b,  1, 1, 0 },
        { ctx->str_h, 16, 0, 1 },
        { ctx->str_i, 32, 0, 0 },
        { ctx->str_u, 32, 1, 0 },
        { ctx->str_f, 32, 0, 1 },
        { ctx->str_d, 64, 0, 1 },
    };

    int l, r;
    for (l = 0; l < STATICARRAYLEN(typeinf); l++)
    {
        if (typeinf[l].datatype == ldatatype)
            break;
    } // for
    for (r = 0; r < STATICARRAYLEN(typeinf); r++)
    {
        if (typeinf[r].datatype == rdatatype)
            break;
    } // for

    enum { CHOOSE_NEITHER, CHOOSE_LEFT, CHOOSE_RIGHT } choice = CHOOSE_NEITHER;
    if ((l < STATICARRAYLEN(typeinf)) && (r < STATICARRAYLEN(typeinf)))
    {
        if (left == NULL)
            choice = CHOOSE_LEFT;  // we need to force to the lvalue.
        else if (typeinf[l].bits > typeinf[r].bits)
            choice = CHOOSE_LEFT;
        else if (typeinf[l].bits < typeinf[r].bits)
            choice = CHOOSE_RIGHT;
        else if (typeinf[l].floating && !typeinf[r].floating)
            choice = CHOOSE_LEFT;
        else if (!typeinf[l].floating && typeinf[r].floating)
            choice = CHOOSE_RIGHT;
        else if (typeinf[l].is_unsigned && !typeinf[r].is_unsigned)
            choice = CHOOSE_LEFT;
        else if (!typeinf[l].is_unsigned && typeinf[r].is_unsigned)
            choice = CHOOSE_RIGHT;
    } // if

    if (choice == CHOOSE_LEFT)
    {
        *right = new_cast_expr(ctx, ldatatype, *right);
        return ldatatype;
    } // if
    else if (choice == CHOOSE_RIGHT)
    {
        *left = new_cast_expr(ctx, rdatatype, *left);
        return rdatatype;
    } // else if

    assert(choice == CHOOSE_NEITHER);
    fail(ctx, "incompatible data types");
    // Ditch original (*right), force a literal value that matches
    //  ldatatype, so further processing is normalized.
    // !!! FIXME: force (right) to match (left).
    delete_expr(ctx, *right);
    *right = new_cast_expr(ctx, ldatatype, new_literal_int_expr(ctx, 0));
    return ldatatype;
} // add_type_coercion


// Go through the AST and make sure all datatypes check out okay. For datatypes
//  that are compatible but are relying on an implicit cast, we add explicit
//  casts to the AST here, so further processing doesn't have to worry about
//  type coercion.
// For things that are incompatible, we generate errors and
//  then replace them with reasonable defaults so further processing can
//  continue (but code generation will be skipped due to errors).
// This means further processing can assume the AST is sane and not have to
//  spend effort verifying it again.
static const char *type_check_ast(Context *ctx, void *_ast)
{
    MOJOSHADER_astNode *ast = (MOJOSHADER_astNode *) _ast;
    const char *datatype;
    const char *datatype2;
    const char *datatype3;

    if (!ast)
        return NULL;

    // upkeep so we report correct error locations...
    ctx->sourcefile = ast->ast.filename;
    ctx->sourceline = ast->ast.line;

    switch (ast->ast.type)
    {
        case MOJOSHADER_AST_OP_POSTINCREMENT:
        case MOJOSHADER_AST_OP_POSTDECREMENT:
        case MOJOSHADER_AST_OP_PREINCREMENT:
        case MOJOSHADER_AST_OP_PREDECREMENT:
        case MOJOSHADER_AST_OP_COMPLEMENT:
        case MOJOSHADER_AST_OP_NEGATE:
            datatype = type_check_ast(ctx, ast->unary.operand);
            require_numeric_datatype(ctx, datatype);
            return datatype;

        case MOJOSHADER_AST_OP_NOT:
            datatype = type_check_ast(ctx, ast->unary.operand);
            require_boolean_datatype(ctx, datatype);
            return datatype;

        case MOJOSHADER_AST_OP_DEREF_ARRAY:
            datatype = type_check_ast(ctx, ast->binary.left);
            datatype2 = type_check_ast(ctx, ast->binary.right);
            require_array_datatype(ctx, datatype);
            require_numeric_datatype(ctx, datatype2);
            add_type_coercion(ctx, NULL, ctx->str_i, &ast->binary.right, datatype2);
            return array_element_datatype(ctx, datatype);

        case MOJOSHADER_AST_OP_DEREF_STRUCT:
            datatype = type_check_ast(ctx, ast->derefstruct.identifier);
            require_struct_datatype(ctx, datatype);
// !!! FIXME: map member to datatype
datatype = "!!! FIXME";
            return datatype;

        case MOJOSHADER_AST_OP_COMMA:
            // evaluate and throw away left, return right.
            type_check_ast(ctx, ast->binary.left);
            return type_check_ast(ctx, ast->binary.right);

        case MOJOSHADER_AST_OP_MULTIPLY:
        case MOJOSHADER_AST_OP_DIVIDE:
        case MOJOSHADER_AST_OP_MODULO:
        case MOJOSHADER_AST_OP_ADD:
        case MOJOSHADER_AST_OP_SUBTRACT:
        case MOJOSHADER_AST_OP_LSHIFT:
        case MOJOSHADER_AST_OP_RSHIFT:
            datatype = type_check_ast(ctx, ast->binary.left);
            datatype2 = type_check_ast(ctx, ast->binary.right);
            require_numeric_datatype(ctx, datatype);
            require_numeric_datatype(ctx, datatype2);
            return add_type_coercion(ctx, &ast->binary.left, datatype,
                                     &ast->binary.right, datatype2);

        case MOJOSHADER_AST_OP_LESSTHAN:
        case MOJOSHADER_AST_OP_GREATERTHAN:
        case MOJOSHADER_AST_OP_LESSTHANOREQUAL:
        case MOJOSHADER_AST_OP_GREATERTHANOREQUAL:
        case MOJOSHADER_AST_OP_NOTEQUAL:
        case MOJOSHADER_AST_OP_EQUAL:
            datatype = type_check_ast(ctx, ast->binary.left);
            datatype2 = type_check_ast(ctx, ast->binary.right);
            add_type_coercion(ctx, &ast->binary.left, datatype,
                              &ast->binary.right, datatype2);
            return ctx->str_b;

        case MOJOSHADER_AST_OP_BINARYAND:
        case MOJOSHADER_AST_OP_BINARYXOR:
        case MOJOSHADER_AST_OP_BINARYOR:
            datatype = type_check_ast(ctx, ast->binary.left);
            datatype2 = type_check_ast(ctx, ast->binary.right);
            require_integer_datatype(ctx, datatype);
            require_integer_datatype(ctx, datatype2);
            return add_type_coercion(ctx, &ast->binary.left, datatype,
                                     &ast->binary.right, datatype2);

        case MOJOSHADER_AST_OP_LOGICALAND:
        case MOJOSHADER_AST_OP_LOGICALOR:
            datatype = type_check_ast(ctx, ast->binary.left);
            datatype2 = type_check_ast(ctx, ast->binary.right);
            require_boolean_datatype(ctx, datatype);
            require_boolean_datatype(ctx, datatype2);
            add_type_coercion(ctx, &ast->binary.left, datatype,
                              &ast->binary.right, datatype2);
            return ctx->str_b;

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
            datatype = type_check_ast(ctx, ast->binary.left);
            datatype2 = type_check_ast(ctx, ast->binary.right);
            add_type_coercion(ctx, NULL, datatype, &ast->binary.right, datatype2);
            return datatype;

        case MOJOSHADER_AST_OP_CONDITIONAL:
            type_check_ast(ctx, ast->ternary.left);
            type_check_ast(ctx, ast->ternary.center);
            type_check_ast(ctx, ast->ternary.right);
            require_boolean_datatype(ctx, datatype);
            return add_type_coercion(ctx, &ast->ternary.center, datatype2,
                                     &ast->ternary.right, datatype3);

        case MOJOSHADER_AST_OP_IDENTIFIER:
            datatype = find_variable(ctx, ast->identifier.identifier);
            if (datatype == NULL)
            {
                fail(ctx, "Unknown identifier");
                // !!! FIXME: replace with a sane default, move on.
                datatype = ctx->str_i;
            } // if
            return datatype;

        case MOJOSHADER_AST_OP_INT_LITERAL:
            return ctx->str_i;

        case MOJOSHADER_AST_OP_FLOAT_LITERAL:
            return ctx->str_f;

        case MOJOSHADER_AST_OP_STRING_LITERAL:
            return ctx->str_S;

        case MOJOSHADER_AST_OP_BOOLEAN_LITERAL:
            return ctx->str_b;

        case MOJOSHADER_AST_ARGUMENTS:
            datatype = type_check_ast(ctx, ast->arguments.argument);
            if (ast->arguments.next != NULL)
            {
                datatype2 = type_check_ast(ctx, ast->arguments.next);
                datatype = stringcache_fmt(ctx->strcache, "%s%s",
                                           datatype, datatype2);
            } // if
            return datatype;

        case MOJOSHADER_AST_OP_CALLFUNC:
            datatype = type_check_ast(ctx, ast->callfunc.identifier);
            datatype2 = type_check_ast(ctx, ast->callfunc.args);
            return require_function_datatype(ctx, datatype);
// !!! FIXME: test each arg against function datatype.
            //return retval;  // this is the datatype of the func's return value.

        case MOJOSHADER_AST_OP_CONSTRUCTOR:
// !!! FIXME: test each arg against constructor datatype.
            type_check_ast(ctx, ast->constructor.args);
            return ast->constructor.datatype;

        case MOJOSHADER_AST_OP_CAST:
            datatype = ast->cast.datatype;
            datatype2 = type_check_ast(ctx, ast->cast.operand);
            // you still need type coercion, since you could do a wrong cast,
            //  like "int x = (short) mychar;"
            add_type_coercion(ctx, NULL, datatype, &ast->cast.operand, datatype2);
            return datatype;

        case MOJOSHADER_AST_STATEMENT_BREAK:
        case MOJOSHADER_AST_STATEMENT_CONTINUE:
        case MOJOSHADER_AST_STATEMENT_DISCARD:
        case MOJOSHADER_AST_STATEMENT_EMPTY:
            type_check_ast(ctx, ast->stmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_EXPRESSION:
            // !!! FIXME: warn about expressions without a side-effect here?
            type_check_ast(ctx, ast->exprstmt.expr);  // !!! FIXME: This is named badly...
            type_check_ast(ctx, ast->exprstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_IF:
            push_scope(ctx);  // new scope for "if ((int x = blah()) != 0)"
            type_check_ast(ctx, ast->ifstmt.expr);
            type_check_ast(ctx, ast->ifstmt.statement);
            pop_scope(ctx);
            type_check_ast(ctx, ast->ifstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_TYPEDEF:
            type_check_ast(ctx, ast->typedefstmt.type_info);
            type_check_ast(ctx, ast->typedefstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_SWITCH:
        {
            MOJOSHADER_astSwitchCases *cases = ast->switchstmt.cases;
            datatype = type_check_ast(ctx, ast->switchstmt.expr);
            while (cases)
            {
                datatype2 = type_check_ast(ctx, cases->expr);
                add_type_coercion(ctx, NULL, datatype,
                                  &cases->expr, datatype2);
                type_check_ast(ctx, cases->statement);
                cases = cases->next;
            } // while
            return NULL;
        } // case

        case MOJOSHADER_AST_SWITCH_CASE:
            assert(0 && "Should be done by MOJOSHADER_AST_STATEMENT_SWITCH.");
            return NULL;

        case MOJOSHADER_AST_STATEMENT_STRUCT:
            type_check_ast(ctx, ast->structstmt.struct_info);
            type_check_ast(ctx, ast->structstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_VARDECL:
            type_check_ast(ctx, ast->vardeclstmt.declaration);
            type_check_ast(ctx, ast->vardeclstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_BLOCK:
            push_scope(ctx);  // new vars declared here live until '}'.
            type_check_ast(ctx, ast->blockstmt.statements);
            pop_scope(ctx);
            type_check_ast(ctx, ast->blockstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_FOR:
            push_scope(ctx);  // new scope for "for (int x = 0; ...)"
            type_check_ast(ctx, ast->forstmt.var_decl);
            type_check_ast(ctx, ast->forstmt.initializer);
            type_check_ast(ctx, ast->forstmt.looptest);
            type_check_ast(ctx, ast->forstmt.counter);
            type_check_ast(ctx, ast->forstmt.statement);
            pop_scope(ctx);
            type_check_ast(ctx, ast->forstmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_DO:
            type_check_ast(ctx, ast->dostmt.statement);
            push_scope(ctx);  // new scope for "while ((int x = blah()) != 0)"
            type_check_ast(ctx, ast->dostmt.expr);
            pop_scope(ctx);
            type_check_ast(ctx, ast->dostmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_WHILE:
            push_scope(ctx);  // new scope for "while ((int x = blah()) != 0)"
            type_check_ast(ctx, ast->whilestmt.expr);
            type_check_ast(ctx, ast->whilestmt.statement);
            pop_scope(ctx);
            type_check_ast(ctx, ast->whilestmt.next);
            return NULL;

        case MOJOSHADER_AST_STATEMENT_RETURN:
            type_check_ast(ctx, ast->returnstmt.expr);
            type_check_ast(ctx, ast->returnstmt.next);

        case MOJOSHADER_AST_COMPUNIT_FUNCTION:
            datatype = get_usertype(ctx, ast->funcunit.declaration->identifier);
            if (datatype == NULL)
            {
                // add function declaration if we've not seen it.
                datatype = ast->funcunit.declaration->datatype;
                push_usertype(ctx, ast->funcunit.declaration->identifier, datatype);
            } // if

            // declarations can be done multiple times if they match.
            else if (datatype != ast->funcunit.declaration->datatype)
            {
                // !!! FIXME: function overloading is legal.
                fail(ctx, "function sigs don't match");
            } // else

            push_scope(ctx);  // so function params are in function scope.
            type_check_ast(ctx, ast->funcunit.declaration);
            if (ast->funcunit.definition == NULL)
                pop_scope(ctx);
            else
            {
                type_check_ast(ctx, ast->funcunit.definition);
                pop_scope(ctx);
                push_variable(ctx, ast->funcunit.declaration->identifier, datatype);
            } // else

            type_check_ast(ctx, ast->funcunit.next);
            return NULL;

        case MOJOSHADER_AST_COMPUNIT_TYPEDEF:
            type_check_ast(ctx, ast->typedefunit.type_info);
            type_check_ast(ctx, ast->typedefunit.next);
            return NULL;

        case MOJOSHADER_AST_COMPUNIT_STRUCT:
            type_check_ast(ctx, ast->structunit.struct_info);
            type_check_ast(ctx, ast->structunit.next);
            return NULL;

        case MOJOSHADER_AST_COMPUNIT_VARIABLE:
            type_check_ast(ctx, ast->varunit.declaration);
            type_check_ast(ctx, ast->varunit.next);
            return NULL;

        case MOJOSHADER_AST_SCALAR_OR_ARRAY:
            datatype = type_check_ast(ctx, ast->soa.dimension);
            require_integer_datatype(ctx, datatype);
assert(0); // !!! FIXME: figure out datatype of identifier.
            return NULL;

        case MOJOSHADER_AST_TYPEDEF:
        {
            MOJOSHADER_astScalarOrArray *soa = ast->typdef.details;
            datatype = get_usertype(ctx, soa->identifier);
            if (datatype != NULL)
            {
                fail(ctx, "typedef already defined");
                return datatype;
            } // if

            datatype = ast->typdef.datatype;

            // don't walk into MOJOSHADER_AST_SCALAR_OR_ARRAY here, since it can't resolve the identifier.
            // !!! FIXME: SCALAR_OR_ARRAY is sort of a mess.
            // !!! FIXME: this part is cut and paste.
            assert( (soa->isarray && soa->dimension) ||
                    (!soa->isarray && !soa->dimension) );

            if (soa->isarray)
            {
                if (soa->dimension->ast.type != MOJOSHADER_AST_OP_INT_LITERAL)
                {
                    fail(ctx, "Expected integer");
                    delete_expr(ctx, soa->dimension);  // make sane.
                    soa->dimension = new_literal_int_expr(ctx, 1);
                } // if

                const int dim = ((MOJOSHADER_astExpressionIntLiteral *) soa->dimension)->value;
                datatype = stringcache_fmt(ctx->strcache, "a{%d,%s}",
                                           dim, datatype);
            } // if

            ast->typdef.datatype = datatype;  // make sane.
            push_usertype(ctx, soa->identifier, datatype);
            return datatype;
        } // case

        case MOJOSHADER_AST_FUNCTION_PARAMS:
            push_variable(ctx, ast->params.identifier, ast->params.datatype);
            type_check_ast(ctx, ast->params.initializer);
            type_check_ast(ctx, ast->params.next);
            return NULL;

        case MOJOSHADER_AST_FUNCTION_SIGNATURE:
            type_check_ast(ctx, ast->funcsig.params);
            return ast->funcsig.datatype;

        case MOJOSHADER_AST_STRUCT_DECLARATION:
            datatype = type_check_ast(ctx, ast->structdecl.members);
            datatype = stringcache_fmt(ctx->strcache, "X{%s}", datatype);
            push_usertype(ctx, ast->structdecl.name, datatype);
            return stringcache_fmt(ctx->strcache, "U{%s}", ast->structdecl.name);

        case MOJOSHADER_AST_STRUCT_MEMBER:
            datatype = type_check_ast(ctx, ast->structmembers.details);
            datatype2 = type_check_ast(ctx, ast->structmembers.next);
            if (datatype2)
            {
                return stringcache_fmt(ctx->strcache, "%s%s",
                                       datatype, datatype2);
            } // if
            return datatype;

        case MOJOSHADER_AST_VARIABLE_DECLARATION:
            // this is true now, but we'll fill in ->datatype no matter what.
            assert((ast->vardecl.datatype && !ast->vardecl.anonymous_datatype) ||
                   (!ast->vardecl.datatype && ast->vardecl.anonymous_datatype));

            // fix up if necessary.
            if (ast->vardecl.anonymous_datatype != NULL)
                ast->vardecl.datatype = type_check_ast(ctx, ast->vardecl.anonymous_datatype);
            datatype = ast->vardecl.datatype;

            // don't walk into MOJOSHADER_AST_SCALAR_OR_ARRAY here, since it can't resolve the identifier.
            // !!! FIXME: SCALAR_OR_ARRAY is sort of a mess.
            // !!! FIXME: this part is cut and paste.
            assert( (ast->vardecl.details->isarray &&
                     ast->vardecl.details->dimension) ||
                     (!ast->vardecl.details->isarray &&
                      !ast->vardecl.details->dimension) );

            if (ast->vardecl.details->isarray)
            {
                MOJOSHADER_astScalarOrArray *soa = ast->vardecl.details;
                if (soa->dimension->ast.type != MOJOSHADER_AST_OP_INT_LITERAL)
                {
                    fail(ctx, "Expected integer");
                    delete_expr(ctx, soa->dimension);  // make sane.
                    soa->dimension = new_literal_int_expr(ctx, 1);
                } // if

                const int dim = ((MOJOSHADER_astExpressionIntLiteral *) soa->dimension)->value;
                datatype = stringcache_fmt(ctx->strcache, "a{%d,%s}",
                                           dim, datatype);
            } // if

            ast->vardecl.datatype = datatype;  // make sane.
            push_variable(ctx, ast->vardecl.details->identifier, datatype);
            datatype2 = type_check_ast(ctx, ast->vardecl.initializer);
            add_type_coercion(ctx, NULL, datatype, &ast->vardecl.initializer, datatype2);

            type_check_ast(ctx, ast->vardecl.annotations);
            type_check_ast(ctx, ast->vardecl.lowlevel);

            datatype2 = type_check_ast(ctx, ast->vardecl.next);
            assert(datatype == datatype2);
            return datatype;

        case MOJOSHADER_AST_ANNOTATION:
        {
            MOJOSHADER_astAnnotations *anno = &ast->annotations;
            while (anno)
            {
                type_check_ast(ctx, anno->initializer);
                anno = anno->next;
            } // while
            return NULL;
        } // case

        case MOJOSHADER_AST_PACK_OFFSET:
        case MOJOSHADER_AST_VARIABLE_LOWLEVEL:
            return NULL;  // no-op (for now, at least).

        default:
            assert(0 && "unexpected type");
    } // switch

    return NULL;
} // type_check_ast



static inline void semantic_analysis(Context *ctx)
{
    type_check_ast(ctx, ctx->ast);

    // !!! FIXME: build an IR here.

    // done with the AST, nuke it.
    delete_compilation_unit(ctx, (MOJOSHADER_astCompilationUnit *) ctx->ast);
    ctx->ast = NULL;

    // !!! FIXME: do everything else.  :)
} // semantic_analysis


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

static inline double strtodouble(const char *_str, unsigned int len)
{
    // !!! FIXME: laziness prevails.
    char *str = (char *) alloca(len+1);
    memcpy(str, _str, len);
    str[len] = '\0';
    return strtod(str, NULL);
} // strtodouble

#if 0
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
#endif

static int convert_to_lemon_token(Context *ctx, const char *token,
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
        //case ((Token) TOKEN_PP_PRAGMA): return TOKEN_HLSL_PRAGMA;
        //case ((Token) '\n'): return TOKEN_HLSL_NEWLINE;

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
            if (tokencmp("matrix")) return TOKEN_HLSL_MATRIX;
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
            if (tokencmp("true")) return TOKEN_HLSL_TRUE;
            if (tokencmp("false")) return TOKEN_HLSL_FALSE;
            if (tokencmp("SamplerComparisonState")) return TOKEN_HLSL_SAMPLERCOMPARISONSTATE;
            if (tokencmp("isolate")) return TOKEN_HLSL_ISOLATE;
            if (tokencmp("maxInstructionCount")) return TOKEN_HLSL_MAXINSTRUCTIONCOUNT;
            if (tokencmp("noExpressionOptimizations")) return TOKEN_HLSL_NOEXPRESSIONOPTIMIZATIONS;
            if (tokencmp("unused")) return TOKEN_HLSL_UNUSED;
            if (tokencmp("xps")) return TOKEN_HLSL_XPS;
            #undef tokencmp

            // get a canonical copy of the string now, as we'll need it.
            token = stringcache_len(ctx->strcache, token, tokenlen);
            if (get_usertype(ctx, token) != NULL)
                return TOKEN_HLSL_USERTYPE;
            return TOKEN_HLSL_IDENTIFIER;

        case TOKEN_EOI: return 0;
        default: assert(0 && "unexpected token from lexer\n"); return 0;
    } // switch

    return 0;
} // convert_to_lemon_token

static void destroy_context(Context *ctx)
{
    if (ctx != NULL)
    {
        MOJOSHADER_free f = ((ctx->free != NULL) ? ctx->free : MOJOSHADER_internal_free);
        void *d = ctx->malloc_data;

        // !!! FIXME: free ctx->errors
        delete_compilation_unit(ctx, (MOJOSHADER_astCompilationUnit*)ctx->ast);
        destroy_symbolmap(ctx, &ctx->usertypes);
        destroy_symbolmap(ctx, &ctx->variables);

        if (ctx->strcache)
            stringcache_destroy(ctx->strcache);

        f(ctx, d);
    } // if
} // destroy_context

static Context *build_context(MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    if (!m) m = MOJOSHADER_internal_malloc;
    if (!f) f = MOJOSHADER_internal_free;

    Context *ctx = (Context *) m(sizeof (Context), d);
    if (ctx == NULL)
        return NULL;

    memset(ctx, '\0', sizeof (Context));
    ctx->malloc = m;
    ctx->free = f;
    ctx->malloc_data = d;
    //ctx->parse_phase = MOJOSHADER_PARSEPHASE_NOTSTARTED;
    create_symbolmap(ctx, &ctx->usertypes); // !!! FIXME: check for failure.
    create_symbolmap(ctx, &ctx->variables); // !!! FIXME: check for failure.
    ctx->strcache = stringcache_create(m, f, d);  // !!! FIXME: check for failure.

    // fill in some common strings we'll want to use without further hashing.
    ctx->str_b = stringcache(ctx->strcache, "b");
    ctx->str_f = stringcache(ctx->strcache, "f");
    ctx->str_i = stringcache(ctx->strcache, "i");
    ctx->str_u = stringcache(ctx->strcache, "u");
    ctx->str_h = stringcache(ctx->strcache, "h");
    ctx->str_d = stringcache(ctx->strcache, "d");
    ctx->str_s = stringcache(ctx->strcache, "s");
    ctx->str_S = stringcache(ctx->strcache, "S");
    ctx->str_s1 = stringcache(ctx->strcache, "s1");
    ctx->str_s2 = stringcache(ctx->strcache, "s2");
    ctx->str_s3 = stringcache(ctx->strcache, "s3");
    ctx->str_sc = stringcache(ctx->strcache, "sc");
    ctx->str_ss = stringcache(ctx->strcache, "ss");
    ctx->str_sS = stringcache(ctx->strcache, "sS");
    ctx->str_Fs = stringcache(ctx->strcache, "Fs");
    ctx->str_Fu = stringcache(ctx->strcache, "Fu");
    ctx->str_ns = stringcache(ctx->strcache, "ns");
    ctx->str_nu = stringcache(ctx->strcache, "nu");

    return ctx;
} // build_context


// parse the source code into an AST.
static void parse_source(Context *ctx, const char *filename,
                        const char *source, unsigned int sourcelen,
                        const MOJOSHADER_preprocessorDefine *defines,
                        unsigned int define_count,
                        MOJOSHADER_includeOpen include_open,
                        MOJOSHADER_includeClose include_close)
{
    TokenData data;
    unsigned int tokenlen;
    Token tokenval;
    const char *token;
    int lemon_token;
    const char *fname;
    Preprocessor *pp;
    void *parser;

    if (!include_open) include_open = MOJOSHADER_internal_include_open;
    if (!include_close) include_close = MOJOSHADER_internal_include_close;

    pp = preprocessor_start(filename, source, sourcelen, include_open,
                            include_close, defines, define_count, 0,
                            ctx->malloc, ctx->free, ctx->malloc_data);

    // !!! FIXME: check if (pp == NULL)...

    parser = ParseHLSLAlloc(ctx->malloc, ctx->malloc_data);

    // !!! FIXME: check if (parser == NULL)...

    #if DEBUG_COMPILER_PARSER
    ParseHLSLTrace(stdout, "COMPILER: ");
    #endif

    // add in standard typedefs...
    static char *types[] = { "bool", "int", "uint", "half", "float", "double" };
    int i;
    for (i = 0; i < STATICARRAYLEN(types); i++)
    {
        char buf[32];
        int j;
        for (j = 1; j <= 4; j++)
        {
            // "float2"
            int len = snprintf(buf, sizeof (buf), "%s%d", types[i], j);
            push_usertype(ctx, stringcache_len(ctx->strcache, buf, len), NULL);
            int k;
            for (k = 1; k <= 4; k++)
            {
                // "float2x2"
                len = snprintf(buf, sizeof (buf), "%s%dx%d", types[i], j, k);
                push_usertype(ctx, stringcache_len(ctx->strcache, buf, len), NULL);
            } // for
        } // for
    } // for

    // Run the preprocessor/lexer/parser...
    int is_pragma = 0;   // !!! FIXME: remove this later when we can parse #pragma.
    int skipping = 0; // !!! FIXME: remove this later when we can parse #pragma.
    do {
        token = preprocessor_nexttoken(pp, &tokenlen, &tokenval);

        if (preprocessor_outofmemory(pp))
        {
            out_of_memory(ctx);
            break;
        } // if

        fname = preprocessor_sourcepos(pp, &ctx->sourceline);
        ctx->sourcefile = fname ? stringcache(ctx->strcache, fname) : 0;

        if ((tokenval == TOKEN_HASH) || (tokenval == TOKEN_HASHHASH))
            tokenval = TOKEN_BAD_CHARS;

        if (tokenval == TOKEN_BAD_CHARS)
        {
            fail(ctx, "Bad characters in source file");
            continue;
        } // else if

        else if (tokenval == TOKEN_PREPROCESSING_ERROR)
        {
            fail(ctx, token);  // this happens to be null-terminated.
            continue;
        } // else if

        else if (tokenval == TOKEN_PP_PRAGMA)
        {
            assert(!is_pragma);
            is_pragma = 1;
            skipping = 1;
            continue;
        }

        else if (tokenval == ((Token) '\n'))
        {
            assert(is_pragma);
            is_pragma = 0;
            skipping = 0;
            continue;
        }

        else if (skipping)
        {
            continue;
        }

        lemon_token = convert_to_lemon_token(ctx, token, tokenlen, tokenval);
        switch (lemon_token)
        {
            case TOKEN_HLSL_INT_CONSTANT:
                data.i64 = strtoi64(token, tokenlen);
                break;

            case TOKEN_HLSL_FLOAT_CONSTANT:
                data.dbl = strtodouble(token, tokenlen);
                break;

            case TOKEN_HLSL_USERTYPE:
            case TOKEN_HLSL_STRING_LITERAL:
            case TOKEN_HLSL_IDENTIFIER:
                data.string = stringcache_len(ctx->strcache, token, tokenlen);
                break;

            default:
                data.i64 = 0;
                break;
        } // switch

        ParseHLSL(parser, lemon_token, data, ctx);

        // this probably isn't perfect, but it's good enough for surviving
        //  the parse. We'll sort out correctness once we have a tree.
        if (lemon_token == TOKEN_HLSL_LBRACE)
            push_scope(ctx);
        else if (lemon_token == TOKEN_HLSL_RBRACE)
            pop_scope(ctx);
    } while (tokenval != TOKEN_EOI);

    ParseHLSLFree(parser, ctx->free, ctx->malloc_data);
    preprocessor_end(pp);
} // parse_source


static MOJOSHADER_astData MOJOSHADER_out_of_mem_ast_data = {
    1, &MOJOSHADER_out_of_mem_error, 0, 0, 0, 0, 0
};

// !!! FIXME: cut and paste from assembler.
static MOJOSHADER_error *build_errors(Context *ctx)
{
    int total = 0;
    MOJOSHADER_error *retval = (MOJOSHADER_error *)
            Malloc(ctx, sizeof (MOJOSHADER_error) * ctx->error_count);
    if (retval == NULL)
        return NULL;

    ErrorList *item = ctx->errors;
    while (item != NULL)
    {
        ErrorList *next = item->next;
        // reuse the string allocations
        memcpy(&retval[total], &item->error, sizeof (MOJOSHADER_error));
        Free(ctx, item);
        item = next;
        total++;
    } // while
    ctx->errors = NULL;

    assert(total == ctx->error_count);
    return retval;
} // build_errors


// !!! FIXME: cut and paste from assembler.
static const MOJOSHADER_astData *build_failed_ast(Context *ctx)
{
    assert(isfail(ctx));

    if (ctx->out_of_memory)
        return &MOJOSHADER_out_of_mem_ast_data;
        
    MOJOSHADER_astData *retval = NULL;
    retval = (MOJOSHADER_astData *) Malloc(ctx, sizeof (MOJOSHADER_astData));
    if (retval == NULL)
        return &MOJOSHADER_out_of_mem_ast_data;

    memset(retval, '\0', sizeof (MOJOSHADER_astData));
    retval->malloc = (ctx->malloc == MOJOSHADER_internal_malloc) ? NULL : ctx->malloc;
    retval->free = (ctx->free == MOJOSHADER_internal_free) ? NULL : ctx->free;
    retval->malloc_data = ctx->malloc_data;

    retval->error_count = ctx->error_count;
    retval->errors = build_errors(ctx);
    if ((retval->errors == NULL) && (ctx->error_count > 0))
    {
        Free(ctx, retval);
        return &MOJOSHADER_out_of_mem_ast_data;
    } // if

    return retval;
} // build_failed_ast


static const MOJOSHADER_astData *build_astdata(Context *ctx)
{
    MOJOSHADER_astData *retval = NULL;

    if (ctx->out_of_memory)
        return &MOJOSHADER_out_of_mem_ast_data;

    retval = (MOJOSHADER_astData *) Malloc(ctx, sizeof (MOJOSHADER_astData));
    if (retval == NULL)
        return &MOJOSHADER_out_of_mem_ast_data;

    memset(retval, '\0', sizeof (MOJOSHADER_astData));

    if (!isfail(ctx))
    {
        retval->source_profile = ctx->source_profile;
        ctx->source_profile = NULL;  // don't free this with the context, now.
        retval->ast = ctx->ast;
        ctx->ast = NULL;  // don't free this with the context, now.
    } // if

    retval->error_count = ctx->error_count;
    retval->errors = build_errors(ctx);
    retval->malloc = (ctx->malloc == MOJOSHADER_internal_malloc) ? NULL : ctx->malloc;
    retval->free = (ctx->free == MOJOSHADER_internal_free) ? NULL : ctx->free;
    retval->malloc_data = ctx->malloc_data;

    return retval;
} // build_astdata

static void choose_src_profile(Context *ctx, const char *srcprofile)
{
    #define TEST_PROFILE(x) do { \
        if (strcmp(srcprofile, x) == 0) { \
            ctx->source_profile = x; \
            return; \
        } \
    } while (0)

    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_VS_1_1);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_VS_2_0);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_VS_3_0);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_PS_1_1);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_PS_1_2);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_PS_1_3);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_PS_1_4);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_PS_2_0);
    TEST_PROFILE(MOJOSHADER_SRC_PROFILE_HLSL_PS_3_0);

    #undef TEST_PROFILE

    fail(ctx, "Unknown profile");
} // choose_src_profile


// API entry point...

// !!! FIXME: move this (and a lot of other things) to mojoshader_ast.c.
const MOJOSHADER_astData *MOJOSHADER_parseAst(const char *srcprofile,
                                    const char *filename, const char *source,
                                    unsigned int sourcelen,
                                    const MOJOSHADER_preprocessorDefine *defs,
                                    unsigned int define_count,
                                    MOJOSHADER_includeOpen include_open,
                                    MOJOSHADER_includeClose include_close,
                                    MOJOSHADER_malloc m, MOJOSHADER_free f,
                                    void *d)
{
    const MOJOSHADER_astData *retval = NULL;
    Context *ctx = NULL;

    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &MOJOSHADER_out_of_mem_ast_data;  // supply both or neither.

    ctx = build_context(m, f, d);
    if (ctx == NULL)
        return &MOJOSHADER_out_of_mem_ast_data;

    choose_src_profile(ctx, srcprofile);

    if (!isfail(ctx))
    {
        parse_source(ctx, filename, source, sourcelen, defs, define_count,
                     include_open, include_close);
    } // if

    if (isfail(ctx))
        retval = (MOJOSHADER_astData *) build_failed_ast(ctx);
    else
        retval = build_astdata(ctx);

    destroy_context(ctx);
    return retval;
} // MOJOSHADER_parseAst


void MOJOSHADER_freeAstData(const MOJOSHADER_astData *_data)
{
    MOJOSHADER_astData *data = (MOJOSHADER_astData *) _data;
    if ((data == NULL) || (data == &MOJOSHADER_out_of_mem_ast_data))
        return;  // no-op.

    MOJOSHADER_free f = (data->free == NULL) ? MOJOSHADER_internal_free : data->free;
    void *d = data->malloc_data;
    int i;

    // we don't f(data->source_profile), because that's internal static data.

    // check for NULL in case of dumb free() impl.
    if (data->errors != NULL)
    {
        for (i = 0; i < data->error_count; i++)
        {
            if (data->errors[i].error != NULL)
                f((void *) data->errors[i].error, d);
            if (data->errors[i].filename != NULL)
                f((void *) data->errors[i].filename, d);
        } // for
        f((void *) data->errors, d);
    } // if

    if (data->ast != NULL)
    {
        // !!! FIXME: make this not require a Context.
        Context ctx;
        memset(&ctx, '\0', sizeof (Context));
        ctx.malloc = data->malloc;
        ctx.free = f;
        ctx.malloc_data = d;
        delete_compilation_unit(&ctx,
                    (MOJOSHADER_astCompilationUnit *) &data->ast->compunit);
    } // if

    f(data, d);
} // MOJOSHADER_freeAstData



const MOJOSHADER_compileData *MOJOSHADER_compile(const char *srcprofile,
                                    const char *filename, const char *source,
                                    unsigned int sourcelen,
                                    const MOJOSHADER_preprocessorDefine *defs,
                                    unsigned int define_count,
                                    MOJOSHADER_includeOpen include_open,
                                    MOJOSHADER_includeClose include_close,
                                    MOJOSHADER_malloc m, MOJOSHADER_free f,
                                    void *d)
{
    Context *ctx = build_context(m, f, d);
    if (!ctx)
        return NULL;  // !!! FIXME: report error.

    parse_source(ctx, filename, source, sourcelen, defs, define_count,
                 include_open, include_close);

    // !!! FIXME: check (ctx->ast != NULL), and maybe isfail().

    semantic_analysis(ctx);

    destroy_context(ctx);

    // !!! FIXME: report success/error.
    return NULL;
} // MOJOSHADER_compile

// end of mojoshader_compiler.c ...

