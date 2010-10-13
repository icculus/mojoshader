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

typedef union TokenData
{
    int64 i64;
    double dbl;
    const char *string;
} TokenData;


// Structures that make up the parse tree...

typedef enum ASTNodeType
{
    AST_OP_START_RANGE,
    AST_OP_START_RANGE_UNARY,
    AST_OP_POSTINCREMENT,
    AST_OP_POSTDECREMENT,
    AST_OP_PREINCREMENT,
    AST_OP_PREDECREMENT,
    AST_OP_NEGATE,
    AST_OP_COMPLEMENT,
    AST_OP_NOT,
    AST_OP_END_RANGE_UNARY,

    AST_OP_START_RANGE_BINARY,
    AST_OP_DEREF_ARRAY,
    AST_OP_CALLFUNC,
    AST_OP_DEREF_STRUCT,
    AST_OP_COMMA,
    AST_OP_MULTIPLY,
    AST_OP_DIVIDE,
    AST_OP_MODULO,
    AST_OP_ADD,
    AST_OP_SUBTRACT,
    AST_OP_LSHIFT,
    AST_OP_RSHIFT,
    AST_OP_LESSTHAN,
    AST_OP_GREATERTHAN,
    AST_OP_LESSTHANOREQUAL,
    AST_OP_GREATERTHANOREQUAL,
    AST_OP_EQUAL,
    AST_OP_NOTEQUAL,
    AST_OP_BINARYAND,
    AST_OP_BINARYXOR,
    AST_OP_BINARYOR,
    AST_OP_LOGICALAND,
    AST_OP_LOGICALOR,
    AST_OP_ASSIGN,
    AST_OP_MULASSIGN,
    AST_OP_DIVASSIGN,
    AST_OP_MODASSIGN,
    AST_OP_ADDASSIGN,
    AST_OP_SUBASSIGN,
    AST_OP_LSHIFTASSIGN,
    AST_OP_RSHIFTASSIGN,
    AST_OP_ANDASSIGN,
    AST_OP_XORASSIGN,
    AST_OP_ORASSIGN,
    AST_OP_END_RANGE_BINARY,

    AST_OP_START_RANGE_TERNARY,
    AST_OP_CONDITIONAL,
    AST_OP_END_RANGE_TERNARY,

    AST_OP_START_RANGE_DATA,
    AST_OP_IDENTIFIER,
    AST_OP_INT_LITERAL,
    AST_OP_FLOAT_LITERAL,
    AST_OP_STRING_LITERAL,
    AST_OP_END_RANGE_DATA,

    AST_OP_START_RANGE_MISC,
    AST_OP_CONSTRUCTOR,
    AST_OP_CAST,
    AST_OP_END_RANGE_MISC,
    AST_OP_END_RANGE,

    AST_COMPUNIT_START_RANGE,
    AST_COMPUNIT_FUNCTION,    // function declaration or definition
    AST_COMPUNIT_TYPEDEF,     // typedef or struct
    AST_COMPUNIT_STRUCT,      // global struct
    AST_COMPUNIT_VARIABLE,    // global variable
    AST_COMPUNIT_END_RANGE,

    AST_STATEMENT_START_RANGE,
    AST_STATEMENT_BLOCK,
    AST_STATEMENT_EMPTY,
    AST_STATEMENT_EXPRESSION,
    AST_STATEMENT_IF,
    AST_STATEMENT_SWITCH,
    AST_STATEMENT_FOR,
    AST_STATEMENT_DO,
    AST_STATEMENT_WHILE,
    AST_STATEMENT_RETURN,
    AST_STATEMENT_BREAK,
    AST_STATEMENT_CONTINUE,
    AST_STATEMENT_DISCARD,
    AST_STATEMENT_TYPEDEF,
    AST_STATEMENT_STRUCT,
    AST_STATEMENT_VARDECL,
    AST_STATEMENT_END_RANGE,

    AST_MISC_START_RANGE,
    AST_FUNCTION_ARGS,
    AST_FUNCTION_SIGNATURE,
    AST_SCALAR_OR_ARRAY,
    AST_TYPEDEF,
    AST_PACK_OFFSET,
    AST_VARIABLE_LOWLEVEL,
    AST_ANNOTATION,
    AST_VARIABLE_DECLARATION,
    AST_STRUCT_DECLARATION,
    AST_STRUCT_MEMBER,
    AST_SWITCH_CASE,
    AST_MISC_END_RANGE,

    AST_END_RANGE
} ASTNodeType;

typedef struct ASTNode
{
    ASTNodeType type;
    const char *filename;
    uint32 line;
} ASTNode;

typedef enum VariableAttributes
{
    VARATTR_EXTERN = (1 << 0),
    VARATTR_NOINTERPOLATION = (1 << 1),
    VARATTR_SHARED = (1 << 2),
    VARATTR_STATIC = (1 << 3),
    VARATTR_UNIFORM = (1 << 4),
    VARATTR_VOLATILE = (1 << 5),
    VARATTR_CONST = (1 << 6),
    VARATTR_ROWMAJOR = (1 << 7),
    VARATTR_COLUMNMAJOR = (1 << 8)
} VariableAttributes;

typedef enum IfAttributes
{
    IFATTR_NONE,
    IFATTR_BRANCH,
    IFATTR_FLATTEN,
    IFATTR_IFALL,
    IFATTR_IFANY,
    IFATTR_PREDICATE,
    IFATTR_PREDICATEBLOCK,
} IfAttributes;

typedef enum SwitchAttributes
{
    SWITCHATTR_NONE,
    SWITCHATTR_FLATTEN,
    SWITCHATTR_BRANCH,
    SWITCHATTR_FORCECASE,
    SWITCHATTR_CALL
} SwitchAttributes;

static inline int operator_is_unary(const ASTNodeType op)
{
    return ((op > AST_OP_START_RANGE_UNARY) && (op < AST_OP_END_RANGE_UNARY));
} // operator_is_unary

static inline int operator_is_binary(const ASTNodeType op)
{
    return ((op > AST_OP_START_RANGE_BINARY) && (op < AST_OP_END_RANGE_BINARY));
} // operator_is_binary

static inline int operator_is_ternary(const ASTNodeType op)
{
    return ((op > AST_OP_START_RANGE_TERNARY) && (op < AST_OP_END_RANGE_TERNARY));
} // operator_is_ternary

typedef struct ASTGeneric
{
    ASTNode ast;
} ASTGeneric;

typedef ASTGeneric Expression;

typedef struct ExpressionUnary
{
    ASTNode ast;
    Expression *operand;
} ExpressionUnary;

typedef struct ExpressionBinary
{
    ASTNode ast;
    Expression *left;
    Expression *right;
} ExpressionBinary;

typedef struct ExpressionTernary
{
    ASTNode ast;
    Expression *left;
    Expression *center;
    Expression *right;
} ExpressionTernary;

typedef struct ExpressionIdentifier
{
    ASTNode ast;  // Always AST_OP_IDENTIFIER
    const char *identifier;
} ExpressionIdentifier;

typedef struct ExpressionIntLiteral
{
    ASTNode ast;  // Always AST_OP_INT_LITERAL
    int64 value;
} ExpressionIntLiteral;

typedef struct ExpressionFloatLiteral
{
    ASTNode ast;  // Always AST_OP_FLOAT_LITERAL
    double value;
} ExpressionFloatLiteral;

typedef struct ExpressionStringLiteral
{
    ASTNode ast;  // Always AST_OP_STRING_LITERAL
    const char *string;
} ExpressionStringLiteral;

typedef struct ExpressionConstructor
{
    ASTNode ast;  // Always AST_OP_CONSTRUCTOR
    const char *datatype;
    Expression *args;
} ExpressionConstructor;

typedef struct ExpressionCast
{
    ASTNode ast;  // Always AST_OP_CAST
    const char *datatype;
    Expression *operand;
} ExpressionCast;

typedef struct CompilationUnit
{
    ASTNode ast;
    struct CompilationUnit *next;
} CompilationUnit;

typedef enum FunctionStorageClass
{
    FNSTORECLS_NONE,
    FNSTORECLS_INLINE
} FunctionStorageClass;

typedef enum InputModifier
{
    INPUTMOD_NONE,
    INPUTMOD_IN,
    INPUTMOD_OUT,
    INPUTMOD_INOUT,
    INPUTMOD_UNIFORM
} InputModifier;

typedef enum InterpolationModifier
{
    INTERPMOD_NONE,
    INTERPMOD_LINEAR,
    INTERPMOD_CENTROID,
    INTERPMOD_NOINTERPOLATION,
    INTERPMOD_NOPERSPECTIVE,
    INTERPMOD_SAMPLE
} InterpolationModifier;

typedef struct FunctionArguments
{
    ASTNode ast;
    InputModifier input_modifier;
    const char *datatype;
    const char *identifier;
    const char *semantic;
    InterpolationModifier interpolation_modifier;
    Expression *initializer;
    struct FunctionArguments *next;
} FunctionArguments;

typedef struct FunctionSignature
{
    ASTNode ast;
    const char *datatype;
    const char *identifier;
    FunctionArguments *args;
    FunctionStorageClass storage_class;
    const char *semantic;
} FunctionSignature;

typedef struct ScalarOrArray
{
    ASTNode ast;
    const char *identifier;
    int isarray;
    Expression *dimension;
} ScalarOrArray;

typedef struct Annotations
{
    ASTNode ast;
    const char *datatype;
    Expression *initializer;
    struct Annotations *next;
} Annotations;

typedef struct PackOffset
{
    ASTNode ast;
    const char *ident1;   // !!! FIXME: rename this.
    const char *ident2;
} PackOffset;

typedef struct VariableLowLevel
{
    ASTNode ast;
    PackOffset *packoffset;
    const char *register_name;
} VariableLowLevel;

typedef struct StructMembers
{
    ASTNode ast;
    const char *datatype;
    const char *semantic;
    ScalarOrArray *details;
    InterpolationModifier interpolation_mod;
    struct StructMembers *next;
} StructMembers;

typedef struct StructDeclaration
{
    ASTNode ast;
    const char *name;
    StructMembers *members;
} StructDeclaration;

typedef struct VariableDeclaration
{
    ASTNode ast;
    int attributes;
    const char *datatype;
    StructDeclaration *anonymous_datatype;
    ScalarOrArray *details;
    const char *semantic;
    Annotations *annotations;
    Expression *initializer;
    VariableLowLevel *lowlevel;
    struct VariableDeclaration *next;
} VariableDeclaration;

typedef struct Statement
{
    ASTNode ast;
    struct Statement *next;
} Statement;

typedef Statement EmptyStatement;
typedef Statement BreakStatement;
typedef Statement ContinueStatement;
typedef Statement DiscardStatement;

typedef struct BlockStatement  // something enclosed in "{}" braces.
{
    ASTNode ast;
    struct Statement *next;
    Statement *statements;  // list of statements enclosed by this block.
} BlockStatement;

typedef struct ReturnStatement
{
    ASTNode ast;
    struct Statement *next;
    Expression *expr;
} ReturnStatement;

typedef struct ExpressionStatement
{
    ASTNode ast;
    struct Statement *next;
    Expression *expr;
} ExpressionStatement;

typedef struct IfStatement
{
    ASTNode ast;
    struct Statement *next;
    int attributes;
    Expression *expr;
    Statement *statement;
    Statement *else_statement;
} IfStatement;

typedef struct SwitchCases
{
    ASTNode ast;
    Expression *expr;
    Statement *statement;
    struct SwitchCases *next;
} SwitchCases;

typedef struct SwitchStatement
{
    ASTNode ast;
    struct Statement *next;
    int attributes;
    Expression *expr;
    SwitchCases *cases;
} SwitchStatement;

typedef struct WhileStatement
{
    ASTNode ast;
    struct Statement *next;
    int64 unroll;  // # times to unroll, 0 to loop, negative for compiler's choice.
    Expression *expr;
    Statement *statement;
} WhileStatement;

typedef WhileStatement DoStatement;

typedef struct ForStatement
{
    ASTNode ast;
    struct Statement *next;
    int64 unroll;  // # times to unroll, 0 to loop, negative for compiler's choice.
    VariableDeclaration *var_decl;
    Expression *initializer;
    Expression *looptest;
    Expression *counter;
    Statement *statement;
} ForStatement;

typedef struct Typedef
{
    ASTNode ast;
    int isconst;
    const char *datatype;
    ScalarOrArray *details;
} Typedef;

typedef struct TypedefStatement
{
    ASTNode ast;
    struct Statement *next;
    Typedef *type_info;
} TypedefStatement;

typedef struct VarDeclStatement
{
    ASTNode ast;
    struct Statement *next;
    VariableDeclaration *declaration;
} VarDeclStatement;

typedef struct StructStatement
{
    ASTNode ast;
    struct Statement *next;
    StructDeclaration *struct_info;
} StructStatement;

typedef struct CompilationUnitFunction
{
    ASTNode ast;
    struct CompilationUnit *next;
    FunctionSignature *declaration;
    Statement *definition;
} CompilationUnitFunction;

typedef struct CompilationUnitTypedef
{
    ASTNode ast;
    struct CompilationUnit *next;
    Typedef *type_info;
} CompilationUnitTypedef;

typedef struct CompilationUnitStruct
{
    ASTNode ast;
    struct CompilationUnit *next;
    StructDeclaration *struct_info;
} CompilationUnitStruct;

typedef struct CompilationUnitVariable
{
    ASTNode ast;
    struct CompilationUnit *next;
    VariableDeclaration *declaration;
} CompilationUnitVariable;


// This tracks typedefs and structs, and notes when they enter/leave scope.

typedef struct UserTypeScopeStack
{
    const char *symbol;
    const char *datatype;
    struct UserTypeScopeStack *next;
} UserTypeScopeStack;

typedef struct UserTypeMap
{
    HashTable *types;
    UserTypeScopeStack *scope;
} UserTypeMap;


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
    UserTypeMap usertypes;
    CompilationUnit *ast;  // Abstract Syntax Tree
} Context;


// Convenience functions for allocators...

static inline void out_of_memory(Context *ctx)
{
    if (!ctx->out_of_memory) printf("out of memory\n");  // !!! FIXME: placeholder.
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

static void fail(Context *ctx, const char *str)
{
    // !!! FIXME: placeholder.
    (void) ctx;
    printf("%s:%u: %s\n", ctx->sourcefile, ctx->sourceline, str);
} // fail


static void usertypemap_nuke(const void *k, const void *v, void *d) {/*no-op*/}

static int create_usertypemap(Context *ctx)
{
    UserTypeMap *map = &ctx->usertypes;

    map->scope = NULL;
    map->types = hash_create(ctx, hash_hash_string, hash_keymatch_string,
                             usertypemap_nuke, 1, ctx->malloc, ctx->free,
                             ctx->malloc_data);
    if (!map->types)
    {
        out_of_memory(ctx);
        return 0;
    } // if

    return 1;
} // create_usertypemap

static void push_usertype(Context *ctx, const char *sym, const char *datatype)
{
    UserTypeMap *map = &ctx->usertypes;
    UserTypeScopeStack *item;

    item = (UserTypeScopeStack *) Malloc(ctx, sizeof (UserTypeScopeStack));
    if (item == NULL)
        return;

    if (sym != NULL)
    {
        if (hash_insert(map->types, sym, datatype) == -1)
        {
            Free(ctx, item);
            return;
        }
    } // if

    item->symbol = sym;  // cached strings, don't copy.
    item->datatype = datatype;
    item->next = map->scope;
    map->scope = item;
} // push_usertype

static void pop_usertype(Context *ctx)
{
    UserTypeMap *map = &ctx->usertypes;
    UserTypeScopeStack *item = map->scope;
    if (!item)
        return;
    if (item->symbol)
        hash_remove(map->types, item->symbol);
    map->scope = item->next;
    Free(ctx, item);
} // pop_usertype

static void push_scope(Context *ctx)
{
    push_usertype(ctx, NULL, NULL);
} // push_scope

static void pop_scope(Context *ctx)
{
    UserTypeMap *map = &ctx->usertypes;
    while ((map->scope) && (map->scope->symbol))
        pop_usertype(ctx);

    assert(map->scope != NULL);
    assert(map->scope->symbol == NULL);
    pop_usertype(ctx);
} // push_scope

static void destroy_usertypemap(Context *ctx)
{
    UserTypeMap *map = &ctx->usertypes;
    while (map->scope)
        pop_usertype(ctx);
    hash_destroy(map->types);
} // destroy_usertypemap


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

static void delete_compilation_unit(Context *ctx, CompilationUnit *unit);
static void delete_statement(Context *ctx, Statement *stmt);

static Expression *new_constructor_expr(Context *ctx, const char *datatype,
                                        Expression *args)
{
    NEW_AST_NODE(retval, ExpressionConstructor, AST_OP_CONSTRUCTOR);
    retval->datatype = datatype;
    retval->args = args;
    return (Expression *) retval;
} // new_constructor_expr

static Expression *new_cast_expr(Context *ctx, const char *datatype,
                                 Expression *operand)
{
    NEW_AST_NODE(retval, ExpressionCast, AST_OP_CAST);
    retval->datatype = datatype;
    retval->operand = operand;
    return (Expression *) retval;
} // new_cast_expr

static Expression *new_unary_expr(Context *ctx, const ASTNodeType op,
                                  Expression *operand)
{
    NEW_AST_NODE(retval, ExpressionUnary, op);
    assert(operator_is_unary(op));
    retval->operand = operand;
    return (Expression *) retval;
} // new_unary_expr

static Expression *new_binary_expr(Context *ctx, const ASTNodeType op,
                                   Expression *left, Expression *right)
{
    NEW_AST_NODE(retval, ExpressionBinary, op);
    assert(operator_is_binary(op));
    retval->left = left;
    retval->right = right;
    return (Expression *) retval;
} // new_binary_expr

static Expression *new_ternary_expr(Context *ctx, const ASTNodeType op,
                                    Expression *left, Expression *center,
                                    Expression *right)
{
    NEW_AST_NODE(retval, ExpressionTernary, op);
    assert(operator_is_ternary(op));
    retval->left = left;
    retval->center = center;
    retval->right = right;
    return (Expression *) retval;
} // new_ternary_expr

static Expression *new_identifier_expr(Context *ctx, const char *string)
{
    NEW_AST_NODE(retval, ExpressionIdentifier, AST_OP_IDENTIFIER);
    retval->identifier = string;  // cached; don't copy string.
    return (Expression *) retval;
} // new_identifier_expr

static Expression *new_literal_int_expr(Context *ctx, const int64 value)
{
    NEW_AST_NODE(retval, ExpressionIntLiteral, AST_OP_INT_LITERAL);
    retval->value = value;
    return (Expression *) retval;
} // new_literal_int_expr

static Expression *new_literal_float_expr(Context *ctx, const double dbl)
{
    NEW_AST_NODE(retval, ExpressionFloatLiteral, AST_OP_FLOAT_LITERAL);
    retval->value = dbl;
    return (Expression *) retval;
} // new_literal_float_expr

static Expression *new_literal_string_expr(Context *ctx, const char *string)
{
    NEW_AST_NODE(retval, ExpressionStringLiteral, AST_OP_STRING_LITERAL);
    retval->string = string;  // cached; don't copy string.
    return (Expression *) retval;
} // new_string_literal_expr

static void delete_expr(Context *ctx, Expression *expr)
{
    DELETE_AST_NODE(expr);
    if (operator_is_unary(expr->ast.type))
    {
        const ExpressionUnary *unary = (const ExpressionUnary *) expr;
        delete_expr(ctx, unary->operand);
    } // if
    else if (operator_is_binary(expr->ast.type))
    {
        const ExpressionBinary *binary = (const ExpressionBinary *) expr;
        delete_expr(ctx, binary->left);
        delete_expr(ctx, binary->right);
    } // else if
    else if (operator_is_ternary(expr->ast.type))
    {
        const ExpressionTernary *ternary = (const ExpressionTernary *) expr;
        delete_expr(ctx, ternary->left);
        delete_expr(ctx, ternary->center);
        delete_expr(ctx, ternary->right);
    } // else if
    else if (expr->ast.type == AST_OP_CAST)
    {
        delete_expr(ctx, ((ExpressionCast *) expr)->operand);
    } // else if
    else if (expr->ast.type == AST_OP_CONSTRUCTOR)
    {
        delete_expr(ctx, ((ExpressionConstructor *) expr)->args);
    } // else if

    // rest of operators don't have extra data to free.

    Free(ctx, expr);
} // delete_expr

static FunctionArguments *new_function_arg(Context *ctx,
                                        const InputModifier inputmod,
                                        const char *datatype,
                                        const char *identifier,
                                        const char *semantic,
                                        const InterpolationModifier interpmod,
                                        Expression *initializer)
{
    NEW_AST_NODE(retval, FunctionArguments, AST_FUNCTION_ARGS);
    retval->input_modifier = inputmod;
    retval->datatype = datatype;
    retval->identifier = identifier;
    retval->semantic = semantic;
    retval->interpolation_modifier = interpmod;
    retval->initializer = initializer;
    retval->next = NULL;
    return retval;
} // new_function_arg

static void delete_function_args(Context *ctx, FunctionArguments *args)
{
    DELETE_AST_NODE(args);
    delete_function_args(ctx, args->next);
    delete_expr(ctx, args->initializer);
    Free(ctx, args);
} // delete_function_args

static FunctionSignature *new_function_signature(Context *ctx,
                                                 const char *datatype,
                                                 const char *identifier,
                                                 FunctionArguments *args)
{
    NEW_AST_NODE(retval, FunctionSignature, AST_FUNCTION_SIGNATURE);
    retval->datatype = datatype;
    retval->identifier = identifier;
    retval->args = args;
    retval->storage_class = FNSTORECLS_NONE;
    retval->semantic = NULL;
    return retval;
} // new_function_signature

static void delete_function_signature(Context *ctx, FunctionSignature *sig)
{
    DELETE_AST_NODE(sig);
    delete_function_args(ctx, sig->args);
    Free(ctx, sig);
} // delete_function_signature

static CompilationUnit *new_function(Context *ctx,
                                     FunctionSignature *declaration,
                                     Statement *definition)
{
    NEW_AST_NODE(retval, CompilationUnitFunction, AST_COMPUNIT_FUNCTION);
    retval->next = NULL;
    retval->declaration = declaration;
    retval->definition = definition;
    return (CompilationUnit *) retval;
} // new_function

static void delete_function(Context *ctx, CompilationUnitFunction *unitfn)
{
    DELETE_AST_NODE(unitfn);
    delete_compilation_unit(ctx, unitfn->next);
    delete_function_signature(ctx, unitfn->declaration);
    delete_statement(ctx, unitfn->definition);
    Free(ctx, unitfn);
} // delete_function

static ScalarOrArray *new_scalar_or_array(Context *ctx, const char *ident,
                                          const int isvec, Expression *dim)
{
    NEW_AST_NODE(retval, ScalarOrArray, AST_SCALAR_OR_ARRAY);
    retval->identifier = ident;
    retval->isarray = isvec;
    retval->dimension = dim;
    return retval;
} // new_scalar_or_array

static void delete_scalar_or_array(Context *ctx, ScalarOrArray *soa)
{
    DELETE_AST_NODE(soa);
    delete_expr(ctx, soa->dimension);
    Free(ctx, soa);
} // delete_scalar_or_array

static Typedef *new_typedef(Context *ctx, int isconst, const char *datatype,
                            ScalarOrArray *soa)
{
    NEW_AST_NODE(retval, Typedef, AST_TYPEDEF);
    retval->isconst = isconst;
    retval->datatype = datatype;
    retval->details = soa;
    return retval;
} // new_typedef

static void delete_typedef(Context *ctx, Typedef *td)
{
    DELETE_AST_NODE(td);
    delete_scalar_or_array(ctx, td->details);
    Free(ctx, td);
} // delete_typedef

static PackOffset *new_pack_offset(Context *ctx, const char *a, const char *b)
{
    NEW_AST_NODE(retval, PackOffset, AST_PACK_OFFSET);
    retval->ident1 = a;
    retval->ident2 = b;
    return retval;
} // new_pack_offset

static void delete_pack_offset(Context *ctx, PackOffset *o)
{
    DELETE_AST_NODE(o);
    Free(ctx, o);
} // delete_pack_offset

static VariableLowLevel *new_variable_lowlevel(Context *ctx, PackOffset *po,
                                               const char *reg)
{
    NEW_AST_NODE(retval, VariableLowLevel, AST_VARIABLE_LOWLEVEL);
    retval->packoffset = po;
    retval->register_name = reg;
    return retval;
} // new_variable_lowlevel

static void delete_variable_lowlevel(Context *ctx, VariableLowLevel *vll)
{
    DELETE_AST_NODE(vll);
    delete_pack_offset(ctx, vll->packoffset);
    Free(ctx, vll);
} // delete_variable_lowlevel

static Annotations *new_annotation(Context *ctx, const char *datatype,
                                   Expression *initializer)
{
    NEW_AST_NODE(retval, Annotations, AST_ANNOTATION);
    retval->datatype = datatype;
    retval->initializer = initializer;
    retval->next = NULL;
    return retval;
} // new_annotation

static void delete_annotation(Context *ctx, Annotations *annotations)
{
    DELETE_AST_NODE(annotations);
    delete_annotation(ctx, annotations->next);
    delete_expr(ctx, annotations->initializer);
    Free(ctx, annotations);
} // delete_annotation

static VariableDeclaration *new_variable_declaration(Context *ctx,
                                    ScalarOrArray *soa, const char *semantic,
                                    Annotations *annotations, Expression *init,
                                    VariableLowLevel *vll)
{
    NEW_AST_NODE(retval, VariableDeclaration, AST_VARIABLE_DECLARATION);
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

static void delete_variable_declaration(Context *ctx, VariableDeclaration *dcl)
{
    DELETE_AST_NODE(dcl);
    delete_variable_declaration(ctx, dcl->next);
    delete_scalar_or_array(ctx, dcl->details);
    delete_annotation(ctx, dcl->annotations);
    delete_expr(ctx, dcl->initializer);
    delete_variable_lowlevel(ctx, dcl->lowlevel);
    Free(ctx, dcl);
} // delete_variable_declaration

static CompilationUnit *new_global_variable(Context *ctx,
                                            VariableDeclaration *decl)
{
    NEW_AST_NODE(retval, CompilationUnitVariable, AST_COMPUNIT_VARIABLE);
    retval->next = NULL;
    retval->declaration = decl;
    return (CompilationUnit *) retval;
} // new_global_variable

static void delete_global_variable(Context *ctx, CompilationUnitVariable *var)
{
    DELETE_AST_NODE(var);
    delete_compilation_unit(ctx, var->next);
    delete_variable_declaration(ctx, var->declaration);
    Free(ctx, var);
} // delete_global_variable

static CompilationUnit *new_global_typedef(Context *ctx, Typedef *td)
{
    NEW_AST_NODE(retval, CompilationUnitTypedef, AST_COMPUNIT_TYPEDEF);
    retval->next = NULL;
    retval->type_info = td;
    return (CompilationUnit *) retval;
} // new_global_typedef

static void delete_global_typedef(Context *ctx, CompilationUnitTypedef *unit)
{
    DELETE_AST_NODE(unit);
    delete_compilation_unit(ctx, unit->next);
    delete_typedef(ctx, unit->type_info);
    Free(ctx, unit);
} // delete_global_typedef

static StructMembers *new_struct_member(Context *ctx, ScalarOrArray *soa,
                                        const char *semantic)
{
    NEW_AST_NODE(retval, StructMembers, AST_STRUCT_MEMBER);
    retval->datatype = NULL;
    retval->semantic = semantic;
    retval->details = soa;
    retval->interpolation_mod = INTERPMOD_NONE;
    retval->next = NULL;
    return retval;
} // new_struct_member

static void delete_struct_member(Context *ctx, StructMembers *member)
{
    DELETE_AST_NODE(member);
    delete_struct_member(ctx, member->next);
    delete_scalar_or_array(ctx, member->details);
    Free(ctx, member);
} // delete_struct_member

static StructDeclaration *new_struct_declaration(Context *ctx,
                                    const char *name, StructMembers *members)
{
    NEW_AST_NODE(retval, StructDeclaration, AST_STRUCT_DECLARATION);
    retval->name = name;
    retval->members = members;
    return retval;
} // new_struct_declaration

static void delete_struct_declaration(Context *ctx, StructDeclaration *decl)
{
    DELETE_AST_NODE(decl);
    delete_struct_member(ctx, decl->members);
    Free(ctx, decl);
} // delete_struct_declaration

static CompilationUnit *new_global_struct(Context *ctx, StructDeclaration *sd)
{
    NEW_AST_NODE(retval, CompilationUnitStruct, AST_COMPUNIT_STRUCT);
    retval->next = NULL;
    retval->struct_info = sd;
    return (CompilationUnit *) retval;
} // new_global_struct

static void delete_global_struct(Context *ctx, CompilationUnitStruct *unit)
{
    DELETE_AST_NODE(unit);
    delete_compilation_unit(ctx, unit->next);
    delete_struct_declaration(ctx, unit->struct_info);
    Free(ctx, unit);
} // delete_global_struct

static void delete_compilation_unit(Context *ctx, CompilationUnit *unit)
{
    if (!unit) return;

    // it's important to not recurse too deeply here, since you may have
    //  thousands of items in this linked list (each line of a massive
    //  function, for example). To avoid this, we iterate the list here,
    //  deleting all children and making them think they have no reason
    //  to recurse in their own delete methods.
    // Please note that everyone should _try_ to delete their "next" member,
    //  just in case, but hopefully this cleaned it out.

    CompilationUnit *i = unit->next;
    unit->next = NULL;
    while (i)
    {
        CompilationUnit *next = i->next;
        i->next = NULL;
        delete_compilation_unit(ctx, i);
        i = next;
    } // while

    switch (unit->ast.type)
    {
        #define DELETE_UNIT(typ, cls, fn) \
            case AST_COMPUNIT_##typ: delete_##fn(ctx, (cls *) unit); break;
        DELETE_UNIT(FUNCTION, CompilationUnitFunction, function);
        DELETE_UNIT(TYPEDEF, CompilationUnitTypedef, global_typedef);
        DELETE_UNIT(VARIABLE, CompilationUnitVariable, global_variable);
        DELETE_UNIT(STRUCT, CompilationUnitStruct, global_struct);
        #undef DELETE_UNIT
        default: assert(0 && "missing cleanup code"); break;
    } // switch

    // don't free (unit) here, the class-specific functions do it.
} // delete_compilation_unit

static Statement *new_typedef_statement(Context *ctx, Typedef *td)
{
    NEW_AST_NODE(retval, TypedefStatement, AST_STATEMENT_TYPEDEF);
    retval->next = NULL;
    retval->type_info = td;
    return (Statement *) retval;
} // new_typedef_statement

static void delete_typedef_statement(Context *ctx, TypedefStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_typedef(ctx, stmt->type_info);
    Free(ctx, stmt);
} // delete_typedef_statement

static Statement *new_return_statement(Context *ctx, Expression *expr)
{
    NEW_AST_NODE(retval, ReturnStatement, AST_STATEMENT_RETURN);
    retval->next = NULL;
    retval->expr = expr;
    return (Statement *) retval;
} // new_return_statement

static void delete_return_statement(Context *ctx, ReturnStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_return_statement

static Statement *new_block_statement(Context *ctx, Statement *statements)
{
    NEW_AST_NODE(retval, BlockStatement, AST_STATEMENT_BLOCK);
    retval->next = NULL;
    retval->statements = statements;
    return (Statement *) retval;
} // new_block_statement

static void delete_block_statement(Context *ctx, BlockStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->statements);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_statement_block

static Statement *new_for_statement(Context *ctx, VariableDeclaration *decl,
                                    Expression *initializer,
                                    Expression *looptest, Expression *counter,
                                    Statement *statement)
{
    NEW_AST_NODE(retval, ForStatement, AST_STATEMENT_FOR);
    retval->next = NULL;
    retval->unroll = -1;
    retval->var_decl = decl;
    retval->initializer = initializer;
    retval->looptest = looptest;
    retval->counter = counter;
    retval->statement = statement;
    return (Statement *) retval;
} // new_for_statement

static void delete_for_statement(Context *ctx, ForStatement *stmt)
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

static Statement *new_do_statement(Context *ctx, int64 unroll,
                                   Statement *stmt, Expression *expr)
{
    NEW_AST_NODE(retval, DoStatement, AST_STATEMENT_DO);
    retval->next = NULL;
    retval->unroll = unroll;
    retval->expr = expr;
    retval->statement = stmt;
    return (Statement *) retval;
} // new_do_statement

static void delete_do_statement(Context *ctx, DoStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_statement(ctx, stmt->statement);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_do_statement

static Statement *new_while_statement(Context *ctx, int64 unroll,
                                      Expression *expr, Statement *stmt)
{
    NEW_AST_NODE(retval, WhileStatement, AST_STATEMENT_WHILE);
    retval->next = NULL;
    retval->unroll = unroll;
    retval->expr = expr;
    retval->statement = stmt;
    return (Statement *) retval;
} // new_while_statement

static void delete_while_statement(Context *ctx, WhileStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_statement(ctx, stmt->statement);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_while_statement

static Statement *new_if_statement(Context *ctx, int attr, Expression *expr,
                                   Statement *stmt, Statement *elsestmt)
{
    NEW_AST_NODE(retval, IfStatement, AST_STATEMENT_IF);
    retval->next = NULL;
    retval->attributes = attr;
    retval->expr = expr;
    retval->statement = stmt;
    retval->else_statement = elsestmt;
    return (Statement *) retval;
} // new_if_statement

static void delete_if_statement(Context *ctx, IfStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_expr(ctx, stmt->expr);
    delete_statement(ctx, stmt->statement);
    delete_statement(ctx, stmt->else_statement);
    Free(ctx, stmt);
} // delete_if_statement

static SwitchCases *new_switch_case(Context *ctx, Expression *expr,
                                    Statement *stmt)
{
    NEW_AST_NODE(retval, SwitchCases, AST_SWITCH_CASE);
    retval->expr = expr;
    retval->statement = stmt;
    retval->next = NULL;
    return retval;
} // new_switch_case

static void delete_switch_case(Context *ctx, SwitchCases *sc)
{
    DELETE_AST_NODE(sc);
    delete_switch_case(ctx, sc->next);
    delete_expr(ctx, sc->expr);
    delete_statement(ctx, sc->statement);
    Free(ctx, sc);
} // delete_switch_case

static Statement *new_empty_statement(Context *ctx)
{
    NEW_AST_NODE(retval, EmptyStatement, AST_STATEMENT_EMPTY);
    retval->next = NULL;
    return (Statement *) retval;
} // new_empty_statement

static void delete_empty_statement(Context *ctx, EmptyStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_empty_statement

static Statement *new_break_statement(Context *ctx)
{
    NEW_AST_NODE(retval, BreakStatement, AST_STATEMENT_BREAK);
    retval->next = NULL;
    return (Statement *) retval;
} // new_break_statement

static void delete_break_statement(Context *ctx, BreakStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_break_statement

static Statement *new_continue_statement(Context *ctx)
{
    NEW_AST_NODE(retval, ContinueStatement, AST_STATEMENT_CONTINUE);
    retval->next = NULL;
    return (Statement *) retval;
} // new_continue_statement

static void delete_continue_statement(Context *ctx, ContinueStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_continue_statement

static Statement *new_discard_statement(Context *ctx)
{
    NEW_AST_NODE(retval, DiscardStatement, AST_STATEMENT_DISCARD);
    retval->next = NULL;
    return (Statement *) retval;
} // new_discard_statement

static void delete_discard_statement(Context *ctx, DiscardStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    Free(ctx, stmt);
} // delete_discard_statement

static Statement *new_expr_statement(Context *ctx, Expression *expr)
{
    NEW_AST_NODE(retval, ExpressionStatement, AST_STATEMENT_EXPRESSION);
    retval->next = NULL;
    retval->expr = expr;
    return (Statement *) retval;
} // new_expr_statement

static void delete_expr_statement(Context *ctx, ExpressionStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_expr(ctx, stmt->expr);
    Free(ctx, stmt);
} // delete_expr_statement

static Statement *new_switch_statement(Context *ctx, int attr,
                                       Expression *expr, SwitchCases *cases)
{
    NEW_AST_NODE(retval, SwitchStatement, AST_STATEMENT_SWITCH);
    retval->next = NULL;
    retval->attributes = attr;
    retval->expr = expr;
    retval->cases = cases;
    return (Statement *) retval;
} // new_switch_statement

static void delete_switch_statement(Context *ctx, SwitchStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_expr(ctx, stmt->expr);
    delete_switch_case(ctx, stmt->cases);
    Free(ctx, stmt);
} // delete_switch_statement

static Statement *new_struct_statement(Context *ctx, StructDeclaration *sd)
{
    NEW_AST_NODE(retval, StructStatement, AST_STATEMENT_STRUCT);
    retval->next = NULL;
    retval->struct_info = sd;
    return (Statement *) retval;
} // new_struct_statement

static void delete_struct_statement(Context *ctx, StructStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_struct_declaration(ctx, stmt->struct_info);
    Free(ctx, stmt);
} // delete_struct_statement

static Statement *new_vardecl_statement(Context *ctx, VariableDeclaration *vd)
{
    NEW_AST_NODE(retval, VarDeclStatement, AST_STATEMENT_VARDECL);
    retval->next = NULL;
    retval->declaration = vd;
    return (Statement *) retval;
} // new_vardecl_statement

static void delete_vardecl_statement(Context *ctx, VarDeclStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_variable_declaration(ctx, stmt->declaration);
    Free(ctx, stmt);
} // delete_vardecl_statement

static void delete_statement(Context *ctx, Statement *stmt)
{
    if (!stmt) return;

    // it's important to not recurse too deeply here, since you may have
    //  thousands of items in this linked list (each line of a massive
    //  function, for example). To avoid this, we iterate the list here,
    //  deleting all children and making them think they have no reason
    //  to recurse in their own delete methods.
    // Please note that everyone should _try_ to delete their "next" member,
    //  just in case, but hopefully this cleaned it out.

    Statement *i = stmt->next;
    stmt->next = NULL;
    while (i)
    {
        Statement *next = i->next;
        i->next = NULL;
        delete_statement(ctx, i);
        i = next;
    } // while

    switch (stmt->ast.type)
    {
        #define DELETE_STATEMENT(typ, cls, fn) case AST_STATEMENT_##typ: \
            delete_##fn##_statement(ctx, (cls *) stmt); break;
        DELETE_STATEMENT(BLOCK, BlockStatement, block);
        DELETE_STATEMENT(EMPTY, EmptyStatement, empty);
        DELETE_STATEMENT(IF, IfStatement, if);
        DELETE_STATEMENT(SWITCH, SwitchStatement, switch);
        DELETE_STATEMENT(EXPRESSION, ExpressionStatement, expr);
        DELETE_STATEMENT(FOR, ForStatement, for);
        DELETE_STATEMENT(DO, DoStatement, do);
        DELETE_STATEMENT(WHILE, WhileStatement, while);
        DELETE_STATEMENT(RETURN, ReturnStatement, return);
        DELETE_STATEMENT(BREAK, BreakStatement, break);
        DELETE_STATEMENT(CONTINUE, ContinueStatement, continue);
        DELETE_STATEMENT(DISCARD, DiscardStatement, discard);
        DELETE_STATEMENT(TYPEDEF, TypedefStatement, typedef);
        DELETE_STATEMENT(STRUCT, StructStatement, struct);
        DELETE_STATEMENT(VARDECL, VarDeclStatement, vardecl);
        #undef DELETE_STATEMENT
        default: assert(0 && "missing cleanup code"); break;
    } // switch
    // don't free (stmt) here, the class-specific functions do it.
} // delete_statement

// This is only for initial parsing: we only care that it exists at this point!
static void add_usertype(Context *ctx, const char *sym)
{
    push_usertype(ctx, sym, NULL);
} // add_usertype

static int is_usertype(const Context *ctx, const char *token)
{
    const void *value;
    return hash_find(ctx->usertypes.types, token, &value);
} // is_usertype


// This is where the actual parsing happens. It's Lemon-generated!
#define __MOJOSHADER_HLSL_COMPILER__ 1
#include "mojoshader_parser_hlsl.h"


static void print_ast(void *ast)
{
    static int indent = 0;
    int i;

    if (!ast) return;

    switch ( ((ASTGeneric *) ast)->ast.type )
    {
        case AST_OP_POSTINCREMENT:
            print_ast(((ExpressionUnary *) ast)->operand);
            printf("++");
            break;

        case AST_OP_POSTDECREMENT:
            print_ast(((ExpressionUnary *) ast)->operand);
            printf("--");
            break;

        case AST_OP_PREINCREMENT:
            printf("++");
            print_ast(((ExpressionUnary *) ast)->operand);
            break;

        case AST_OP_PREDECREMENT:
            printf("--");
            print_ast(((ExpressionUnary *) ast)->operand);
            break;

        case AST_OP_NEGATE:
            printf("-");
            print_ast(((ExpressionUnary *) ast)->operand);
            break;

        case AST_OP_COMPLEMENT:
            printf("~");
            print_ast(((ExpressionUnary *) ast)->operand);
            break;

        case AST_OP_NOT:
            printf("!");
            print_ast(((ExpressionUnary *) ast)->operand);
            break;

        case AST_OP_DEREF_ARRAY:
            print_ast(((ExpressionBinary *) ast)->left);
            printf("[");
            print_ast(((ExpressionBinary *) ast)->right);
            printf("]");
            break;

        case AST_OP_CALLFUNC:
            print_ast(((ExpressionBinary *) ast)->left);
            printf("(");
            print_ast(((ExpressionBinary *) ast)->right);
            printf(")");
            break;

        case AST_OP_DEREF_STRUCT:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(".");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_COMMA:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(", ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_MULTIPLY:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" * ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_DIVIDE:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" / ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_MODULO:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" %% ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_ADD:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" + ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_SUBTRACT:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" - ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_LSHIFT:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" << ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_RSHIFT:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" >> ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_LESSTHAN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" < ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_GREATERTHAN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" > ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_LESSTHANOREQUAL:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" <= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_GREATERTHANOREQUAL:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" >= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_EQUAL:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" == ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_NOTEQUAL:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" != ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_BINARYAND:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" & ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_BINARYXOR:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" ^ ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_BINARYOR:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" | ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_LOGICALAND:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" && ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_LOGICALOR:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" || ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_ASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" = ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_MULASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" *= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_DIVASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" /= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_MODASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" %%= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_ADDASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" += ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_SUBASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" -= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_LSHIFTASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" <<= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_RSHIFTASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" >>= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_ANDASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" &= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_XORASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" ^= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_ORASSIGN:
            print_ast(((ExpressionBinary *) ast)->left);
            printf(" |= ");
            print_ast(((ExpressionBinary *) ast)->right);
            break;

        case AST_OP_CONDITIONAL:
            print_ast(((ExpressionTernary *) ast)->left);
            printf(" ? ");
            print_ast(((ExpressionTernary *) ast)->center);
            printf(" : ");
            print_ast(((ExpressionTernary *) ast)->right);
            break;

        case AST_OP_IDENTIFIER:
            printf("%s", ((ExpressionIdentifier *) ast)->identifier);
            break;

        case AST_OP_INT_LITERAL:
            printf("%lld", (long long) ((ExpressionIntLiteral *) ast)->value);
            break;

        case AST_OP_FLOAT_LITERAL:
            printf("%f", ((ExpressionFloatLiteral *) ast)->value);
            break;

        case AST_OP_STRING_LITERAL:
            printf("\"%s\"", ((ExpressionStringLiteral *) ast)->string);
            break;

        case AST_OP_CONSTRUCTOR:
            printf("%s(", ((ExpressionConstructor *) ast)->datatype);
            print_ast(((ExpressionConstructor *) ast)->args);
            printf(")");
            break;

        case AST_OP_CAST:
            printf("(%s) (", ((ExpressionCast *) ast)->datatype);
            print_ast(((ExpressionCast *) ast)->operand);
            printf(")");
            break;

        case AST_STATEMENT_EMPTY:
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_EXPRESSION:
            print_ast(((ExpressionStatement *) ast)->expr);  // !!! FIXME: This is named badly...
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_IF:
            printf("if (");
            print_ast(((IfStatement *) ast)->expr);
            printf(")\n");

            if (((IfStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
                indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((ForStatement *) ast)->statement);
            if (((IfStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
            {
                printf("\n");
                indent--;
            } // if

            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_TYPEDEF:
            print_ast(((TypedefStatement *) ast)->type_info);
            printf("\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_SWITCH:
            switch ( ((SwitchStatement *) ast)->attributes )
            {
                case SWITCHATTR_NONE: break;
                case SWITCHATTR_FLATTEN: printf("[flatten] "); break;
                case SWITCHATTR_BRANCH: printf("[branch] "); break;
                case SWITCHATTR_FORCECASE: printf("[forcecase] "); break;
                case SWITCHATTR_CALL: printf("[call] "); break;
            } // switch

            printf("switch (");
            print_ast(((SwitchStatement *) ast)->expr);
            printf(")\n");
            for (i = 0; i < indent; i++) printf("    ");
            printf("{\n");
            indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((SwitchStatement *) ast)->cases);
            indent--;
            printf("\n");
            for (i = 0; i < indent; i++) printf("    ");
            printf("}\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_SWITCH_CASE:
            printf("case ");
            print_ast(((SwitchCases *) ast)->expr);
            printf(":\n");
            indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((SwitchCases *) ast)->statement);
            printf("\n");
            indent--;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((SwitchCases *) ast)->next);
            break;

        case AST_STATEMENT_STRUCT:
            print_ast(((CompilationUnitStruct *) ast)->struct_info);
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_VARDECL:
            print_ast(((VarDeclStatement *) ast)->declaration);
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_BLOCK:
            printf("{\n");
            indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((BlockStatement *) ast)->statements);
            printf("\n");
            indent--;
            for (i = 0; i < indent; i++) printf("    ");
            printf("}\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_FOR:
            if (((ForStatement *) ast)->unroll == 0)
                printf("[loop] ");
            else if (((ForStatement *) ast)->unroll == -1)
                printf("[unroll] ");
            else if (((ForStatement *) ast)->unroll > 0)
            {
                printf("[unroll(%lld)] ",
                        (long long) (((ForStatement *) ast)->unroll) );
            } // else if

            printf("for (");
            print_ast(((ForStatement *) ast)->var_decl);
            print_ast(((ForStatement *) ast)->initializer);
            printf("; ");
            print_ast(((ForStatement *) ast)->looptest);
            printf("; ");
            print_ast(((ForStatement *) ast)->counter);

            printf(")\n");
            if (((ForStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
                indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((ForStatement *) ast)->statement);
            if (((ForStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
            {
                printf("\n");
                indent--;
            } // if

            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_DO:
            if (((DoStatement *) ast)->unroll == 0)
                printf("[loop] ");
            else if (((DoStatement *) ast)->unroll == -1)
                printf("[unroll] ");
            else if (((DoStatement *) ast)->unroll > 0)
            {
                printf("[unroll(%lld)] ",
                        (long long) (((DoStatement *) ast)->unroll) );
            } // else if

            printf("do\n");

            if (((DoStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
                indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((ForStatement *) ast)->statement);
            if (((DoStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
            {
                printf("\n");
                indent--;
            } // if

            for (i = 0; i < indent; i++) printf("    ");
            printf("while (");
            print_ast(((DoStatement *) ast)->expr);
            printf(");\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_WHILE:
            if (((WhileStatement *) ast)->unroll == 0)
                printf("[loop] ");
            else if (((WhileStatement *) ast)->unroll == -1)
                printf("[unroll] ");
            else if (((WhileStatement *) ast)->unroll > 0)
            {
                printf("[unroll(%lld)] ",
                        (long long) (((WhileStatement *) ast)->unroll) );
            } // else if

            printf("while (");
            print_ast(((WhileStatement *) ast)->expr);
            printf(")\n");

            if (((WhileStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
                indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((ForStatement *) ast)->statement);
            if (((WhileStatement *) ast)->statement->ast.type != AST_STATEMENT_BLOCK)
            {
                printf("\n");
                indent--;
            } // if

            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_RETURN:
            printf("return");
            if (((ReturnStatement *) ast)->expr)
            {
                printf(" ");
                print_ast(((ReturnStatement *) ast)->expr);
            } // if
            printf(";");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_BREAK:
            printf("break;");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_CONTINUE:
            printf("continue;");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_STATEMENT_DISCARD:
            printf("discard;");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_COMPUNIT_FUNCTION:
            print_ast(((CompilationUnitFunction *) ast)->declaration);
            if (((CompilationUnitFunction *) ast)->definition == NULL)
                printf(";\n");
            else
            {
                printf("\n");
                for (i = 0; i < indent; i++) printf("    ");
                print_ast(((CompilationUnitFunction *) ast)->definition);
                printf("\n\n");
            } // else
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((CompilationUnit *) ast)->next);
            break;

        case AST_COMPUNIT_TYPEDEF:
            print_ast(((CompilationUnitTypedef *) ast)->type_info);
            printf("\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((Statement *) ast)->next);
            break;

        case AST_COMPUNIT_STRUCT:
            print_ast(((CompilationUnitStruct *) ast)->struct_info);
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((CompilationUnit *) ast)->next);
            break;

        case AST_COMPUNIT_VARIABLE:
            print_ast(((CompilationUnitVariable *) ast)->declaration);
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((CompilationUnit *) ast)->next);
            break;

        case AST_SCALAR_OR_ARRAY:
            printf("%s", ((ScalarOrArray*) ast)->identifier);
            if (((ScalarOrArray*) ast)->isarray)
            {
                printf("[");
                print_ast(((ScalarOrArray*) ast)->dimension);
                printf("]");
            } // if
            break;

        case AST_TYPEDEF:
            printf("typedef %s%s ",
                    (((Typedef *) ast)->isconst) ? "const " : "",
                    (((Typedef *) ast)->datatype));
            print_ast(((Typedef *) ast)->details);
            printf(";");
            break;

        case AST_FUNCTION_ARGS:
            switch (((FunctionArguments *) ast)->input_modifier)
            {
                case INPUTMOD_NONE: break;
                case INPUTMOD_IN: printf("in "); break;
                case INPUTMOD_OUT: printf("out "); break;
                case INPUTMOD_INOUT: printf("in out "); break;
                case INPUTMOD_UNIFORM: printf("uniform "); break;
            } // switch

            printf("%s %s", (((FunctionArguments *) ast)->datatype),
                   (((FunctionArguments *) ast)->identifier));
            if (((FunctionArguments *) ast)->semantic)
                printf(" : %s", ((FunctionArguments *) ast)->semantic);

            switch (((FunctionArguments *) ast)->interpolation_modifier)
            {
                case INTERPMOD_NONE: break;
                case INTERPMOD_LINEAR: printf(" linear"); break;
                case INTERPMOD_CENTROID: printf(" centroid"); break;
                case INTERPMOD_NOINTERPOLATION: printf(" nointerpolation"); break;
                case INTERPMOD_NOPERSPECTIVE: printf(" noperspective"); break;
                case INTERPMOD_SAMPLE: printf(" sample"); break;
            } // switch

            if (((FunctionArguments *) ast)->initializer)
                print_ast(((FunctionArguments *) ast)->initializer);

            if (((FunctionArguments *) ast)->next)
            {
                printf(", ");
                print_ast(((FunctionArguments *) ast)->next);
            } // if
            break;

        case AST_FUNCTION_SIGNATURE:
            switch (((FunctionSignature *) ast)->storage_class)
            {
                case FNSTORECLS_NONE: break;
                case FNSTORECLS_INLINE: printf("inline "); break;
            } // switch
            printf("%s %s(",
                    ((FunctionSignature *) ast)->datatype ?
                        ((FunctionSignature *) ast)->datatype : "void",
                    ((FunctionSignature *) ast)->identifier);
            print_ast(((FunctionSignature *) ast)->args);
            printf(")");
            if (((FunctionSignature *) ast)->semantic)
                printf(" : %s", ((FunctionSignature *) ast)->semantic);
            break;

        case AST_STRUCT_DECLARATION:
            printf("struct %s\n", ((StructDeclaration *) ast)->name);
            for (i = 0; i < indent; i++) printf("    ");
            printf("{\n");
            indent++;
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((StructDeclaration *) ast)->members);
            printf("\n");
            indent--;
            for (i = 0; i < indent; i++) printf("    ");
            printf("}");
            break;

        case AST_STRUCT_MEMBER:
            switch (((StructMembers *) ast)->interpolation_mod)
            {
                case INTERPMOD_NONE: break;
                case INTERPMOD_LINEAR: printf("linear "); break;
                case INTERPMOD_CENTROID: printf("centroid "); break;
                case INTERPMOD_NOINTERPOLATION: printf("nointerpolation "); break;
                case INTERPMOD_NOPERSPECTIVE: printf("noperspective "); break;
                case INTERPMOD_SAMPLE: printf("sample "); break;
            } // switch
            printf("%s ", ((StructMembers *) ast)->datatype);
            print_ast(((StructMembers *) ast)->details);
            if (((StructMembers *) ast)->semantic)
                printf(" : %s", ((StructMembers *) ast)->semantic);
            printf(";\n");
            for (i = 0; i < indent; i++) printf("    ");
            print_ast(((StructMembers *) ast)->next);
            break;

        case AST_VARIABLE_DECLARATION:
            if (((VariableDeclaration *) ast)->attributes & VARATTR_EXTERN)
                printf("extern ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_NOINTERPOLATION)
                printf("nointerpolation ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_SHARED)
                printf("shared");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_STATIC)
                printf("static ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_UNIFORM)
                printf("uniform ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_VOLATILE)
                printf("nointerpolation ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_CONST)
                printf("const ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_ROWMAJOR)
                printf("rowmajor ");
            if (((VariableDeclaration *) ast)->attributes & VARATTR_COLUMNMAJOR)
                printf("columnmajor ");

            if (((VariableDeclaration *) ast)->datatype)
                printf("%s", ((VariableDeclaration *) ast)->datatype);
            else
                print_ast(((VariableDeclaration *) ast)->anonymous_datatype);
            printf(" ");
            print_ast(((VariableDeclaration *) ast)->details);
            if (((VariableDeclaration *) ast)->semantic)
                printf(" : %s", ((VariableDeclaration *) ast)->semantic);
            if (((VariableDeclaration *) ast)->annotations)
            {
                printf(" ");
                print_ast(((VariableDeclaration *) ast)->annotations);
            } // if
            print_ast(((VariableDeclaration *) ast)->initializer);
            print_ast(((VariableDeclaration *) ast)->lowlevel);

            if (((VariableDeclaration *) ast)->next)
            {
                int attr = (((VariableDeclaration *) ast)->next)->attributes;
                printf(", ");
                (((VariableDeclaration *) ast)->next)->attributes = 0;
                print_ast(((VariableDeclaration *) ast)->next);
                (((VariableDeclaration *) ast)->next)->attributes = attr;
            } // if
            break;

        case AST_PACK_OFFSET:
            printf(" : packoffset(%s%s%s)",
                    ((PackOffset *) ast)->ident1,
                    ((PackOffset *) ast)->ident2 ? "." : "",
                    ((PackOffset *) ast)->ident2 ? ((PackOffset *) ast)->ident2 : "");
            break;

        case AST_VARIABLE_LOWLEVEL:
            print_ast(((VariableLowLevel *) ast)->packoffset);
            if (((VariableLowLevel *) ast)->register_name)
                printf(" : register(%s)", ((VariableLowLevel *) ast)->register_name);
            break;

        case AST_ANNOTATION:
        {
            const Annotations *a = (Annotations *) ast;
            printf("<");
            while (a)
            {
                printf(" %s ", a->datatype);
                print_ast(a->initializer);
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
} // print_ast


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
            if (tokencmp("SamplerComparisonState")) return TOKEN_HLSL_SAMPLERCOMPARISONSTATE;
            if (tokencmp("isolate")) return TOKEN_HLSL_ISOLATE;
            if (tokencmp("maxInstructionCount")) return TOKEN_HLSL_MAXINSTRUCTIONCOUNT;
            if (tokencmp("noExpressionOptimizations")) return TOKEN_HLSL_NOEXPRESSIONOPTIMIZATIONS;
            if (tokencmp("unused")) return TOKEN_HLSL_UNUSED;
            if (tokencmp("xps")) return TOKEN_HLSL_XPS;

            #undef tokencmp

            // get a canonical copy of the string now, as we'll need it.
            token = stringcache_len(ctx->strcache, token, tokenlen);
            if (is_usertype(ctx, token))
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
        delete_compilation_unit(ctx, ctx->ast);
        destroy_usertypemap(ctx);

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
    create_usertypemap(ctx);  // !!! FIXME: check for failure.
    ctx->strcache = stringcache_create(m, f, d);  // !!! FIXME: check for failure.

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
            add_usertype(ctx, stringcache_len(ctx->strcache, buf, len));
            int k;
            for (k = 1; k <= 4; k++)
            {
                // "float2x2"
                len = snprintf(buf, sizeof (buf), "%s%dx%d", types[i], j, k);
                add_usertype(ctx, stringcache_len(ctx->strcache, buf, len));
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


void MOJOSHADER_compile(const char *filename,
                        const char *source, unsigned int sourcelen,
                        const MOJOSHADER_preprocessorDefine *defines,
                        unsigned int define_count,
                        MOJOSHADER_includeOpen include_open,
                        MOJOSHADER_includeClose include_close,
                        MOJOSHADER_malloc m, MOJOSHADER_free f, void *d)
{
    Context *ctx = build_context(m, f, d);
    if (!ctx)
        return;  // !!! FIXME: report error.

    parse_source(ctx, filename, source, sourcelen, defines, define_count,
                 include_open, include_close);

    // !!! FIXME: check (ctx->ast != NULL), and maybe isfail().

    print_ast(ctx->ast);

    destroy_context(ctx);

    // !!! FIXME: report success/error.
} // MOJOSHADER_compile

// end of mojoshader_compiler.c ...

