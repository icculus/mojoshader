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


// Structures that make up the parse tree...

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

    OP_CONSTRUCTOR,
    OP_CAST
} Operator;

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

typedef struct ExpressionConstructor
{
    Operator op;  // Always OP_CONSTRUCTOR
    const char *datatype;
    Expression *args;
} ExpressionConstructor;

typedef struct ExpressionCast
{
    Operator op;  // Always OP_CAST
    const char *datatype;
    Expression *operand;
} ExpressionCast;

typedef enum CompilationUnitType
{
    COMPUNITTYPE_FUNCTION,  // function declaration or definition
    COMPUNITTYPE_TYPEDEF,   // typedef or struct
    COMPUNITTYPE_STRUCT,    // global struct
    COMPUNITTYPE_VARIABLE   // global variable
} CompilationUnitType;

typedef struct CompilationUnit
{
    CompilationUnitType type;
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
    const char *datatype;
    const char *identifier;
    FunctionArguments *args;
    FunctionStorageClass storage_class;
    const char *semantic;
} FunctionSignature;

typedef struct ScalarOrArray
{
    const char *identifier;
    int isarray;
    Expression *dimension;
} ScalarOrArray;

typedef struct Annotations
{
    const char *datatype;
    Expression *initializer;
    struct Annotations *next;
} Annotations;

typedef struct PackOffset
{
    const char *ident1;   // !!! FIXME: rename this.
    const char *ident2;
} PackOffset;

typedef struct VariableLowLevel
{
    PackOffset *packoffset;
    const char *register_name;
} VariableLowLevel;

typedef struct StructMembers
{
    const char *datatype;
    const char *semantic;
    ScalarOrArray *details;
    InterpolationModifier interpolation_mod;
    struct StructMembers *next;
} StructMembers;

typedef struct StructDeclaration
{
    const char *name;
    StructMembers *members;
} StructDeclaration;

typedef struct VariableDeclaration
{
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

typedef enum StatementType
{
    STATEMENTTYPE_EMPTY,
    STATEMENTTYPE_EXPRESSION,
    STATEMENTTYPE_IF,
    STATEMENTTYPE_SWITCH,
    STATEMENTTYPE_FOR,
    STATEMENTTYPE_DO,
    STATEMENTTYPE_WHILE,
    STATEMENTTYPE_RETURN,
    STATEMENTTYPE_BREAK,
    STATEMENTTYPE_CONTINUE,
    STATEMENTTYPE_DISCARD,
    STATEMENTTYPE_TYPEDEF,
    STATEMENTTYPE_STRUCT,
    STATEMENTTYPE_VARDECL,
} StatementType;

typedef struct Statement
{
    StatementType type;
    struct Statement *next;
} Statement;

typedef Statement EmptyStatement;
typedef Statement BreakStatement;
typedef Statement ContinueStatement;
typedef Statement DiscardStatement;

typedef struct ReturnStatement
{
    StatementType type;
    struct Statement *next;
    Expression *expr;
} ReturnStatement;

typedef struct ExpressionStatement
{
    StatementType type;
    struct Statement *next;
    Expression *expr;
} ExpressionStatement;

typedef struct IfStatement
{
    StatementType type;
    struct Statement *next;
    int attributes;
    Expression *expr;
    Statement *statement;
    Statement *else_statement;
} IfStatement;

typedef struct SwitchCases
{
    Expression *expr;
    Statement *statement;
    struct SwitchCases *next;
} SwitchCases;

typedef struct SwitchStatement
{
    StatementType type;
    struct Statement *next;
    int attributes;
    Expression *expr;
    SwitchCases *cases;
} SwitchStatement;

typedef struct WhileStatement
{
    StatementType type;
    struct Statement *next;
    int64 unroll;  // # times to unroll, 0 to loop, -1 for compiler's choice.
    Expression *expr;
    Statement *statement;
} WhileStatement;

typedef WhileStatement DoStatement;

typedef struct ForStatement
{
    StatementType type;
    struct Statement *next;
    int64 unroll;  // # times to unroll, 0 to loop, -1 for compiler's choice.
    VariableDeclaration *var_decl;
    Expression *initializer;
    Expression *looptest;
    Expression *counter;
    Statement *statement;
} ForStatement;

typedef struct Typedef
{
    int isconst;
    const char *datatype;
    ScalarOrArray *details;
} Typedef;

typedef struct TypedefStatement
{
    StatementType type;
    struct Statement *next;
    Typedef *type_info;
} TypedefStatement;

typedef struct VarDeclStatement
{
    StatementType type;
    struct Statement *next;
    VariableDeclaration *decl;
} VarDeclStatement;

typedef struct StructStatement
{
    StatementType type;
    struct Statement *next;
    StructDeclaration *struct_info;
} StructStatement;

typedef struct CompilationUnitFunction
{
    CompilationUnitType type;
    struct CompilationUnit *next;
    FunctionSignature *declaration;
    Statement *definition;
} CompilationUnitFunction;

typedef struct CompilationUnitTypedef
{
    CompilationUnitType type;
    struct CompilationUnit *next;
    Typedef *type_info;
} CompilationUnitTypedef;

typedef struct CompilationUnitStruct
{
    CompilationUnitType type;
    struct CompilationUnit *next;
    StructDeclaration *struct_info;
} CompilationUnitStruct;

typedef struct CompilationUnitVariable
{
    CompilationUnitType type;
    struct CompilationUnit *next;
    VariableDeclaration *declaration;
} CompilationUnitVariable;


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
    Preprocessor *preprocessor;
    StringBucket *string_hashtable[256];
    const char *sourcefile;
    unsigned int sourceline;
    const char *usertypes[512];  // !!! FIXME: dynamic allocation
    int usertype_count;
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

// These functions are mostly for construction and cleanup of nodes in the
//  parse tree. Mostly this is simple allocation and initialization, so we
//  can do as little in the lemon code as possible, and then sort it all out
//  afterwards.

#define NEW_AST_NODE(cls) \
    cls *retval = Malloc(ctx, sizeof (cls)); \
    if (retval == NULL) { return NULL; } \

#define DELETE_AST_NODE(cls) do {\
    if (!cls) return; \
} while (0)

static void delete_compilation_unit(Context *ctx, CompilationUnit *unit);
static void delete_statement(Context *ctx, Statement *stmt);

static Expression *new_constructor_expr(Context *ctx, const char *datatype,
                                        Expression *args)
{
    NEW_AST_NODE(ExpressionConstructor);
    retval->op = OP_CONSTRUCTOR;
    retval->datatype = datatype;
    retval->args = args;
    return (Expression *) retval;
} // new_constructor_expr

static Expression *new_cast_expr(Context *ctx, const char *datatype,
                                 Expression *operand)
{
    NEW_AST_NODE(ExpressionCast);
    retval->op = OP_CAST;
    retval->datatype = datatype;
    retval->operand = operand;
    return (Expression *) retval;
} // new_cast_expr

static Expression *new_unary_expr(Context *ctx, const Operator op,
                                  Expression *operand)
{
    NEW_AST_NODE(ExpressionUnary);
    assert(operator_is_unary(op));
    retval->op = op;
    retval->operand = operand;
    return (Expression *) retval;
} // new_unary_expr

static Expression *new_binary_expr(Context *ctx, const Operator op,
                                   Expression *left, Expression *right)
{
    NEW_AST_NODE(ExpressionBinary);
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
    NEW_AST_NODE(ExpressionTernary);
    assert(operator_is_ternary(op));
    retval->op = op;
    retval->left = left;
    retval->center = center;
    retval->right = right;
    return (Expression *) retval;
} // new_ternary_expr

static Expression *new_identifier_expr(Context *ctx, const char *string)
{
    NEW_AST_NODE(ExpressionIdentifier);
    retval->op = OP_IDENTIFIER;
    retval->identifier = string;  // cached; don't copy string.
    return (Expression *) retval;
} // new_identifier_expr

static Expression *new_literal_int_expr(Context *ctx, const int64 value)
{
    NEW_AST_NODE(ExpressionIntLiteral);
    retval->op = OP_INT_LITERAL;
    retval->value = value;
    return (Expression *) retval;
} // new_literal_int_expr

static Expression *new_literal_float_expr(Context *ctx, const double dbl)
{
    NEW_AST_NODE(ExpressionFloatLiteral);
    retval->op = OP_FLOAT_LITERAL;
    retval->value = dbl;
    return (Expression *) retval;
} // new_literal_float_expr

static Expression *new_literal_string_expr(Context *ctx, const char *string)
{
    NEW_AST_NODE(ExpressionStringLiteral);
    retval->op = OP_STRING_LITERAL;
    retval->string = string;  // cached; don't copy string.
    return (Expression *) retval;
} // new_string_literal_expr

static void delete_expr(Context *ctx, Expression *expr)
{
    DELETE_AST_NODE(expr);
    if (operator_is_unary(expr->op))
    {
        const ExpressionUnary *unary = (const ExpressionUnary *) expr;
        delete_expr(ctx, unary->operand);
    } // if
    else if (operator_is_binary(expr->op))
    {
        const ExpressionBinary *binary = (const ExpressionBinary *) expr;
        delete_expr(ctx, binary->left);
        delete_expr(ctx, binary->right);
    } // else if
    else if (operator_is_ternary(expr->op))
    {
        const ExpressionTernary *ternary = (const ExpressionTernary *) expr;
        delete_expr(ctx, ternary->left);
        delete_expr(ctx, ternary->center);
        delete_expr(ctx, ternary->right);
    } // else if
    else if (expr->op == OP_CAST)
    {
        delete_expr(ctx, ((ExpressionCast *) expr)->operand);
    } // else if
    else if (expr->op == OP_CONSTRUCTOR)
    {
        delete_expr(ctx, ((ExpressionConstructor *) expr)->args);
    } // else if
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
    NEW_AST_NODE(FunctionArguments);
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
    NEW_AST_NODE(FunctionSignature);
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
    NEW_AST_NODE(CompilationUnitFunction);
    retval->type = COMPUNITTYPE_FUNCTION;
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
    NEW_AST_NODE(ScalarOrArray);
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
    NEW_AST_NODE(Typedef);
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
    NEW_AST_NODE(PackOffset);
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
    NEW_AST_NODE(VariableLowLevel);
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
    NEW_AST_NODE(Annotations);
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
    NEW_AST_NODE(VariableDeclaration)
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
    NEW_AST_NODE(CompilationUnitVariable);
    retval->type = COMPUNITTYPE_FUNCTION;
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
    NEW_AST_NODE(CompilationUnitTypedef)
    retval->type = COMPUNITTYPE_TYPEDEF;
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
    NEW_AST_NODE(StructMembers);
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
    NEW_AST_NODE(StructDeclaration);
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
    NEW_AST_NODE(CompilationUnitStruct)
    retval->type = COMPUNITTYPE_STRUCT;
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

    switch (unit->type)
    {
        #define DELETE_UNIT(typ, cls, fn) \
            case COMPUNITTYPE_##typ: delete_##fn(ctx, (cls *) unit); break;
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
    NEW_AST_NODE(TypedefStatement)
    retval->type = STATEMENTTYPE_TYPEDEF;
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
    NEW_AST_NODE(ReturnStatement);
    retval->type = STATEMENTTYPE_RETURN;
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

static Statement *new_for_statement(Context *ctx, VariableDeclaration *decl,
                                    Expression *initializer,
                                    Expression *looptest, Expression *counter,
                                    Statement *statement)
{
    NEW_AST_NODE(ForStatement);
    retval->type = STATEMENTTYPE_FOR;
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
    NEW_AST_NODE(DoStatement);
    retval->type = STATEMENTTYPE_DO;
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
    NEW_AST_NODE(WhileStatement);
    retval->type = STATEMENTTYPE_WHILE;
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
    NEW_AST_NODE(IfStatement);
    retval->type = STATEMENTTYPE_IF;
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
    NEW_AST_NODE(SwitchCases);
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
    NEW_AST_NODE(EmptyStatement);
    retval->type = STATEMENTTYPE_EMPTY;
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
    NEW_AST_NODE(BreakStatement);
    retval->type = STATEMENTTYPE_BREAK;
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
    NEW_AST_NODE(ContinueStatement);
    retval->type = STATEMENTTYPE_CONTINUE;
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
    NEW_AST_NODE(DiscardStatement);
    retval->type = STATEMENTTYPE_DISCARD;
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
    NEW_AST_NODE(ExpressionStatement);
    retval->type = STATEMENTTYPE_EXPRESSION;
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
    NEW_AST_NODE(SwitchStatement);
    retval->type = STATEMENTTYPE_SWITCH;
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
    NEW_AST_NODE(StructStatement);
    retval->type = STATEMENTTYPE_STRUCT;
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
    NEW_AST_NODE(VarDeclStatement);
    retval->type = STATEMENTTYPE_VARDECL;
    retval->next = NULL;
    retval->decl = vd;
    return (Statement *) retval;
} // new_vardecl_statement

static void delete_vardecl_statement(Context *ctx, VarDeclStatement *stmt)
{
    DELETE_AST_NODE(stmt);
    delete_statement(ctx, stmt->next);
    delete_variable_declaration(ctx, stmt->decl);
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

    switch (stmt->type)
    {
        #define DELETE_STATEMENT(typ, cls, fn) \
            case typ: delete_##fn##_statement(ctx, (cls *) stmt); break;
        DELETE_STATEMENT(STATEMENTTYPE_EMPTY, EmptyStatement, empty);
        DELETE_STATEMENT(STATEMENTTYPE_IF, IfStatement, if);
        DELETE_STATEMENT(STATEMENTTYPE_SWITCH, SwitchStatement, switch);
        DELETE_STATEMENT(STATEMENTTYPE_EXPRESSION, ExpressionStatement, expr);
        DELETE_STATEMENT(STATEMENTTYPE_FOR, ForStatement, for);
        DELETE_STATEMENT(STATEMENTTYPE_DO, DoStatement, do);
        DELETE_STATEMENT(STATEMENTTYPE_WHILE, WhileStatement, while);
        DELETE_STATEMENT(STATEMENTTYPE_RETURN, ReturnStatement, return);
        DELETE_STATEMENT(STATEMENTTYPE_BREAK, BreakStatement, break);
        DELETE_STATEMENT(STATEMENTTYPE_CONTINUE, ContinueStatement, continue);
        DELETE_STATEMENT(STATEMENTTYPE_DISCARD, DiscardStatement, discard);
        DELETE_STATEMENT(STATEMENTTYPE_TYPEDEF, TypedefStatement, typedef);
        DELETE_STATEMENT(STATEMENTTYPE_STRUCT, StructStatement, struct);
        DELETE_STATEMENT(STATEMENTTYPE_VARDECL, VarDeclStatement, vardecl);
        #undef DELETE_STATEMENT
        default: assert(0 && "missing cleanup code"); break;
    } // switch
    // don't free (stmt) here, the class-specific functions do it.
} // delete_statement

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

static const char *cache_string_fmt(Context *ctx, const char *fmt, ...)
{
    char buf[128];  // use the stack if reasonable.
    char *ptr = NULL;
    int len = 0;  // number of chars, NOT counting null-terminator!
    va_list ap;

    va_start(ap, fmt);
    len = vsnprintf(buf, sizeof (buf), fmt, ap);
    va_end(ap);

    if (len > sizeof (buf))
    {
        ptr = (char *) Malloc(ctx, len);
        if (ptr == NULL)
            return NULL;

        va_start(ap, fmt);
        vsnprintf(ptr, len, fmt, ap);
        va_end(ap);
    } // if

    const char *retval = cache_string(ctx, ptr ? ptr : buf, len);
    if (ptr != NULL)
        Free(ctx, ptr);

    return retval;
} // cache_string_fmt

// This is where the actual parsing happens. It's Lemon-generated!
#define __MOJOSHADER_HLSL_COMPILER__ 1
#include "mojoshader_parser_hlsl.h"


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

            if (is_usertype(ctx, token, tokenlen))
                return TOKEN_HLSL_USERTYPE;
            return TOKEN_HLSL_IDENTIFIER;

        case TOKEN_EOI: return 0;
        default: assert(0 && "unexpected token from lexer\n"); return 0;
    } // switch

    return 0;
} // convert_to_lemon_token

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

static void destroy_context(Context *ctx)
{
    if (ctx->preprocessor != NULL)
        preprocessor_end(ctx->preprocessor);
    // !!! FIXME: free ctx->errors
    // !!! FIXME: free ctx->usertypes
    delete_compilation_unit(ctx, ctx->ast);
    free_string_cache(ctx);
} // destroy_context

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
    const char *fname;

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    memset(&ctx, '\0', sizeof (Context));
    ctx.malloc = m;
    ctx.free = f;
    ctx.malloc_data = d;
    ctx.preprocessor = preprocessor_start(filename, source, sourcelen,
                                           include_open, include_close,
                                           defines, define_count, 0, m, f, d);

    // !!! FIXME: check if (ctx.preprocessor == NULL)...

    void *pParser = ParseHLSLAlloc(m, d);

    #if DEBUG_COMPILER_PARSER
    ParseHLSLTrace(stdout, "COMPILER: ");
    #endif

    do {
        token = preprocessor_nexttoken(ctx.preprocessor, &tokenlen, &tokenval);

        if (preprocessor_outofmemory(ctx.preprocessor))
        {
            out_of_memory(&ctx);
            break;
        } // if

        fname = preprocessor_sourcepos(ctx.preprocessor, &ctx.sourceline);
        ctx.sourcefile = fname ? cache_string(&ctx, fname, strlen(fname)) : 0;

        if (tokenval == TOKEN_BAD_CHARS)
        {
            fail(&ctx, "Bad characters in source file");
            continue;
        } // else if

        else if (tokenval == TOKEN_PREPROCESSING_ERROR)
        {
            fail(&ctx, token);  // this happens to be null-terminated.
            continue;
        } // else if

        lemon_token = convert_to_lemon_token(&ctx, token, tokenlen, tokenval);
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
                data.string = cache_string(&ctx, token, tokenlen);
                break;

            default:
                data.i64 = 0;
                break;
        } // switch

        ParseHLSL(pParser, lemon_token, data, &ctx);
    } while (tokenval != TOKEN_EOI);

    ParseHLSLFree(pParser, f, d);
    destroy_context(&ctx);
} // MOJOSHADER_compile

// end of mojoshader_compiler.c ...

