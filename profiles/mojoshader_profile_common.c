#include "mojoshader_profile.h"

void * MOJOSHADERCALL MallocBridge(int bytes, void *data)
{
    return Malloc((Context *) data, (size_t) bytes);
} // MallocBridge

void MOJOSHADERCALL FreeBridge(void *ptr, void *data)
{
    Free((Context *) data, ptr);
} // FreeBridge


// jump between output sections in the context...

int set_output(Context *ctx, Buffer **section)
{
    // only create output sections on first use.
    if (*section == NULL)
    {
        *section = buffer_create(256, MallocBridge, FreeBridge, ctx);
        if (*section == NULL)
            return 0;
    } // if

    ctx->output = *section;
    return 1;
} // set_output

void push_output(Context *ctx, Buffer **section)
{
    assert(ctx->output_stack_len < (int) (STATICARRAYLEN(ctx->output_stack)));
    ctx->output_stack[ctx->output_stack_len] = ctx->output;
    ctx->indent_stack[ctx->output_stack_len] = ctx->indent;
    ctx->output_stack_len++;
    if (!set_output(ctx, section))
        return;
    ctx->indent = 0;
} // push_output


void failf(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
void failf(Context *ctx, const char *fmt, ...)
{
    ctx->isfail = 1;
    if (ctx->out_of_memory)
        return;

    // no filename at this level (we pass a NULL to errorlist_add_va()...)
    va_list ap;
    va_start(ap, fmt);
    errorlist_add_va(ctx->errors, NULL, ctx->current_position, fmt, ap);
    va_end(ap);
} // failf


void output_line(Context *ctx, const char *fmt, ...) ISPRINTF(2,3);
void output_line(Context *ctx, const char *fmt, ...)
{
    assert(ctx->output != NULL);
    if (isfail(ctx))
        return;  // we failed previously, don't go on...

    const int indent = ctx->indent;
    if (indent > 0)
    {
        char *indentbuf = (char *) alloca(indent);
        memset(indentbuf, '\t', indent);
        buffer_append(ctx->output, indentbuf, indent);
    } // if

    va_list ap;
    va_start(ap, fmt);
    buffer_append_va(ctx->output, fmt, ap);
    va_end(ap);

    buffer_append(ctx->output, ctx->endline, ctx->endline_len);
} // output_line


// !!! FIXME: this is sort of nasty.
void floatstr(Context *ctx, char *buf, size_t bufsize, float f,
                     int leavedecimal)
{
    const size_t len = MOJOSHADER_printFloat(buf, bufsize, f);
    if ((len+2) >= bufsize)
        fail(ctx, "BUG: internal buffer is too small");
    else
    {
        char *end = buf + len;
        char *ptr = strchr(buf, '.');
        if (ptr == NULL)
        {
            if (leavedecimal)
                strcat(buf, ".0");
            return;  // done.
        } // if

        while (--end != ptr)
        {
            if (*end != '0')
            {
                end++;
                break;
            } // if
        } // while
        if ((leavedecimal) && (end == ptr))
            end += 2;
        *end = '\0';  // chop extra '0' or all decimal places off.
    } // else
} // floatstr

static inline uint32 reg_to_ui32(const RegisterType regtype, const int regnum)
{
    return ( ((uint32) regnum) | (((uint32) regtype) << 16) );
} // reg_to_uint32

// !!! FIXME: ditch this for a hash table.
RegisterList *reglist_insert(Context *ctx, RegisterList *prev,
                                    const RegisterType regtype,
                                    const int regnum)
{
    const uint32 newval = reg_to_ui32(regtype, regnum);
    RegisterList *item = prev->next;
    while (item != NULL)
    {
        const uint32 val = reg_to_ui32(item->regtype, item->regnum);
        if (newval == val)
            return item;  // already set, so we're done.
        else if (newval < val)  // insert it here.
            break;
        else // if (newval > val)
        {
            // keep going, we're not to the insertion point yet.
            prev = item;
            item = item->next;
        } // else
    } // while

    // we need to insert an entry after (prev).
    item = (RegisterList *) Malloc(ctx, sizeof (RegisterList));
    if (item != NULL)
    {
        item->regtype = regtype;
        item->regnum = regnum;
        item->usage = MOJOSHADER_USAGE_UNKNOWN;
        item->index = 0;
        item->writemask = 0;
        item->misc = 0;
        item->written = 0;
        item->array = NULL;
        item->next = prev->next;
        prev->next = item;
    } // if

    return item;
} // reglist_insert

RegisterList *reglist_find(const RegisterList *prev,
                                  const RegisterType rtype, const int regnum)
{
    const uint32 newval = reg_to_ui32(rtype, regnum);
    RegisterList *item = prev->next;
    while (item != NULL)
    {
        const uint32 val = reg_to_ui32(item->regtype, item->regnum);
        if (newval == val)
            return item;  // here it is.
        else if (newval < val)  // should have been here if it existed.
            return NULL;
        else // if (newval > val)
            item = item->next;
    } // while

    return NULL;  // wasn't in the list.
} // reglist_find


int allocate_scratch_register(Context *ctx)
{
    const int retval = ctx->scratch_registers++;
    if (retval >= ctx->max_scratch_registers)
        ctx->max_scratch_registers = retval + 1;
    return retval;
} // allocate_scratch_register

int allocate_branch_label(Context *ctx)
{
    return ctx->assigned_branch_labels++;
} // allocate_branch_label


// D3D stuff that's used in more than just the d3d profile...

int isscalar(Context *ctx, const MOJOSHADER_shaderType shader_type,
                    const RegisterType rtype, const int rnum)
{
    const int uses_psize = ctx->uses_pointsize;
    const int uses_fog = ctx->uses_fog;
    if ( (rtype == REG_TYPE_OUTPUT) && ((uses_psize) || (uses_fog)) )
    {
        const RegisterList *reg = reglist_find(&ctx->attributes, rtype, rnum);
        if (reg != NULL)
        {
            const MOJOSHADER_usage usage = reg->usage;
            return ( (uses_psize && (usage == MOJOSHADER_USAGE_POINTSIZE)) ||
                     (uses_fog && (usage == MOJOSHADER_USAGE_FOG)) );
        } // if
    } // if

    return scalar_register(shader_type, rtype, rnum);
} // isscalar

const char *get_D3D_register_string(Context *ctx,
                                           RegisterType regtype,
                                           int regnum, char *regnum_str,
                                           size_t regnum_size)
{
    const char *retval = NULL;
    int has_number = 1;

    switch (regtype)
    {
        case REG_TYPE_TEMP:
            retval = "r";
            break;

        case REG_TYPE_INPUT:
            retval = "v";
            break;

        case REG_TYPE_CONST:
            retval = "c";
            break;

        case REG_TYPE_ADDRESS:  // (or REG_TYPE_TEXTURE, same value.)
            retval = shader_is_vertex(ctx) ? "a" : "t";
            break;

        case REG_TYPE_RASTOUT:
            switch ((RastOutType) regnum)
            {
                case RASTOUT_TYPE_POSITION: retval = "oPos"; break;
                case RASTOUT_TYPE_FOG: retval = "oFog"; break;
                case RASTOUT_TYPE_POINT_SIZE: retval = "oPts"; break;
            } // switch
            has_number = 0;
            break;

        case REG_TYPE_ATTROUT:
            retval = "oD";
            break;

        case REG_TYPE_OUTPUT: // (or REG_TYPE_TEXCRDOUT, same value.)
            if (shader_is_vertex(ctx) && shader_version_atleast(ctx, 3, 0))
                retval = "o";
            else
                retval = "oT";
            break;

        case REG_TYPE_CONSTINT:
            retval = "i";
            break;

        case REG_TYPE_COLOROUT:
            retval = "oC";
            break;

        case REG_TYPE_DEPTHOUT:
            retval = "oDepth";
            has_number = 0;
            break;

        case REG_TYPE_SAMPLER:
            retval = "s";
            break;

        case REG_TYPE_CONSTBOOL:
            retval = "b";
            break;

        case REG_TYPE_LOOP:
            retval = "aL";
            has_number = 0;
            break;

        case REG_TYPE_MISCTYPE:
            switch ((const MiscTypeType) regnum)
            {
                case MISCTYPE_TYPE_POSITION: retval = "vPos"; break;
                case MISCTYPE_TYPE_FACE: retval = "vFace"; break;
            } // switch
            has_number = 0;
            break;

        case REG_TYPE_LABEL:
            retval = "l";
            break;

        case REG_TYPE_PREDICATE:
            retval = "p";
            break;

        //case REG_TYPE_TEMPFLOAT16:  // !!! FIXME: don't know this asm string
        default:
            fail(ctx, "unknown register type");
            retval = "???";
            has_number = 0;
            break;
    } // switch

    if (has_number)
        snprintf(regnum_str, regnum_size, "%u", (uint) regnum);
    else
        regnum_str[0] = '\0';

    return retval;
} // get_D3D_register_string

// !!! FIXME: These should stay in the mojoshader_profile_d3d file
// !!! FIXME: but ARB1 relies on them, so we have to move them here.
// !!! FIXME: If/when we kill off ARB1, we can move these back.

const char *get_D3D_varname_in_buf(Context *ctx, RegisterType rt,
                                           int regnum, char *buf,
                                           const size_t len)
{
    char regnum_str[16];
    const char *regtype_str = get_D3D_register_string(ctx, rt, regnum,
                                              regnum_str, sizeof (regnum_str));
    snprintf(buf,len,"%s%s", regtype_str, regnum_str);
    return buf;
} // get_D3D_varname_in_buf


const char *get_D3D_varname(Context *ctx, RegisterType rt, int regnum)
{
    char buf[64];
    get_D3D_varname_in_buf(ctx, rt, regnum, buf, sizeof (buf));
    return StrDup(ctx, buf);
} // get_D3D_varname
