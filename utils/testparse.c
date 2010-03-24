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
#include "mojoshader.h"

#ifdef _MSC_VER
#define snprintf _snprintf
#endif

#if MOJOSHADER_DEBUG_MALLOC
static void *Malloc(int len)
{
    void *ptr = malloc(len + sizeof (int));
    int *store = (int *) ptr;
    printf("malloc() %d bytes (%p)\n", len, ptr);
    if (ptr == NULL) return NULL;
    *store = len;
    return (void *) (store + 1);
} // Malloc


static void Free(void *_ptr)
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

static const char *shader_type(const MOJOSHADER_shaderType s)
{
    switch (s)
    {
        case MOJOSHADER_TYPE_UNKNOWN: return "unknown";
        case MOJOSHADER_TYPE_PIXEL: return "pixel";
        case MOJOSHADER_TYPE_VERTEX: return "vertex";
        case MOJOSHADER_TYPE_GEOMETRY: return "geometry";
        default: return "(bogus value?)";
    } // switch

    return NULL;  // shouldn't hit this.
} // shader_type


static int do_parse(const char *fname, const unsigned char *buf,
                    const int len, const char *prof)
{
    const MOJOSHADER_parseData *pd;
    int retval = 0;

    pd = MOJOSHADER_parse(prof, buf, len, NULL, 0, Malloc, Free, NULL);
    printf("PROFILE: %s\n", prof);
    if (pd->error_count > 0)
    {
        int i;
        for (i = 0; i < pd->error_count; i++)
        {
            printf("%s:%d: ERROR: %s\n",
                    pd->errors[i].filename ? pd->errors[i].filename : fname,
                    pd->errors[i].error_position,
                    pd->errors[i].error);
        } // for
    } // if
    else
    {
        retval = 1;
        printf("SHADER TYPE: %s\n", shader_type(pd->shader_type));
        printf("VERSION: %d.%d\n", pd->major_ver, pd->minor_ver);
        printf("INSTRUCTION COUNT: %d\n", (int) pd->instruction_count);

        printf("ATTRIBUTES:");
        if (pd->attribute_count == 0)
            printf(" (none.)\n");
        else
        {
            int i;
            printf("\n");
            for (i = 0; i < pd->attribute_count; i++)
            {
                static const char *usagenames[] = {
                    "position", "blendweight", "blendindices", "normal",
                    "psize", "texcoord", "tangent", "binormal", "tessfactor",
                    "positiont", "color", "fog", "depth", "sample"
                };
                const MOJOSHADER_attribute *a = &pd->attributes[i];
                char numstr[16] = { 0 };
                if (a->index != 0)
                    snprintf(numstr, sizeof (numstr), "%d", a->index);
                printf("    * %s%s", usagenames[(int) a->usage], numstr);
                if (a->name != NULL)
                    printf(" (\"%s\")", a->name);
                printf("\n");
            } // for
        } // else

        printf("CONSTANTS:");
        if (pd->constant_count == 0)
            printf(" (none.)\n");
        else
        {
            int i;
            printf("\n");
            for (i = 0; i < pd->constant_count; i++)
            {
                static const char *typenames[] = { "float", "int", "bool" };
                const MOJOSHADER_constant *c = &pd->constants[i];
                printf("    * %d: %s (", c->index, typenames[(int) c->type]);
                if (c->type == MOJOSHADER_UNIFORM_FLOAT)
                {
                    printf("%f %f %f %f", c->value.f[0], c->value.f[1],
                                          c->value.f[2], c->value.f[3]);
                } // if
                else if (c->type == MOJOSHADER_UNIFORM_INT)
                {
                    printf("%d %d %d %d", c->value.i[0], c->value.i[1],
                                          c->value.i[2], c->value.i[3]);
                } // else if
                else if (c->type == MOJOSHADER_UNIFORM_BOOL)
                {
                    printf("%s", c->value.b ? "true" : "false");
                } // else if
                else
                {
                    printf("???");
                } // else
                printf(")\n");
            } // for
        } // else

        printf("UNIFORMS:");
        if (pd->uniform_count == 0)
            printf(" (none.)\n");
        else
        {
            int i;
            printf("\n");
            for (i = 0; i < pd->uniform_count; i++)
            {
                static const char *typenames[] = { "float", "int", "bool" };
                const MOJOSHADER_uniform *u = &pd->uniforms[i];
                const char *arrayof = "";
                const char *constant = u->constant ? "const " : "";
                char arrayrange[64] = { '\0' };
                if (u->array_count > 0)
                {
                    arrayof = "array[";
                    snprintf(arrayrange, sizeof (arrayrange), "%d] ",
                             u->array_count);
                } // if

                printf("    * %d: %s%s%s%s", u->index, constant, arrayof,
                        arrayrange, typenames[(int) u->type]);
                if (u->name != NULL)
                    printf(" (\"%s\")", u->name);
                printf("\n");
            } // for
        } // else

        printf("SAMPLERS:");
        if (pd->sampler_count == 0)
            printf(" (none.)\n");
        else
        {
            int i;
            printf("\n");
            for (i = 0; i < pd->sampler_count; i++)
            {
                static const char *typenames[] = { "2d", "cube", "volume" };
                const MOJOSHADER_sampler *s = &pd->samplers[i];
                printf("    * %d: %s", s->index, typenames[(int) s->type]);
                if (s->name != NULL)
                    printf(" (\"%s\")", s->name);
                printf("\n");
            } // for
        } // else

        if (pd->output != NULL)
        {
            int i;
            printf("OUTPUT:\n");
            for (i = 0; i < pd->output_len; i++)
                putchar((int) pd->output[i]);
            printf("\n");
        } // if
    } // else
    printf("\n\n");
    MOJOSHADER_freeParseData(pd);

    return retval;
} // do_parse


int main(int argc, char **argv)
{
    int retval = 0;

    printf("MojoShader testparse\n");
    printf("Compiled against changeset %s\n", MOJOSHADER_CHANGESET);
    printf("Linked against changeset %s\n", MOJOSHADER_changeset());
    printf("\n");

    if (argc <= 2)
        printf("\n\nUSAGE: %s <profile> [file1] ... [fileN]\n\n", argv[0]);
    else
    {
        const char *profile = argv[1];
        int i;

        for (i = 2; i < argc; i++)
        {
            FILE *io = fopen(argv[i], "rb");
            printf("FILE: %s\n", argv[i]);
            if (io == NULL)
                printf(" ... fopen('%s') failed.\n", argv[i]);
            else
            {
                unsigned char *buf = (unsigned char *) malloc(1000000);
                int rc = fread(buf, 1, 1000000, io);
                fclose(io);
                if (!do_parse(argv[i], buf, rc, profile))
                    retval = 1;
                free(buf);
            } // else
        } // for
    } // else

    return retval;
} // main

// end of testparse.c ...

