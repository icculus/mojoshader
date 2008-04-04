#include <stdio.h>
#include <stdlib.h>
#include "mojoshader.h"


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


static void do_parse(const unsigned char *buf, const int len, const char *prof)
{
    const MOJOSHADER_parseData *pd;

    pd = MOJOSHADER_parse(prof, buf, len, Malloc, Free);
    printf("PROFILE: %s\n", prof);
    if (pd->error != NULL)
        printf("ERROR: %s\n", pd->error);
    else
    {
        printf("SHADER TYPE: %s\n", shader_type(pd->shader_type));
        printf("VERSION: %d.%d\n", pd->major_ver, pd->minor_ver);
        printf("INSTRUCTION COUNT: %d\n", (int) pd->instruction_count);
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
                printf("    * %d: %s\n", u->index, typenames[(int) u->type]);
            } // for
        } // else

        if (pd->output != NULL)
            printf("OUTPUT:\n%s\n", pd->output);
    } // else
    printf("\n\n");
    MOJOSHADER_freeParseData(pd);
} // do_parse


int main(int argc, char **argv)
{
    printf("MojoShader testparse\n");
    printf("Compiled against version %d\n", MOJOSHADER_VERSION);
    printf("Linked against version %d\n", MOJOSHADER_version());
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
                do_parse(buf, rc, profile);
                free(buf);
            } // else
        } // for
    } // else

    return 0;
} // main

// end of testparse.c ...

