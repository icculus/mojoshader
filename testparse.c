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


static void do_parse(unsigned char *buf, int len, const char *prof)
{
    const MOJOSHADER_parseData *pd;

    pd = MOJOSHADER_parse(prof, buf, len, Malloc, Free);
    printf("PROFILE: %s\n", prof);
    printf("SHADER TYPE: %s\n", shader_type(pd->shader_type));
    printf("VERSION: %d.%d\n", pd->major_ver, pd->minor_ver);
    printf("INSTRUCTION COUNT: %d\n", (int) pd->instruction_count);
    if (pd->error != NULL)
        printf("ERROR: %s\n", pd->error);
    if (pd->output != NULL)
        printf("OUTPUT:\n%s\n", pd->output);
    printf("\n\n");
    MOJOSHADER_freeParseData(pd);
} // do_parse


int main(int argc, char **argv)
{
    int i;

    printf("MojoShader testparse\n");
    printf("Compiled against version %d\n", MOJOSHADER_VERSION);
    printf("Linked against version %d\n", MOJOSHADER_version());
    printf("\n");

    if (argc == 1)
        printf("No files specified.\n");
    else
    {
        for (i = 1; i < argc; i++)
        {
            FILE *io = fopen(argv[i], "rb");
            printf("FILE: %s\n", argv[i]);
            if (io == NULL)
                printf("fopen('%s') failed.\n", argv[i]);
            else
            {
                unsigned char *buf = (unsigned char *) malloc(1000000);
                int rc = fread(buf, 1, 1000000, io);
                fclose(io);
                do_parse(buf, rc, MOJOSHADER_PROFILE_D3D);
                do_parse(buf, rc, MOJOSHADER_PROFILE_GLSL);
                free(buf);
            } // else
        } // for
    } // else

    return 0;
} // main

// end of testparse.c ...

