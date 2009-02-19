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
#include "mojoshader.h"

static int preprocess(const char *fname, const char *buf, int len,
                      const char *outfile,
                      const MOJOSHADER_preprocessorDefine *defs,
                      unsigned int defcount)
{
    FILE *io = outfile ? fopen(outfile, "wb") : stdout;
    if (io == NULL)
    {
        printf(" ... fopen('%s') failed.\n", outfile);
        return 0;
    } // if

    const MOJOSHADER_preprocessData *pd;
    int retval = 0;

    pd = MOJOSHADER_preprocess(fname, buf, len, defs, defcount, NULL,
                               NULL, NULL, NULL, NULL);

    if (pd->error_count > 0)
    {
        int i;
        for (i = 0; i < pd->error_count; i++)
        {
            printf("%s:%d: ERROR: %s\n",
                    pd->errors[i].filename ? pd->errors[i].filename : "???",
                    pd->errors[i].error_position,
                    pd->errors[i].error);
        } // for
    } // if
    else
    {
        if (pd->output != NULL)
        {
            if (fwrite(pd->output, pd->output_len, 1, io) != 1)
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


int main(int argc, char **argv)
{
    int retval = 1;
    const char *infile = NULL;
    const char *outfile = NULL;
    int i;

    MOJOSHADER_preprocessorDefine *defs = NULL;
    unsigned int defcount = 0;

    for (i = 1; i < argc; i++)
    {
        const char *arg = argv[i];

        if (strcmp(arg, "-o") == 0)
        {
            if (outfile != NULL)
            {
                printf("multiple output files specified.\n");
                exit(1);
            } // if

            arg = argv[++i];
            if (arg == NULL)
            {
                printf("no filename after '-o'\n");
                exit(1);
            } // if
            outfile = arg;
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
            {
                printf("multiple input files specified.\n");
                exit(1);
            } // if
            infile = arg;
        } // else
    } // for

    if (infile == NULL)
    {
        printf("no input file specified.\n");
        exit(1);
    } // if

    FILE *io = fopen(infile, "rb");
    if (io == NULL)
        printf(" ... fopen('%s') failed.\n", infile);
    else
    {
        char *buf = (char *) malloc(1000000);
        int rc = fread(buf, 1, 1000000, io);
        fclose(io);
        if (rc == EOF)
            printf(" ... fread('%s') failed.\n", infile);
        else
        {
            if (preprocess(infile, buf, rc, outfile, defs, defcount))
                retval = 0;
            else
            {
                if (outfile != NULL)
                    remove(outfile);
            } // else
            free(buf);
        } // else
    } // else

    for (i = 0; i < defcount; i++)
        free((void *) defs[defcount].identifier);
    free(defs);

    return retval;
} // main

// end of preprocess.c ...

