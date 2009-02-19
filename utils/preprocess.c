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

static const char **include_paths = NULL;
static unsigned int include_path_count = 0;


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


void close_include(const char *data, MOJOSHADER_malloc m,
                   MOJOSHADER_free f, void *d)
{
    f((void *) data, d);
} // close_include


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

    pd = MOJOSHADER_preprocess(fname, buf, len, defs, defcount,
                               open_include, close_include, NULL, NULL, NULL);

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

        if (strcmp(arg, "-I") == 0)
        {
            arg = argv[++i];
            if (arg == NULL)
            {
                printf("no path after '-I'\n");
                exit(1);
            } // if
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
        free((void *) defs[i].identifier);
    free(defs);

    free(include_paths);

    return retval;
} // main

// end of preprocess.c ...

