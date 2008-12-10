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

static int assemble(const char *buf, const char *outfile)
{
    FILE *io = fopen(outfile, "wb");
    if (io == NULL)
    {
        printf(" ... fopen('%s') failed.\n", outfile);
        return 0;
    } // if

    const MOJOSHADER_parseData *pd;
    int retval = 0;

    pd = MOJOSHADER_assemble(buf, NULL, NULL, NULL);
    if (pd->error != NULL)
        printf("ERROR: %s\n", pd->error);
    else
    {
        if (pd->output != NULL)
        {
            if (fwrite(pd->output, pd->output_len, 1, io) != 1)
                printf(" ... fwrite('%s') failed.\n", outfile);
            else if (fclose(io) == EOF)
                printf(" ... fclose('%s') failed.\n", outfile);
            else
                retval = 1;
        } // if
    } // else
    MOJOSHADER_freeParseData(pd);

    return retval;
} // assemble


int main(int argc, char **argv)
{
    int retval = 1;

    if (argc != 3)
        printf("\n\nUSAGE: %s <d3dasmfile> <outputfile>\n\n", argv[0]);
    else
    {
        const char *infile = argv[1];
        const char *outfile = argv[2];
        FILE *io = fopen(infile, "rb");
        if (io == NULL)
            printf(" ... fopen('%s') failed.\n", infile);
        else
        {
            char *buf = (char *) malloc(1000000);
            int rc = fread(buf, 1, 1000000-1, io);
            fclose(io);
            if (rc == EOF)
                printf(" ... fread('%s') failed.\n", infile);
            else
            {
                buf[rc] = '\0';
                if (assemble(buf, outfile))
                    retval = 0;
                else
                    remove(outfile);
                free(buf);
            } // else
        } // for
    } // else

    return retval;
} // main

// end of assemble.c ...

