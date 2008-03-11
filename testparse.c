#include <stdio.h>
#include <stdlib.h>
#include "d3d2glsl.h"

int main(int argc, char **argv)
{
    if (argv[1] != NULL)
    {
        FILE *io = fopen(argv[1], "rb");
        if (io != NULL)
        {
            unsigned char *buf = (unsigned char *) malloc(1000000);
            int rc = fread(buf, 1, 1000000, io);
            fclose(io);
            D3D2GLSL_parse("d3d", buf, rc);
            free(buf);
        } // if
    } // if

    return 0;
} // main

// end of testparse.c ...

