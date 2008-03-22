#include <stdio.h>
#include <stdlib.h>
#include "mojoshader.h"

int main(int argc, char **argv)
{
    printf("Compiled against version %d\n", MOJOSHADER_VERSION);
    printf("Linked against version %d\n", MOJOSHADER_version());

    if (argv[1] != NULL)
    {
        FILE *io = fopen(argv[1], "rb");
        if (io != NULL)
        {
            unsigned char *buf = (unsigned char *) malloc(1000000);
            int rc = fread(buf, 1, 1000000, io);
            fclose(io);
            MOJOSHADER_parse("d3d", buf, rc, NULL, NULL);
            free(buf);
        } // if
    } // if

    return 0;
} // main

// end of testparse.c ...

