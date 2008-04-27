#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>

#include "mojoshader.h"

#define FINDERRORS_COMPILE_SHADERS 1

#if FINDERRORS_COMPILE_SHADERS
#define GL_GLEXT_PROTOTYPES 1
#define GL_GLEXT_LEGACY 1
#include "SDL.h"
#include "gl.h"
#include "glext.h"
#endif

#define report printf

static int do_dir(const char *dname, const char *profile)
{
    const int dirlen = strlen(dname) + 1;
    int total = 0;
    DIR *dirp = opendir(dname);
    if (dirp != NULL)
    {
        int do_quit = 0;
        struct dirent *dent;
        while ((dent = readdir(dirp)) != NULL)
        {
            #if FINDERRORS_COMPILE_SHADERS
            SDL_Event e;  // pump event queue to keep OS happy.
            while (SDL_PollEvent(&e))
            {
                if (e.type == SDL_QUIT)
                    do_quit = 1;
            } // while
            SDL_GL_SwapBuffers();
            #endif

            if (do_quit)
            {
                report("FAIL: user requested quit!\n");
                break;
            } // if

            if (strstr(dent->d_name, ".bytecode") == NULL)
                continue;

            total++;

            char *fname = (char *) alloca(strlen(dent->d_name) + dirlen);
            sprintf(fname, "%s/%s", dname, dent->d_name);
            FILE *io = fopen(fname, "rb");
            if (io == NULL)
            {
                report("FAIL: %s fopen() failed.\n", fname);
                continue;
            } // if

            static unsigned char buf[1024 * 256];
            int rc = fread(buf, 1, sizeof (buf), io);
            fclose(io);
            if (rc == -1)
            {
                report("FAIL: %s %s\n", fname, strerror(errno));
                continue;
            } // if

            #if FINDERRORS_COMPILE_SHADERS
            MOJOSHADER_glShader *shader = MOJOSHADER_glCompileShader(buf, rc);
            if (shader == NULL)
                report("FAIL: %s %s\n", fname, MOJOSHADER_glGetError());
            else
                report("PASS: %s\n", fname);
            MOJOSHADER_glDeleteShader(shader);
            #else
            const MOJOSHADER_parseData *pd = MOJOSHADER_parse(profile, buf, rc, 0, 0, 0);
            if (pd->error != NULL)
                report("FAIL: %s %s\n", fname, pd->error);
            else
                report("PASS: %s\n", fname);
            MOJOSHADER_freeParseData(pd);
            #endif
        } // while
        closedir(dirp);
    } // if

    return total;
} // do_dir


int main(int argc, char **argv)
{
    //printf("MojoShader finderrors\n");
    //printf("Compiled against version %d\n", MOJOSHADER_VERSION);
    //printf("Linked against version %d\n", MOJOSHADER_version());
    //printf("\n");

    if (argc <= 2)
        printf("\n\nUSAGE: %s <profile> [dir1] ... [dirN]\n\n", argv[0]);
    else
    {
        int total = 0;
        int i;
        const char *profile = argv[1];

        #if FINDERRORS_COMPILE_SHADERS
        SDL_Init(SDL_INIT_VIDEO);
        SDL_GL_LoadLibrary(NULL);
        SDL_SetVideoMode(640, 480, 0, SDL_OPENGL);
        if (!MOJOSHADER_glInit(profile, SDL_GL_GetProcAddress, 0, 0, 0))
        {
            printf("MOJOSHADER_glInit() fail: %s\n", MOJOSHADER_glGetError());
            SDL_Quit();
            return 1;
        } // if
        #endif

        for (i = 2; i < argc; i++)
            total += do_dir(argv[i], profile);

        printf("Saw %d bytecode files.\n", total);

        #if FINDERRORS_COMPILE_SHADERS
        MOJOSHADER_glDeinit();
        SDL_Quit();
        #endif
    } // else

    return 0;
} // main

// end of finderrors.c ...

