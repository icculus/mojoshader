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
#include "SDL.h"
#include "gl.h"
#include "glext.h"
#endif

#define report printf

static int compile_shader(const char *fname, const MOJOSHADER_parseData *pd)
{
    int retval = 1;

    #if FINDERRORS_COMPILE_SHADERS
    const GLenum shader_type = (pd->shader_type == MOJOSHADER_TYPE_PIXEL) ? GL_FRAGMENT_SHADER_ARB : GL_VERTEX_SHADER_ARB;
    GLint shaderlen = (GLint) pd->output_len;
    GLhandleARB program = glCreateProgramObjectARB();
    GLhandleARB shader = glCreateShaderObjectARB(shader_type);
    GLint ok = 0;
    GLcharARB err[1024];
    GLsizei len = 0;

    retval = 0;

    glShaderSourceARB(shader, 1, (const GLcharARB **) &pd->output, &shaderlen);
    glCompileShaderARB(shader);
    glGetObjectParameterivARB(shader, GL_OBJECT_COMPILE_STATUS_ARB, &ok);
    if (!ok)
    {
        glGetInfoLogARB(shader, sizeof (err), &len, err);
        printf("FAIL: %s glsl compile: %s\n", fname, err);
    } // if
    else
    {
        glAttachObjectARB(program, shader);
        glLinkProgramARB(program);
        glGetObjectParameterivARB(program, GL_OBJECT_LINK_STATUS_ARB, &ok);
        if (!ok)
        {
            glGetInfoLogARB(program, sizeof (err), &len, err);
            printf("FAIL: %s glsl link: %s\n", fname, err);
        } // if
        else
        {
            retval = 1;
        } // else
    } // else
    glDeleteObjectARB(shader);
    glDeleteObjectARB(program);
    #endif

    return retval;
} // compile_shader


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
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
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

            const MOJOSHADER_parseData *pd = MOJOSHADER_parse(profile, buf, rc, 0, 0, 0);
            if (pd->error != NULL)
                report("FAIL: %s %s\n", fname, pd->error);
            else
            {
                if (compile_shader(fname, pd))
                    report("PASS: %s\n", fname);
            } // else
            MOJOSHADER_freeParseData(pd);
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

        #if FINDERRORS_COMPILE_SHADERS
        SDL_Init(SDL_INIT_VIDEO);
        SDL_SetVideoMode(640, 480, 0, SDL_OPENGL);
        glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
        #endif

        const char *profile = argv[1];

        for (i = 2; i < argc; i++)
            total += do_dir(argv[i], profile);

        printf("Saw %d bytecode files.\n", total);

        #if FINDERRORS_COMPILE_SHADERS
        SDL_Quit();
        #endif
    } // else

    return 0;
} // main

// end of finderrors.c ...

