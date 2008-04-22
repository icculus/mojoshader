#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <pthread.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>

#include "mojoshader.h"

#define FINDERRORS_COMPILE_SHADERS 1

#if FINDERRORS_COMPILE_SHADERS
#include "SDL.h"
#include <gl.h>
#include <glext.h>
#endif

static const char *profile = NULL;
static volatile int die_threads = 0;
static pthread_mutex_t grab_mutex;
static pthread_mutex_t report_mutex;

typedef struct ShaderBytecode
{
    void *name;
    void *data;
    struct ShaderBytecode *next;
} ShaderBytecode;

static volatile ShaderBytecode *gbytecode = NULL;
static volatile ShaderBytecode *gparsed = NULL;

static void report(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    pthread_mutex_lock(&report_mutex);
    vprintf(fmt, ap);
    pthread_mutex_unlock(&report_mutex);
    va_end(ap);
} // report


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


static void *worker(void *unused)
{
    unsigned char buf[1024 * 256];
    while (!die_threads)
    {
        ShaderBytecode *my_bytecode = NULL;
        pthread_mutex_lock(&grab_mutex);
        if (gbytecode == NULL)
            die_threads = 1;
        else
        {
            my_bytecode = (ShaderBytecode *) gbytecode;
            gbytecode = gbytecode->next;
        } // else
        pthread_mutex_unlock(&grab_mutex);

        if (my_bytecode == NULL)
            break;

        const char *fname = (const char *) my_bytecode->name;
        FILE *io = fopen(fname, "rb");
        if (io == NULL)
            report("FAIL: %s fopen() failed.\n", fname);
        else
        {
            int rc = fread(buf, 1, sizeof (buf), io);
            fclose(io);
            if (rc == -1)
                report("FAIL: %s %s\n", fname, strerror(errno));
            else
                my_bytecode->data = (void *) MOJOSHADER_parse(profile, buf, rc, 0, 0, 0);

            pthread_mutex_lock(&grab_mutex);
            my_bytecode->next = (ShaderBytecode *) gparsed;
            gparsed = my_bytecode;
            pthread_mutex_unlock(&grab_mutex);
        } // else
    } // while

    return NULL;
} // worker


static int do_dir(const char *dname)
{
    const int dirlen = strlen(dname) + 1;
    int total = 0;
    DIR *dirp = opendir(dname);
    if (dirp != NULL)
    {
        ShaderBytecode *bytecode = NULL;
        struct dirent *dent;
        while ((dent = readdir(dirp)) != NULL)
        {
            if (strstr(dent->d_name, ".bytecode") == NULL)
                continue;

            total++;
            bytecode = (ShaderBytecode *) malloc(sizeof (ShaderBytecode));
            bytecode->data = NULL;
            bytecode->name = malloc(strlen(dent->d_name) + dirlen);
            sprintf((char *) bytecode->name, "%s/%s", dname, dent->d_name);
            bytecode->next = (ShaderBytecode *) gbytecode;
            gbytecode = bytecode;
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
        #define MAX_WORKERS 4
        pthread_t workers[MAX_WORKERS];
        int total = 0;

        #if FINDERRORS_COMPILE_SHADERS
        SDL_Init(SDL_INIT_VIDEO);
        SDL_SetVideoMode(640, 480, 0, SDL_OPENGL);
        #endif

        pthread_mutex_init(&grab_mutex, NULL);
        pthread_mutex_init(&report_mutex, NULL);

        profile = argv[1];
        int i;

        for (i = 2; i < argc; i++)
            total += do_dir(argv[i]);

        printf("Saw %d bytecode files.\n", total);

        for (i = 0; i < MAX_WORKERS; i++)
            pthread_create(&workers[i], NULL, worker, NULL);

        while (1)
        {
            ShaderBytecode *my_bytecode = NULL;
            pthread_mutex_lock(&grab_mutex);
            if (gparsed != NULL)
            {
                my_bytecode = (ShaderBytecode *) gparsed;
                gparsed = gparsed->next;
            } // if
            pthread_mutex_unlock(&grab_mutex);

            if (my_bytecode == NULL)
            {
                if (gbytecode == NULL)
                    break;
                else
                {
                    usleep(10000);
                    continue;
                } // else
            } // if

            const MOJOSHADER_parseData *pd = (const MOJOSHADER_parseData *)
                                                my_bytecode->data;
            const char *fname = my_bytecode->name;
            if (pd != NULL)
            {
                if (pd->error != NULL)
                    report("FAIL: %s %s\n", fname, pd->error);
                else
                {
                    if (compile_shader(fname, pd))
                        report("PASS: %s\n", fname);
                } // else
                MOJOSHADER_freeParseData(pd);
            } // if

            free(my_bytecode->name);
            free(my_bytecode);
        } // while

        for (i = 0; i < MAX_WORKERS; i++)
            pthread_join(workers[i], NULL);

        pthread_mutex_destroy(&report_mutex);
        pthread_mutex_destroy(&grab_mutex);

        #if FINDERRORS_COMPILE_SHADERS
        SDL_Quit();
        #endif
    } // else

    return 0;
} // main

// end of finderrors.c ...

