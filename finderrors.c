#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <pthread.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>

#include "mojoshader.h"

static const char *profile = NULL;
static volatile int die_threads = 0;
static pthread_mutex_t grab_mutex;
static pthread_mutex_t report_mutex;

typedef struct ShaderBytecode
{
    char *name;
    struct ShaderBytecode *next;
} ShaderBytecode;

static volatile ShaderBytecode *gbytecode = NULL;

static void report(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    pthread_mutex_lock(&report_mutex);
    vprintf(fmt, ap);
    pthread_mutex_unlock(&report_mutex);
    va_end(ap);
} // report

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

        const char *fname = my_bytecode->name;
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
            {
                const MOJOSHADER_parseData *pd;
                pd = MOJOSHADER_parse(profile, buf, rc, 0, 0, 0);
                if (pd->error != NULL)
                    report("FAIL: %s %s\n", fname, pd->error);
                else
                    report("PASS: %s\n", fname);
                MOJOSHADER_freeParseData(pd);
            } // else
        } // else

        free(my_bytecode->name);
        free(my_bytecode);
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
            bytecode->name = (char *) malloc(strlen(dent->d_name) + dirlen);
            sprintf(bytecode->name, "%s/%s", dname, dent->d_name);
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

        pthread_mutex_init(&grab_mutex, NULL);
        pthread_mutex_init(&report_mutex, NULL);

        profile = argv[1];
        int i;

        for (i = 2; i < argc; i++)
            total += do_dir(argv[i]);

        printf("Saw %d bytecode files.\n", total);

        for (i = 0; i < MAX_WORKERS; i++)
            pthread_create(&workers[i], NULL, worker, NULL);

        for (i = 0; i < MAX_WORKERS; i++)
            pthread_join(workers[i], NULL);

        pthread_mutex_destroy(&report_mutex);
        pthread_mutex_destroy(&grab_mutex);
    } // else

    return 0;
} // main

// end of finderrors.c ...

