/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#ifndef __INCL_MOJOSHADER_H_
#define __INCL_MOJOSHADER_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
 * const int compiled_against = MOJOSHADER_VERSION;
 * const int linked_against = MOJOSHADER_version();
 */
#define MOJOSHADER_VERSION 1
int MOJOSHADER_version(void);

/*
 * These allocators work just like the C runtime's malloc() and free()
 *  (in fact, they use malloc() and free() internally if you don't
 *  specify your own allocator).
 */
typedef void *(*MOJOSHADER_malloc)(int bytes);
typedef void (*MOJOSHADER_free)(void *ptr);

/* !!! FIXME: documentation. */
/* !!! FIXME: this needs to change to return a buffer of GLSL code. */
int MOJOSHADER_parse(const char *profile, const unsigned char *tokenbuf,
                   const unsigned int bufsize, MOJOSHADER_malloc m,
                   MOJOSHADER_free f);

#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of mojoshader.h ... */

