/**
 * d3d2glsl; generate GLSL programs from bytecode of compiled Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#ifndef __INCL_D3D2GLSL_H_
#define __INCL_D3D2GLSL_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef void *(*D3D2GLSL_malloc)(int bytes);
typedef void (*D3D2GLSL_free)(void *ptr);

/* !!! FIXME: documentation. */
/* !!! FIXME: this needs to change to return a buffer of GLSL code. */
int D3D2GLSL_parse(const char *profile, const unsigned char *tokenbuf,
                   const unsigned int bufsize, D3D2GLSL_malloc m,
                   D3D2GLSL_free f);

#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of d3d2glsl.h ... */

