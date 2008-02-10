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

/* !!! FIXME: documentation. */
/* !!! FIXME: this needs to change to return a buffer of GLSL code. */
int D3D2GLSL_parse(const uint8 *tokenbuf, const uint32 bufsize);

#ifdef __cplusplus
}
#endif

#endif  /* include-once blocker. */

/* end of d3d2glsl.h ... */

