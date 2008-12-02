#ifndef _INCLUDE_MOJOSHADER_INTERNAL_H_
#define _INCLUDE_MOJOSHADER_INTERNAL_H_

#ifndef __MOJOSHADER_INTERNAL__
#error Do not include this header from your applications.
#endif

// Shader bytecode format is described at MSDN:
//  http://msdn2.microsoft.com/en-us/library/ms800307.aspx

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include "mojoshader.h"

#if (defined(__APPLE__) && defined(__MACH__))
#define PLATFORM_MACOSX 1
#endif

// This is the highest shader version we currently support.

#define MAX_SHADER_MAJOR 3
#define MAX_SHADER_MINOR 0


// If SUPPORT_PROFILE_* isn't defined, we assume an implicit desire to support.
//  You get all the profiles unless you go out of your way to disable them.

#ifndef SUPPORT_PROFILE_D3D
#define SUPPORT_PROFILE_D3D 1
#endif

#ifndef SUPPORT_PROFILE_PASSTHROUGH
#define SUPPORT_PROFILE_PASSTHROUGH 1
#endif

#ifndef SUPPORT_PROFILE_GLSL
#define SUPPORT_PROFILE_GLSL 1
#endif

#ifndef SUPPORT_PROFILE_ARB1
#define SUPPORT_PROFILE_ARB1 1
#endif


// Get basic wankery out of the way here...

#ifdef _WINDOWS
#define ENDLINE_STR "\r\n"
#else
#define ENDLINE_STR "\n";
#endif

typedef unsigned int uint;  // this is a printf() helper. don't use for code.

#ifdef _MSC_VER
#include <malloc.h>
#define snprintf _snprintf
typedef unsigned __int8 uint8;
typedef unsigned __int16 uint16;
typedef unsigned __int32 uint32;
typedef __int32 int32;
// Warning Level 4 considered harmful.  :)
#pragma warning(disable: 4100)  // "unreferenced formal parameter"
#pragma warning(disable: 4389)  // "signed/unsigned mismatch"
#else
#include <stdint.h>
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef int32_t int32;
#endif

#ifdef __GNUC__
#define ISPRINTF(x,y) __attribute__((format (printf, x, y)))
#else
#define ISPRINTF(x,y)
#endif

#define STATICARRAYLEN(x) ( (sizeof ((x))) / (sizeof ((x)[0])) )

// Special-case return values from the parsing pipeline...
#define FAIL (-1)
#define NOFAIL (-2)
#define END_OF_STREAM (-3)


// Byteswap magic...

#if ((defined __GNUC__) && (defined __POWERPC__))
    static inline uint32 SWAP32(uint32 x)
    {
        __asm__ __volatile__("lwbrx %0,0,%1" : "=r" (x) : "r" (&x));
        return x;
    } // SWAP32
    static inline uint16 SWAP16(uint16 x)
    {
        __asm__ __volatile__("lhbrx %0,0,%1" : "=r" (x) : "r" (&x));
        return x;
    } // SWAP16
#elif defined(__POWERPC__)
    static inline uint32 SWAP32(uint32 x)
    {
        return ( (((x) >> 24) & 0x000000FF) | (((x) >>  8) & 0x0000FF00) |
                 (((x) <<  8) & 0x00FF0000) | (((x) << 24) & 0xFF000000) );
    } // SWAP32
    static inline uint16 SWAP16(uint16 x)
    {
        return ( (((x) >> 8) & 0x00FF) | (((x) << 8) & 0xFF00) );
    } // SWAP16
#else
#   define SWAP16(x) (x)
#   define SWAP32(x) (x)
#endif

#endif  // _INCLUDE_MOJOSHADER_INTERNAL_H_

// end of mojoshader_internal.h ...

