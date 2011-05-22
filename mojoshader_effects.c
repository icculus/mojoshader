/**
 * MojoShader; generate shader programs from bytecode of compiled
 *  Direct3D shaders.
 *
 * Please see the file LICENSE.txt in the source's root directory.
 *
 *  This file written by Ryan C. Gordon.
 */

#define __MOJOSHADER_INTERNAL__ 1
#include "mojoshader_internal.h"


static MOJOSHADER_effect MOJOSHADER_out_of_mem_effect = {
    1, &MOJOSHADER_out_of_mem_error, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static uint32 readui32(const uint8 **_ptr, uint32 *_len)
{
    uint32 retval = 0;
    if (*_len < sizeof (retval))
        *_len = 0;
    else
    {
        const uint32 *ptr = (const uint32 *) *_ptr;
        retval = SWAP32(*ptr);
        *_ptr += sizeof (retval);
        *_len -= sizeof (retval);
    } // else
    return retval;
} // readui32

// !!! FIXME: this is sort of a big, ugly function.
const MOJOSHADER_effect *MOJOSHADER_parseEffect(const char *profile,
                                                const unsigned char *buf,
                                                const unsigned int _len,
                                                const MOJOSHADER_swizzle *swiz,
                                                const unsigned int swizcount,
                                                MOJOSHADER_malloc m,
                                                MOJOSHADER_free f,
                                                void *d)
{
    if ( ((m == NULL) && (f != NULL)) || ((m != NULL) && (f == NULL)) )
        return &MOJOSHADER_out_of_mem_effect;  // supply both or neither.

    if (m == NULL) m = MOJOSHADER_internal_malloc;
    if (f == NULL) f = MOJOSHADER_internal_free;

    MOJOSHADER_effect *retval = m(sizeof (MOJOSHADER_effect), d);
    if (retval == NULL)
        return &MOJOSHADER_out_of_mem_effect;  // supply both or neither.
    memset(retval, '\0', sizeof (*retval));

    retval->malloc = m;
    retval->free = f;
    retval->malloc_data = d;

    const uint8 *ptr = (const uint8 *) buf;
    uint32 len = (uint32) _len;
    size_t siz = 0;
    int i, j, k;

    if (len < 8)
        goto parseEffect_unexpectedEOF;

    if (readui32(&ptr, &len) != 0xFEFF0901) // !!! FIXME: is this always magic?
        goto parseEffect_notAnEffectsFile;
    else
    {
        const uint32 offset = readui32(&ptr, &len);
        if (offset > len)
            goto parseEffect_unexpectedEOF;
        ptr += offset;
        len -= offset;
    } // else

    // params...

    if (len < 16)
        goto parseEffect_unexpectedEOF;

    const uint32 numparams = readui32(&ptr, &len);
    const uint32 numtechniques = readui32(&ptr, &len);

    readui32(&ptr, &len); // !!! FIXME: there are 8 unknown bytes here.
    readui32(&ptr, &len);

    for (i = 0; i < numparams; i++)
    {
        if (len < 16)
            goto parseEffect_unexpectedEOF;

        /*const uint32 startoffset = */ readui32(&ptr, &len);
        /*const uint32 endoffset = */ readui32(&ptr, &len);
        readui32(&ptr, &len);  // !!! FIXME: don't know what this is.
        const uint32 numannos = readui32(&ptr, &len);
        for (j = 0; j < numannos; j++)
        {
            if (len < 8)
                goto parseEffect_unexpectedEOF;
            // !!! FIXME: parse annotations.
            readui32(&ptr, &len);
            readui32(&ptr, &len);
        } // for
    } // for

    uint32 numshaders = 0;  // we'll calculate this later.

    // techniques...

    if (numtechniques > 0)
    {
        siz = sizeof (MOJOSHADER_effectTechnique) * numtechniques;
        retval->techniques = (MOJOSHADER_effectTechnique *) m(siz, d);
        if (retval->techniques == NULL)
            goto parseEffect_outOfMemory;
        memset(retval->techniques, '\0', siz);

        retval->technique_count = numtechniques;

        for (i = 0; i < numtechniques; i++)
        {
            if (len < 12)
                goto parseEffect_unexpectedEOF;
            
            MOJOSHADER_effectTechnique *technique = &retval->techniques[i];

            // !!! FIXME: is this always 12?
            const uint32 nameoffset = readui32(&ptr, &len) + 12;
            readui32(&ptr, &len);  // !!! FIXME: don't know what this field does.
            const uint32 numpasses = readui32(&ptr, &len);

            if (nameoffset >= _len)
                goto parseEffect_unexpectedEOF;

            if (numpasses > 0)
            {
                // !!! FIXME: verify this doesn't go past EOF looking for a null.
                siz = strlen(((const char *) buf) + nameoffset) + 1;
                technique->name = (char *) m(siz, d);
                if (technique->name == NULL)
                    goto parseEffect_outOfMemory;
                strcpy((char *) technique->name, ((const char *) buf) + nameoffset);

                technique->pass_count = numpasses;

                siz = sizeof (MOJOSHADER_effectPass) * numpasses;
                technique->passes = (MOJOSHADER_effectPass *) m(siz, d);
                if (technique->passes == NULL)
                    goto parseEffect_outOfMemory;
                memset(technique->passes, '\0', siz);

                for (j = 0; j < numpasses; j++)
                {
                    if (len < 12)
                        goto parseEffect_unexpectedEOF;

                    MOJOSHADER_effectPass *pass = &technique->passes[j];

                    // !!! FIXME: is this always 12?
                    const uint32 passnameoffset = readui32(&ptr, &len) + 12;
                    readui32(&ptr, &len);  // !!! FIXME: don't know what this field does.
                    const uint32 numstates = readui32(&ptr, &len);

                    if (passnameoffset >= _len)
                        goto parseEffect_unexpectedEOF;

                    // !!! FIXME: verify this doesn't go past EOF looking for a null.
                    siz = strlen(((const char *) buf) + passnameoffset) + 1;
                    pass->name = (char *) m(siz, d);
                    if (pass->name == NULL)
                        goto parseEffect_outOfMemory;
                    strcpy((char *) pass->name, ((const char *) buf) + passnameoffset);

                    if (numstates > 0)
                    {
                        pass->state_count = numstates;

                        siz = sizeof (MOJOSHADER_effectState) * numstates;
                        pass->states = (MOJOSHADER_effectState *) m(siz, d);
                        if (pass->states == NULL)
                            goto parseEffect_outOfMemory;
                        memset(pass->states, '\0', siz);

                        for (k = 0; k < numstates; k++)
                        {
                            if (len < 16)
                                goto parseEffect_unexpectedEOF;

                            MOJOSHADER_effectState *state = &pass->states[k];
                            const uint32 type = readui32(&ptr, &len);
                            readui32(&ptr, &len);  // !!! FIXME: don't know what this field does.
                            /*const uint32 offsetend = */ readui32(&ptr, &len);
                            /*const uint32 offsetstart = */ readui32(&ptr, &len);
                            state->type = type;

                            if ((type == 0x92) || (type == 0x93))
                                numshaders++;
                        } // for
                    } // if
                } // for
            } // if
        } // for
    } // if

    // textures...

    if (len < 8)
        goto parseEffect_unexpectedEOF;

    const int numtextures = readui32(&ptr, &len);
    const int numobjects = readui32(&ptr, &len);  // !!! FIXME: "objects" for lack of a better word.

    if (numtextures > 0)
    {
        siz = sizeof (MOJOSHADER_effectTexture) * numtextures;
        retval->textures = m(siz, d);
        if (retval->textures == NULL)
            goto parseEffect_outOfMemory;
        memset(retval->textures, '\0', siz);

        for (i = 0; i < numtextures; i++)
        {
            if (len < 8)
                goto parseEffect_unexpectedEOF;

            MOJOSHADER_effectTexture *texture = &retval->textures[i];
            const uint32 texparam = readui32(&ptr, &len);
            const uint32 texsize = readui32(&ptr, &len);
            // apparently texsize will pad out to 32 bits.
            const uint32 readsize = (((texsize + 3) / 4) * 4);
            if (len < readsize)
                goto parseEffect_unexpectedEOF;

            texture->param = texparam;
            char *str = m(texsize + 1, d);
            if (str == NULL)
                goto parseEffect_outOfMemory;
            memcpy(str, ptr, texsize);
            str[texsize] = '\0';
            texture->name = str;

            ptr += readsize;
            len -= readsize;
        } // for
    } // if

    // shaders...

    if (numshaders > 0)
    {
        siz = sizeof (MOJOSHADER_effectShader) * numshaders;
        retval->shaders = (MOJOSHADER_effectShader *) m(siz, d);
        if (retval->shaders == NULL)
            goto parseEffect_outOfMemory;
        memset(retval->shaders, '\0', siz);

        retval->shader_count = numshaders;

        // !!! FIXME: I wonder if we should pull these from offsets and not
        // !!! FIXME:  count on them all being in a line like this.
        for (i = 0; i < numshaders; i++)
        {
            if (len < 24)
                goto parseEffect_unexpectedEOF;

            MOJOSHADER_effectShader *shader = &retval->shaders[i];
            const uint32 technique = readui32(&ptr, &len);
            const uint32 pass = readui32(&ptr, &len);
            readui32(&ptr, &len);  // !!! FIXME: don't know what this does.
            readui32(&ptr, &len);  // !!! FIXME: don't know what this does (vertex/pixel/geometry?)
            readui32(&ptr, &len);  // !!! FIXME: don't know what this does.
            const uint32 shadersize = readui32(&ptr, &len);

            if (len < shadersize)
                goto parseEffect_unexpectedEOF;

            shader->technique = technique;
            shader->pass = pass;
            shader->shader = MOJOSHADER_parse(profile, ptr, shadersize,
                                              swiz, swizcount, m, f, d);

            // !!! FIXME: check for errors.

            ptr += shadersize;
            len -= shadersize;
        } // for
    } // if

    // !!! FIXME: we parse this, but don't expose the data, yet.
    // mappings ...
    assert(numshaders <= numobjects);
    const uint32 nummappings = numobjects - numshaders;
    if (nummappings > 0)
    {
        for (i = 0; i < nummappings; i++)
        {
            if (len < 24)
                goto parseEffect_unexpectedEOF;

            /*const uint32 magic = */ readui32(&ptr, &len);
            /*const uint32 index = */ readui32(&ptr, &len);
            readui32(&ptr, &len);  // !!! FIXME: what is this field?
            readui32(&ptr, &len);  // !!! FIXME: what is this field?
            /*const uint32 type = */ readui32(&ptr, &len);
            const uint32 mapsize = readui32(&ptr, &len);
            if (mapsize > 0)
            {
                const uint32 readsize = (((mapsize + 3) / 4) * 4);
                if (len < readsize)
                    goto parseEffect_unexpectedEOF;
            } // if
        } // for
    } // if

    retval->profile = (char *) m(strlen(profile) + 1, d);
    if (retval->profile == NULL)
        goto parseEffect_outOfMemory;
    strcpy((char *) retval->profile, profile);

    return retval;


// !!! FIXME: do something with this.
parseEffect_notAnEffectsFile:
parseEffect_unexpectedEOF:
parseEffect_outOfMemory:
    MOJOSHADER_freeEffect(retval);
    return &MOJOSHADER_out_of_mem_effect;
} // MOJOSHADER_parseEffect


void MOJOSHADER_freeEffect(const MOJOSHADER_effect *_effect)
{
    MOJOSHADER_effect *effect = (MOJOSHADER_effect *) _effect;
    if ((effect == NULL) || (effect == &MOJOSHADER_out_of_mem_effect))
        return;  // no-op.

    MOJOSHADER_free f = effect->free;
    void *d = effect->malloc_data;
    int i, j;

    for (i = 0; i < effect->error_count; i++)
    {
        f((void *) effect->errors[i].error, d);
        f((void *) effect->errors[i].filename, d);
    } // for
    f((void *) effect->errors, d);

    f((void *) effect->profile, d);

    for (i = 0; i < effect->technique_count; i++)
    {
        MOJOSHADER_effectTechnique *technique = &effect->techniques[i];
        for (j = 0; j < technique->pass_count; j++)
            f(technique->passes[j].states, d);
        f(technique->passes, d);
    } // for

    f(effect->techniques, d);

    for (i = 0; i < effect->texture_count; i++)
        f((void *) effect->textures[i].name, d);
    f(effect->textures, d);

    for (i = 0; i < effect->shader_count; i++)
        MOJOSHADER_freeParseData(effect->shaders[i].shader);
    f(effect->shaders, d);

    f(effect, d);
} // MOJOSHADER_freeEffect

// end of mojoshader_effects.c ...

