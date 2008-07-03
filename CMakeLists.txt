CMAKE_MINIMUM_REQUIRED(VERSION 2.4)
PROJECT(MojoShader)

IF(CMAKE_COMPILER_IS_GNUCC)
    ADD_DEFINITIONS(-Wall -ggdb3)
ENDIF(CMAKE_COMPILER_IS_GNUCC)

# testparse uses this when I'm looking at memory usage patterns.
#ADD_DEFINITIONS(-DMOJOSHADER_DEBUG_MALLOC=1)

IF(MSVC)
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS=1)
    ADD_DEFINITIONS(-TP)  # force .c files to compile as C++.
ENDIF(MSVC)

ADD_LIBRARY(mojoshader STATIC mojoshader.c mojoshader_opengl.c)

FIND_PACKAGE(SDL)
IF(SDL_FOUND)
    INCLUDE_DIRECTORIES(${SDL_INCLUDE_DIR})
    ADD_DEFINITIONS(-DFINDERRORS_COMPILE_SHADERS=1)
    ADD_EXECUTABLE(glcaps glcaps.c)
    TARGET_LINK_LIBRARIES(glcaps ${SDL_LIBRARY})
    ADD_EXECUTABLE(bestprofile bestprofile.c)
    TARGET_LINK_LIBRARIES(bestprofile mojoshader ${SDL_LIBRARY})
    ADD_EXECUTABLE(availableprofiles availableprofiles.c)
    TARGET_LINK_LIBRARIES(availableprofiles mojoshader ${SDL_LIBRARY})
ENDIF(SDL_FOUND)

ADD_EXECUTABLE(testparse testparse.c)
TARGET_LINK_LIBRARIES(testparse mojoshader)
ADD_EXECUTABLE(testoutput testoutput.c)
TARGET_LINK_LIBRARIES(testoutput mojoshader)
ADD_EXECUTABLE(finderrors finderrors.c)
TARGET_LINK_LIBRARIES(finderrors mojoshader ${SDL_LIBRARY})

# End of CMakeLists.txt ...

