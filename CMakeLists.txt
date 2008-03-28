CMAKE_MINIMUM_REQUIRED(VERSION 2.4)
PROJECT(MojoShader)

IF(CMAKE_COMPILER_IS_GNUCC)
    ADD_DEFINITIONS(-Wall -ggdb3)
ENDIF(CMAKE_COMPILER_IS_GNUCC)

# testparse uses this when I'm looking at memory usage patterns.
#ADD_DEFINITIONS(-DMOJOSHADER_DEBUG_MALLOC=1)

ADD_EXECUTABLE(testparse testparse.c mojoshader.c)

# End of CMakeLists.txt ...

