# THIS file is not used for default compilation
# might be used for windows version with cmake later - keep intact


PROJECT(EBImage)

INCLUDE_DIRECTORIES($ENV{R_HOME}/include)
INCLUDE_DIRECTORIES($ENV{MAGICK_DIR}/include)
# Basic units
SET(SRC conversions.cpp io.cpp display.cpp)
# Magick++ 2D processing routines
SET(SRC ${SRC} stdFilters.cpp distmaps.cpp)
# Subscript operators
#SET(SRC ${SRC} subscripts.cpp)
# Main library file
SET(SRC ${SRC} libmain.cpp)

LINK_DIRECTORIES($ENV{R_HOME}/lib)
LINK_DIRECTORIES($ENV{MAGICK_DIR}/lib)

SET(LIBS Magick++ pthread R stdc++)

ADD_LIBRARY(ebimage SHARED ${SRC})
TARGET_LINK_LIBRARIES(ebimage ${LIBS})

