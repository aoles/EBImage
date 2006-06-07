AC_INIT([EBImage], 1.3.016)

AC_CHECK_PROG(magickfound, [Magick++-config --version], yes, no)
if test "$magickfound" = "yes"; then
    echo ImageMagick version `Magick++-config --version` found
    AC_CHECK_PROG(magickincludes, [Magick++-config --cppflags], yes, no)
    if test "$magickincludes" = "yes"; then
        echo ImageMagick++ includes located in `Magick++-config --cppflags`
        COMPILERFLAGS="`Magick++-config --cppflags`"
        COMPILERFLAGS="${CXXFLAGS} `Magick++-config --cxxflags`"
        CXXFLAGS="${COMPILERFLAGS} ${CXXFLAGS}"
        LIBS="`Magick++-config --ldflags`"
        LIBS="${LIBS} `Magick++-config --libs`"
    else
        AC_MSG_ERROR("Cannot find ImageMagick++-devel (Magick++-devel)")
    fi
else
    AC_MSG_ERROR("Cannot find ImageMagick (Magick++)")
fi

if test -z "${R_HOME}"; then
    AC_MSG_ERROR("Cannot determine R_HOME...")
fi

RCONF="${R_HOME}/bin/R CMD config"
echo R found in: ${R_HOME}
CXX="`${RCONF} CXX`"

LFLAGS=${LIBS}

AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)
AC_SUBST(LFLAGS)

AC_OUTPUT(src/Makevars)
