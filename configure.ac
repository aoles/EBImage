AC_INIT([EBImage], 1.5.005)

AC_CHECK_PROG(magickfound, [Magick++-config --version], yes, no)
if test "$magickfound" = "yes"; then
    echo ImageMagick version `Magick++-config --version` found
    AC_CHECK_PROG(magickincludes, [Magick++-config --cppflags], yes, no)
    if test "$magickincludes" = "yes"; then
        echo ImageMagick++ includes located in `Magick++-config --cppflags`
        COMPILERFLAGS="`Magick++-config --cppflags`"
        COMPILERFLAGS="${COMPILERFLAGS} `Magick++-config --cxxflags`"
        CXXFLAGS="${COMPILERFLAGS} ${CXXFLAGS}"
        LIBS="`Magick++-config --ldflags`"
        LIBS="${LIBS} `Magick++-config --libs`"
        LIBS="${LIBS} ${LFLAGS}"
        LFLAGS="${LIBS}"
    else
        AC_MSG_ERROR("Cannot find ImageMagick++-devel (Magick++-devel)")
    fi
else
    AC_MSG_ERROR("Cannot find ImageMagick (Magick++)")
fi

AC_CHECK_PROG(pkgfound, [pkg-config --cflags gtk+-2.0], yes, no)
if test "$pkgfound" = "yes"; then
    echo Compiling with GTK+ support
    COMPILERFLAGS="`pkg-config --cflags gtk+-2.0`"
    CXXFLAGS="-DUSEGTK ${COMPILERFLAGS} ${CXXFLAGS}"
    LIBS="`pkg-config --libs gtk+-2.0`"
    LIBS="${LIBS} ${LFLAGS}"
    LFLAGS="${LIBS}"
else
    echo Compiling without GTK+ support
fi

if test -z "${R_HOME}"; then
    AC_MSG_ERROR("Cannot determine R_HOME...")
fi

#RCONF="${R_HOME}/bin/R CMD config"
#echo R found in: ${R_HOME}
#CXX="`${RCONF} CXX`"

AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)
AC_SUBST(LFLAGS)

AC_OUTPUT(src/Makevars)
