AC_INIT([EBImage], 2.1.0)

AC_CHECK_PROG(magickfound, [Magick-config --version], yes, no)
if test "$magickfound" = "yes"; then
    echo ImageMagick version `Magick-config --version` found
    AC_CHECK_PROG(magickincludes, [Magick-config --cppflags], yes, no)
    if test "$magickincludes" = "yes"; then
        echo ImageMagick includes located in `Magick-config --cppflags`
        COMPILERFLAGS="`Magick-config --cflags`"
        COMPILERFLAGS="${COMPILERFLAGS} `Magick-config --cppflags`"
        CFLAGS="-Wall ${COMPILERFLAGS} ${CFLAGS}"
        CXXFLAGS="-Wall ${COMPILERFLAGS} ${CXXFLAGS}"
        LINKERFLAGS="`Magick-config --ldflags`"
        LINKERFLAGS="${LINKERFLAGS} `Magick-config --libs`"
        LIBS="${LINKERFLAGS} ${LIBS} ${LFLAGS}"
    else
        AC_MSG_ERROR("ImageMagick includes NOT FOUND. Can not proceed!")
    fi
else
    AC_MSG_ERROR("ImageMagick NOT FOUND. Can not proceed!")
fi

AC_CHECK_PROG(pkgfound, [pkg-config --version], yes, no)
if test "$pkgfound" = "yes"; then
    if `pkg-config --cflags gtk+-2.0 > /dev/null` ; then
        echo Compiling with GTK+ support
        COMPILERFLAGS="`pkg-config --cflags gtk+-2.0`"
        CFLAGS="-DUSE_GTK -DGLIB_GETTEXT ${COMPILERFLAGS} ${CFLAGS}"
        ## DO NOT ADD GTK keys to g++, that code does not use it
        ## CXXFLAGS="-DUSE_GTK -DGLIB_GETTEXT ${COMPILERFLAGS} ${CXXFLAGS}"
        LINKERFLAGS="`pkg-config --libs gtk+-2.0`"
        LIBS="${LINKERFLAGS} ${LIBS} ${LFLAGS}"
        LFLAGS="${LIBS}"
    else
    echo 'GTK+2.0' NOT FOUND: compiling without GTK+
    fi
else
    echo 'PKG-CONFIG' NOT FOUND: compiling without GTK+
fi

if test -z "${R_HOME}"; then
    AC_MSG_ERROR("Cannot determine R_HOME...")
fi

echo -----------------------------------------------------

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT(src/Makevars)
