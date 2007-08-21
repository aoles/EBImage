AC_INIT([EBImage], 2.2.0)

AC_CHECK_PROG(magickfound, [Magick-config --cppflags], yes, no)
if test "$magickfound" = "yes"; then
    echo "* found ImageMagick version `Magick-config --version`"
    ## check the version
    Magick-config --version | head -n 1 | awk '{
            x[1] = substr($1,1,1);
            x[2] = substr($1,3,1);
            x[3] = substr($1,5,1);
            if ( x[1] < 6 ) exit(1);
            if ( x[1] > 6 ) exit(0);
            if ( x[2] < 2 ) exit(2);
            if ( x[2] > 2 ) exit(0);
            if ( x[3] < 4 ) exit(3);
            exit(0);
          }'
    ext=$?
    if [[ $ext -ge 1 ]] ; then
      AC_MSG_ERROR("Required ImageMagick version >= 6.2.4")
    fi
    echo "* found ImageMagick includes in `Magick-config --cppflags`"
    COMPILERFLAGS="`Magick-config --cflags`"
    COMPILERFLAGS="${COMPILERFLAGS} `Magick-config --cppflags`"
    CFLAGS="-Wall ${COMPILERFLAGS} ${CFLAGS}"
    CXXFLAGS="-Wall ${COMPILERFLAGS} ${CXXFLAGS}"
    LINKERFLAGS="`Magick-config --ldflags`"
    LINKERFLAGS="${LINKERFLAGS} `Magick-config --libs`"
    LINKERFLAGS="${LINKERFLAGS} -lWand"
    LIBS="${LINKERFLAGS} ${LIBS} ${LFLAGS}"
else
    AC_MSG_ERROR("ImageMagick or ImageMagick includes not found!")
fi

PKGCONF=pkg-config
AC_CHECK_PROG(pkgfound, [pkg-config --version], yes, no)
if test "$pkgfound" = "no"; then
  PKGCONF=pkgconfig
  AC_CHECK_PROG(pkgfound, [pkgconfig --version], yes, no)
fi

if test "$pkgfound" = "yes"; then
    if `$PKGCONF --cflags gtk+-2.0 > /dev/null` ; then
        echo "* compiling with GTK+ support"
        COMPILERFLAGS="`$PKGCONF --cflags gtk+-2.0`"
        CFLAGS="-DUSE_GTK -DGLIB_GETTEXT ${COMPILERFLAGS} ${CFLAGS}"
        ## DO NOT ADD GTK keys to c++, that code does not use it
        ## CXXFLAGS="-DUSE_GTK -DGLIB_GETTEXT ${COMPILERFLAGS} ${CXXFLAGS}"
        LINKERFLAGS="`$PKGCONF --libs gtk+-2.0`"
        LIBS="${LINKERFLAGS} ${LIBS} ${LFLAGS}"
        LFLAGS="${LIBS}"
    else
        echo "* GTK+2.0 not found: compiling without GTK+ support"
    fi
else
    echo "* pkg-config/pkgconfig not found: compiling without GTK+ support"
fi

if test -z "${R_HOME}"; then
    AC_MSG_ERROR("Cannot determine R_HOME...")
fi

echo -----------------------------------------------------

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT(src/Makevars)
