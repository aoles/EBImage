AC_INIT([EBImage], 1.0.1) dnl package name, version

AC_CHECK_PROG(magickfound, [Magick++-config --version], yes, no)
if test "$magickfound" = "yes"; then
    echo ImageMagick version $magickfound found
    AC_CHECK_PROG(magickincludes, [Magick++-config --cppflags], yes, no)
    if test "$magickincludes" = "yes"; then
        echo ImageMagick++ includes located in $magickincludes
        CXXFLAGS="`Magick++-config --cppflags`"
        CXXFLAGS="${CXXFLAGS} `Magick++-config --cxxflags`"
        LIBS="`Magick++-config --ldflags`"
        LIBS="${LIBS} `Magick++-config --libs`"
    else
        AC_MSG_ERROR("Cannot find ImageMagick++-devel (Magick++-devel)")
    fi
else
    AC_MSG_ERROR("Cannot find ImageMagick (Magick++)")
fi

dnl : ${R_HOME=`R RHOME`}

if test -z "${R_HOME}"; then
dnl echo "could not determine R_HOME"
dnl exit 1
    AC_MSG_ERROR("Cannot determine R_HOME...")
fi
RCONF="${R_HOME}/bin/R CMD config"
echo R found in: ${R_HOME}
CXX="`${RCONF} CXX`"

dnl CXXFLAGS="-I. -Isrc -I${R_HOME}/include ${CXXFLAGS}"
dnl CXXFLAGS="-c -g -fPIC -I. -Isrc -I${R_HOME}/include ${CXXFLAGS}"
dnl LIBS="-shared -g -fPIC -L${R_HOME}/lib -lR ${LIBS}"

LFLAGS=${LIBS}

dnl substitute CXXFLAGS and LIBS
AC_SUBST(CXXFLAGS)
AC_SUBST(LIBS)
AC_SUBST(LFLAGS)

dnl and do subsitution in the src/Makevars.in
AC_OUTPUT(src/Makevars)
