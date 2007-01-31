/* -------------------------------------------------------------------------
Package initialization
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"
#include <R_ext/Rdynload.h>

/*----------------------------------------------------------------------- */
static R_CallMethodDef libraryRCalls[] = {
    {"lib_",               (DL_FUNC) &lib_,               1},
    {"lib_readImages",     (DL_FUNC) &lib_readImages,     2},
    {"lib_chooseImages",   (DL_FUNC) &lib_chooseImages,   0},
    {"lib_writeImages",    (DL_FUNC) &lib_writeImages,    3},
    {"lib_display",        (DL_FUNC) &lib_display,        2},
    {"lib_animate",        (DL_FUNC) &lib_animate,        1},
    {"lib_channel",        (DL_FUNC) &lib_channel,        2},
    {"lib_filterMagick",   (DL_FUNC) &lib_filterMagick,   3},
    {"lib_erode_dilate",   (DL_FUNC) &lib_erode_dilate,   4},
    {"lib_filterThresh",   (DL_FUNC) &lib_filterThresh,   2},
    {"lib_filterFill",     (DL_FUNC) &lib_filterFill,     5},
    {"lib_distMap",        (DL_FUNC) &lib_distMap,        4},
    {"lib_filterInvWS",    (DL_FUNC) &lib_filterInvWS,    4},
    {"lib_getFeatures",    (DL_FUNC) &lib_getFeatures,    2},
    {"lib_paintFeatures",  (DL_FUNC) &lib_paintFeatures,  4},
    {"lib_combineFeatures",(DL_FUNC) &lib_combineFeatures,4},
    {"lib_matchFeatures",  (DL_FUNC) &lib_matchFeatures,  2},
    {"lib_deleteFeatures", (DL_FUNC) &lib_deleteFeatures, 3},

    /* add above all R-lib functions from common.h */
    {NULL, NULL, 0}
};

/*----------------------------------------------------------------------- */
void
/*
#ifdef HAVE_VISIBILITY_ATTRIBUTE
    __attribute__ ((visibility ("default")))
#endif
*/
R_init_EBImage (DllInfo * winDll) {
#ifdef USE_GTK
    /* argc, agrv global vars defined in common.h */
/*
    argc = 1;
    argv = (char **) Calloc (1, char* );
    argv[0] = Calloc (255, char );
    strcpy (argv[0], "R session");
*/
    argc = 0;
    argv = NULL;
    GTK_OK = 0;
    // initialize gtk, vars defined in common.h and initialised in init.c
    if ( !gtk_init_check(&argc, &argv) )
        warning ( _("failed to initialize GTK+") );
    else {
        GTK_OK = 1;
        // add R event handler to enable automatic window redraw
#       ifndef WIN32
        hdlr = addInputHandler(R_InputHandlers, ConnectionNumber(GDK_DISPLAY()), _doIter, -1);
#       else
        R_tcldo = _doIterWin32;
#       endif
    }
#endif
    R_registerRoutines (winDll, NULL, libraryRCalls, NULL, NULL);
    R_useDynamicSymbols (winDll, FALSE);
    /* this initialization is not required on Linux */
    /* in fact I am not sure this is required on Windows! */
#   ifdef WIN32
    InitializeMagick ("");
    if ( !IsMagickInstantiated () )
        error ( _("cannot initialize ImageMagick") );
#   endif
}

void
/*
#ifdef HAVE_VISIBILITY_ATTRIBUTE
    __attribute__ ((visibility ("default")))
#endif
*/
R_unload_EBImage (DllInfo * winDll) {
    /* this destroy is not required on Linux */
    /* in fact I am not sure this is required on Windows! */
#   ifdef WIN32
    if ( IsMagickInstantiated() )
        DestroyMagick();
#   endif
/*
    Free (argv[0]);
    Free (argv);
*/
}

