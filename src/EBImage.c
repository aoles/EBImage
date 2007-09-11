/* -------------------------------------------------------------------------
Package initialization
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"
#include "colors.h"
#include "display.h"
#include "io.h"

#include "filters_distmap.h"
#include "filters_magick.h"
#include "filters_morph.h"
#include "filters_propagate.h"
#include "filters_normalize.h"
#include "filters_watershed.h"
#include "filters_thresh.h"

#include "features_hull.h"
#include "features_moments.h"
#include "features_haralick.h"
#include "features_zernike.h"

#include "drawable.h"

#include "objects.h"

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>

#include <magick/ImageMagick.h>

/* GTK+ includes */
#ifdef USE_GTK
#   include <gtk/gtk.h>
#   ifdef WIN32
        typedef unsigned long ulong;
        extern  __declspec(dllimport) void (* R_tcldo) ();
#       include <sys/types.h>
#   else
#       include "R_ext/eventloop.h"
#       include <gdk/gdkx.h>
#   endif
#endif

/*----------------------------------------------------------------------- */
static R_CallMethodDef libraryRCalls[] = {
    {"lib_readImages",     (DL_FUNC) &lib_readImages,     2},
    {"lib_chooseImages",   (DL_FUNC) &lib_chooseImages,   1},
    {"lib_writeImages",    (DL_FUNC) &lib_writeImages,    3},
    {"lib_display",        (DL_FUNC) &lib_display,        3},
    {"lib_animate",        (DL_FUNC) &lib_animate,        1},
    {"lib_channel",        (DL_FUNC) &lib_channel,        2},

    {"lib_filterMagick",   (DL_FUNC) &lib_filterMagick,   3},
    {"lib_erode_dilate",   (DL_FUNC) &lib_erode_dilate,   4},
    {"lib_filterThresh",   (DL_FUNC) &lib_filterThresh,   2},
    {"lib_filterFill",     (DL_FUNC) &lib_filterFill,     5},
    {"lib_normalize",      (DL_FUNC) &lib_normalize,      3},
    {"lib_distMap",        (DL_FUNC) &lib_distMap,        4},
    {"lib_filterInvWS",    (DL_FUNC) &lib_filterInvWS,    3},
    {"lib_propagate",      (DL_FUNC) &lib_propagate,      5},

    {"lib_paintFeatures",  (DL_FUNC) &lib_paintFeatures,  4},
    {"lib_matchFeatures",  (DL_FUNC) &lib_matchFeatures,  2},
    {"lib_deleteFeatures", (DL_FUNC) &lib_deleteFeatures, 2},
    {"lib_stack_objects",  (DL_FUNC) &lib_stack_objects,  6},
    {"lib_tile_stack",     (DL_FUNC) &lib_tile_stack,     3},

    {"lib_drawText",       (DL_FUNC) &lib_drawText,       5},

    {"lib_basic_hull",     (DL_FUNC) &lib_basic_hull,     1},
    {"lib_cmoments",       (DL_FUNC) &lib_cmoments,       2},
    {"lib_moments",        (DL_FUNC) &lib_moments,        4},
    {"lib_edge_profile",   (DL_FUNC) &lib_edge_profile,   2},
    {"lib_co_occurrence",  (DL_FUNC) &lib_co_occurrence,  3},
    {"lib_haralick",       (DL_FUNC) &lib_haralick,       1},
    {"lib_zernike",        (DL_FUNC) &lib_zernike,        6},
    {"lib_pseudo_zernike", (DL_FUNC) &lib_pseudo_zernike, 6},

    /* add above all R-lib functions from common.h */
    {NULL, NULL, 0}
};

#ifdef USE_GTK
void _doIter (void * userData) {
    while ( gtk_events_pending() ) gtk_main_iteration();
}
#    ifdef WIN32
void _doIterWin32 () {
    _doIter (NULL);
}
#    endif
#endif

char ** argv;
int argc;

/*----------------------------------------------------------------------- */
void
R_init_EBImage (DllInfo * winDll) {
#ifdef USE_GTK
    /* argc, agrv global vars defined in common.h */
    argc = 0;
    argv = NULL;
    GTK_OK = 0;
    // initialize gtk, vars defined in common.h and initialised in init.c
    gtk_disable_setlocale();
    if ( !gtk_init_check(&argc, &argv) )
        warning ( "failed to initialize GTK+. GTK+ dependent functions will not work" );
    else {
        GTK_OK = 1;
        // add R event handler to enable automatic window redraw
#       ifndef WIN32
        addInputHandler(R_InputHandlers, ConnectionNumber(GDK_DISPLAY()), _doIter, -1);
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
        error ( "cannot initialize ImageMagick" );
#   endif
}

void
R_unload_EBImage (DllInfo * winDll) {
    /* this destroy is not required on Linux */
    /* in fact I am not sure this is required on Windows! */
#   ifdef WIN32
    if ( IsMagickInstantiated() )
        DestroyMagick();
#   endif
}

