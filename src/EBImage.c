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

#include "distmap.h"
#include "filters_magick.h"
#include "morphology.h"
#include "translate.h"
#include "filters_propagate.h"
#include "filters_normalize.h"
#include "filters_watershed.h"
#include "thresh.h"
#include "floodFill.h"

#include "features_hull.h"
#include "features_moments.h"
#include "features_haralick.h"
#include "features_zernike.h"

#include "drawable.h"

#include "objects.h"
#include "frameDist.h"

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>

#include <magick/ImageMagick.h>
#include <wand/magick-wand.h>

/* nan GTK+ includes */
#ifdef USE_GTK
	#include <gtk/gtk.h>
	// GTK/Windows interface copied from the RGtk2 pacakge by Michael Lawrence (src/Rgtk.c)
	void R_gtk_eventHandler(void *userData) {
		while(gtk_events_pending()) gtk_main_iteration();
	}
	#ifdef WIN32
        	typedef unsigned long ulong;
		#ifdef _RTCLDO_METHOD
        		extern  __declspec(dllimport) void (* R_tcldo) ();
			void R_gtk_handle_events() {
				R_gtk_eventHandler(NULL);
			}
		#else
			#include <windows.h>
			#define HWND_MESSAGE	((HWND)-3)
			#define RGTK2_TIMER_ID	0
			#define RGTK2_TIMER_DELAY 50
			VOID CALLBACK R_gtk_timer_proc(HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime) {
				R_gtk_eventHandler(NULL);
			}
		#endif // R < 2.8.0
		#include <sys/types.h>
	#else
		#include "R_ext/eventloop.h"
		#include <gdk/gdkx.h>
	#endif // Windows
#endif // GTK

/*----------------------------------------------------------------------- */
static R_CallMethodDef libraryRCalls[] = {
    {"lib_readImages",     (DL_FUNC) &lib_readImages,     2},
    {"lib_chooseImages",   (DL_FUNC) &lib_chooseImages,   1},
    {"lib_writeImages",    (DL_FUNC) &lib_writeImages,    3},
    {"lib_display",        (DL_FUNC) &lib_display,        3},
    {"lib_animate",        (DL_FUNC) &lib_animate,        1},
    {"lib_channel",        (DL_FUNC) &lib_channel,        2},

    {"lib_filterMagick",   (DL_FUNC) &lib_filterMagick,   3},
    {"translate",          (DL_FUNC) &translate,          2},
    {"lib_erode_dilate",   (DL_FUNC) &lib_erode_dilate,   4},
    {"thresh",   (DL_FUNC) &thresh,   2},
    {"floodFill",      (DL_FUNC) &floodFill,      4},
    {"fillHull",       (DL_FUNC) &fillHull,       1},
    {"lib_normalize",      (DL_FUNC) &lib_normalize,      3},
    {"distmap",            (DL_FUNC) &distmap,            2},
    {"lib_filterInvWS",    (DL_FUNC) &lib_filterInvWS,    3},
    {"lib_propagate",      (DL_FUNC) &lib_propagate,      5},

    {"paintObjects",  (DL_FUNC) &paintObjects,  4},
    {"matchObjects",  (DL_FUNC) &matchObjects,  2},
    {"rmObjects", (DL_FUNC) &rmObjects, 2},
    {"tile",     (DL_FUNC) &tile,     3},
    {"untile",         (DL_FUNC) &untile,         4},
    {"stackObjects",  (DL_FUNC) &stackObjects,  6},
    {"lib_frameDist",      (DL_FUNC) &lib_frameDist,      5},

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
#ifndef WIN32
        addInputHandler(R_InputHandlers, ConnectionNumber(GDK_DISPLAY()), R_gtk_eventHandler, -1);
#else
	#ifdef _RTCLDO_METHOD
        	R_tcldo = R_gtk_handle_events;
	#else
		LPCTSTR class="EBImage";
		HINSTANCE instance = GetModuleHandle(NULL);
		WNDCLASS wndClass = {0, DefWindowProc, 0, 0, instance, NULL, 0, 0, NULL, class};
		RegisterClass(&wndClass);
		HWND win = CreateWindow(class, NULL, 0, 1, 1, 1, 1, HWND_MESSAGE, NULL, instance, NULL);
		SetTimer(win, RGTK2_TIMER_ID, RGTK2_TIMER_DELAY, (TIMERPROC)R_gtk_timer_proc);
	#endif // R < 2.8.0
#endif // Win32
    }
#endif // GTK
    R_registerRoutines (winDll, NULL, libraryRCalls, NULL, NULL);
    R_useDynamicSymbols (winDll, FALSE);
#   ifdef WIN32
    InitializeMagick ("");
    if ( !IsMagickInstantiated () )
        error ( "cannot initialize ImageMagick" );
#   endif
    /* MagickWand must be started */
    MagickWandGenesis();
}

void
R_unload_EBImage (DllInfo * winDll) {
  /* MagickWand must be terminated at the end of the process since MagickWandTerminus() closes all the Wand AND ALSO the MagickCore services ! */
  MagickWandTerminus();
#   ifdef WIN32
    if ( IsMagickInstantiated() )
        DestroyMagick();
#   endif
}

