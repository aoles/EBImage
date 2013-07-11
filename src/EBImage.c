/* -------------------------------------------------------------------------
Package initialization
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"

#include "distmap.h"
#include "morphology.h"
#include "greyscale_morphology.h"
#include "spatial.h"
#include "propagate.h"
#include "normalize.h"
#include "watershed.h"
#include "thresh.h"
#include "floodFill.h"
#include "medianFilter.h"
#include "haralick.h"
#include "drawCircle.h"
#include "objects.h"
#include "ocontour.h"
#include "tile.h"

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>

// C preferred way to get rid of 'unused parameter' warnings
#define UNUSED(expr) do { (void)(expr); } while (0)

static R_CallMethodDef libraryRCalls[] = {
    {"lib_erode_dilate",                (DL_FUNC) &lib_erode_dilate,   3},
    {"lib_erode_dilate_greyscale",      (DL_FUNC) &lib_erode_dilate_greyscale,   3},
    {"lib_opening_closing_greyscale",   (DL_FUNC) &lib_opening_closing_greyscale,   3},
    {"lib_tophat_greyscale",            (DL_FUNC) &lib_tophat_greyscale,   3},
    {"thresh",                          (DL_FUNC) &thresh,   2},
    {"floodFill",                       (DL_FUNC) &floodFill,      4},
    {"fillHull",                        (DL_FUNC) &fillHull,       1},
    {"bwlabel",                         (DL_FUNC) &bwlabel,       1},
    {"normalize",                       (DL_FUNC) &normalize,      4},
    {"distmap",                         (DL_FUNC) &distmap,            2},
    {"watershed",                       (DL_FUNC) &watershed,    3},
    {"propagate",                       (DL_FUNC) &propagate,      4},
    {"paintObjects",                    (DL_FUNC) &paintObjects,  4},
    {"rmObjects",                       (DL_FUNC) &rmObjects, 2},
    {"tile",                            (DL_FUNC) &tile,     3},
    {"untile",                          (DL_FUNC) &untile,         3},
    {"stackObjects",                    (DL_FUNC) &stackObjects,  5},
    {"ocontour",                        (DL_FUNC) &ocontour,  1},
    {"haralickMatrix",                  (DL_FUNC) &haralickMatrix,  3},
    {"haralickFeatures",                (DL_FUNC) &haralickFeatures,       1},
    {"drawCircle",                      (DL_FUNC) &drawCircle, 4},
    {"affine",                          (DL_FUNC) &affine, 4},
    {"medianFilter",                    (DL_FUNC) &medianFilter, 4},
    /* add above all R-lib functions from common.h */
    {NULL, NULL, 0}
};

void R_init_EBImage (DllInfo * winDll) {
    R_registerRoutines (winDll, NULL, libraryRCalls, NULL, NULL);
    R_useDynamicSymbols (winDll, FALSE);
}

void R_unload_EBImage (DllInfo * winDll) {
  UNUSED(winDll);
}

