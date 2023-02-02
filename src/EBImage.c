/* -------------------------------------------------------------------------
Package initialization
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */
#include "tools.h"
#include "distmap.h"
#include "morphology.h"
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
#include "nativeRaster.h"
#include "getFrames.h"
#include "transpose.h"
#include "clahe.h"

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

SEXP Image_colormode;

static R_CallMethodDef CallEntries[] = {
    CALLDEF(transpose, 1),
    CALLDEF(numberOfFrames, 2),
    CALLDEF(validImageObject, 1),
    CALLDEF(morphology, 3),
    CALLDEF(thresh, 2),
    CALLDEF(floodFill, 4),
    CALLDEF(fillHull, 1),
    CALLDEF(bwlabel, 1),
    CALLDEF(normalize, 4),
    CALLDEF(distmap, 2),
    CALLDEF(watershed, 4),
    CALLDEF(propagate, 4),
    CALLDEF(paintObjects, 5),
    CALLDEF(rmObjects, 3),
    CALLDEF(tile, 3),
    CALLDEF(untile, 3),
    CALLDEF(stackObjects, 5),
    CALLDEF(ocontour, 1),
    CALLDEF(haralickMatrix, 3),
    CALLDEF(haralickFeatures, 1),
    CALLDEF(drawCircle, 4),
    CALLDEF(affine, 6),
    CALLDEF(medianFilter, 3),
    CALLDEF(nativeRaster, 1),
    CALLDEF(getFrame, 3),
    CALLDEF(getFrames, 3),
    CALLDEF(clahe, 6),
    /* add above all R-lib functions from common.h */
    {NULL, NULL, 0}
};

void R_init_EBImage (DllInfo *dll) {
    R_registerRoutines (dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols (dll, FALSE);
    
    Image_colormode = install("colormode");
}
