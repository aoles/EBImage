/* -------------------------------------------------------------------------
Package initialization
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */
#include "EBImage.h"
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

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Error.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef CallEntries[] = {
    CALLDEF(erode_dilate, 3),
    CALLDEF(opening_closing, 3),
    CALLDEF(tophat, 3),
    CALLDEF(thresh, 2),
    CALLDEF(floodFill, 4),
    CALLDEF(fillHull, 1),
    CALLDEF(bwlabel, 1),
    CALLDEF(normalize, 4),
    CALLDEF(distmap, 2),
    CALLDEF(watershed, 3),
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
    CALLDEF(affine, 5),
    CALLDEF(medianFilter, 3),
    CALLDEF(nativeRaster, 1),
    CALLDEF(getFrame, 3),
    CALLDEF(getFrames, 3),
    /* add above all R-lib functions from common.h */
    {NULL, NULL, 0}
};

void R_init_EBImage (DllInfo *dll) {
    R_registerRoutines (dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols (dll, FALSE);
    
    Image_colormode = install("colormode");
    Image_Data      = install(".Data");
}
