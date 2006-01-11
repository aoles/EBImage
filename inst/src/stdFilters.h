/* ============================================================================
 * 2D Image Processing routines based on Magick++ functions
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2005
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

extern "C" {
    SEXP stdFilter2D(SEXP rimage, SEXP filterNo, SEXP param);
    SEXP stdFilter2DRedim(SEXP rimage, SEXP filterNo, SEXP param);
    SEXP normalizeImages(SEXP rimage, SEXP range, SEXP independent);
};

