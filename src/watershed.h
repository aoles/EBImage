/* ============================================================================
 * Image 2D processing algorithms
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2006
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

extern "C" {
    SEXP watershedDetection(SEXP rimage, SEXP ref, SEXP seeds, SEXP params);
};

