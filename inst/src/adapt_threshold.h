/* ============================================================================
 * 2D Image Adaptive Threshold Filter
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2005
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

extern "C" {
    SEXP adaptiveThreshold(SEXP rimage, SEXP param);
};

