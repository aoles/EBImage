/* ============================================================================
 * 2D Image Filters: EBImage specific
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2005
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

extern "C" {
    SEXP adaptiveThreshold(SEXP rimage, SEXP param);
    SEXP distMap(SEXP rimage, SEXP alg);
};

