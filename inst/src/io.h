/* ============================================================================
 * Image IO routines
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2005
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

extern "C" {
    SEXP      readImages(SEXP files, SEXP rgb);
    SEXP      writeImages(SEXP rimage, SEXP files);
    SEXP      pingImages(SEXP files, SEXP showComments);
};

