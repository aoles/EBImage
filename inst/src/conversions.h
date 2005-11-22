/* ============================================================================
 * Image conversion routines: R <--> ImageMagick
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2005
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

SEXP         stack2SEXP(MagickStack& stack, bool rgb);
MagickStack  SEXP2Stack(SEXP rimage);
MagickImage  SEXP2Image(SEXP rimage);

extern "C" {
    SEXP toGray(SEXP rgb);
    SEXP toRGB(SEXP gray);
    SEXP getRed(SEXP rgb);
    SEXP getGreen(SEXP rgb);
    SEXP getBlue(SEXP rgb);
    SEXP asRed(SEXP gray);
    SEXP asGreen(SEXP gray);
    SEXP asBlue(SEXP gray);
};
