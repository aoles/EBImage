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
MagickImage  pullImageData(SEXP rimage, int index);
void         pushImageData(MagickImage& image, SEXP rimage, int index);
