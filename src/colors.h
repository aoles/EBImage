/* ============================================================================
 * Color conversions
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright: Oleg Sklyar, 2006
 *            European Bioinformatics Institute; Bioconductor.org
 * ============================================================================
 */

#include "common.h"

extern "C" {
    SEXP toGray(SEXP rgb);
    SEXP toRGB(SEXP gray);
    SEXP getRed(SEXP rgb);
    SEXP getGreen(SEXP rgb);
    SEXP getBlue(SEXP rgb);
    SEXP asRed(SEXP gray);
    SEXP asGreen(SEXP gray);
    SEXP asBlue(SEXP gray);

    SEXP toColorString(SEXP rgb);
    SEXP fromColorString(SEXP str); /* RGB vector */
    
    SEXP intToColorString(SEXP x);
    SEXP colorStringToInt(SEXP x);
    
    
    
    
    SEXP any2rgb     (SEXP x);
    SEXP any2gray    (SEXP x);
    SEXP any2X11char (SEXP x);
    SEXP add2rgb     (SEXP x, SEXP y);
    SEXP sub2rgb     (SEXP x, SEXP y);
    SEXP scale2rgb   (SEXP x, SEXP factor);

};
