#ifndef EBIMAGE_CONV_H
#define EBIMAGE_CONV_H

#include <R.h>
#include <Rdefines.h>
#include <magick/ImageMagick.h>

#ifdef __cplusplus
extern "C" {
#endif

Image * sexp2Magick (SEXP);
SEXP magick2SEXP (Image *, int);

#ifdef __cplusplus
};
#endif

#endif
