#ifndef EBIMAGE_COLORS_H
#define EBIMAGE_COLORS_H

#include <R.h>
#include <Rdefines.h>
#include <magick/ImageMagick.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_channel (SEXP, SEXP);
Image * vector2image1D (SEXP);
Image * int2image1D (int *, int);
Image * double2image1D (double *, int);
SEXP image1D2REAL (Image *, int);
void image1D2double (Image *, double *, int);
SEXP image1D2INTEGER (Image *, int);
void image1D2int (Image *, int *, int);
SEXP image1D2CHAR (Image *);


#ifdef __cplusplus
};
#endif

#endif
