#ifndef EBIMAGE_CLAHE_H
#define EBIMAGE_CLAHE_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP clahe (SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

#ifdef BYTE_IMAGE
typedef unsigned char kz_pixel_t;	 /* for 8 bit-per-pixel images */
#define uiNR_OF_GREY (256)
#else
typedef unsigned short kz_pixel_t;	 /* for 16 bit-per-pixel images (default) */
# define uiNR_OF_GREY (65536)
#endif

/******** Prototype of CLAHE function. Put this in a separate include file. *****/
int CLAHE(kz_pixel_t* pImage, unsigned int uiXRes, unsigned int uiYRes, kz_pixel_t Min,
	  kz_pixel_t Max, unsigned int uiNrX, unsigned int uiNrY,
	  unsigned int uiNrBins, float fCliplimit);

#ifdef __cplusplus
};
#endif

#endif
