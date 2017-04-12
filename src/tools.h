#ifndef EBIMAGE_TOOLS_H
#define EBIMAGE_TOOLS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MODE_GRAYSCALE  0
#define MODE_COLOR      2

#define COLOR_MODE(x) ( R_has_slot(x, Image_colormode) ? INTEGER(GET_SLOT(x, Image_colormode))[0] : MODE_GRAYSCALE )

#define INDEX_FROM_XY(x, y, xsize) ((x) + (y) * (xsize))
#define POINT_FROM_INDEX(pt, index, xsize) pt.x = index % xsize; pt.y = index / xsize;

#define DISTANCE_XY(pt1, pt2) sqrt( (long double) ( (pt1.x - pt2.x) * (pt1.x - pt2.x) + (pt1.y - pt2.y) * (pt1.y - pt2.y) ) )

extern SEXP Image_colormode;

typedef struct {
    int x, y;
} PointXY;

typedef struct {
  int r, g, b;
} ColorStrides;

typedef double numeric;

SEXP numberOfFrames (SEXP, SEXP);
SEXP validImageObject (SEXP x);
int validImage (SEXP x,int test);
int getNumberOfFrames (SEXP x, int type);
int getNumberOfChannels (SEXP x, int colormode);
void getColorStrides(SEXP x, int index, ColorStrides *strides);

int isImage (SEXP x);

#ifdef __cplusplus
}
#endif

#endif
