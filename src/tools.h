#ifndef EBIMAGE_TOOLS_H
#define EBIMAGE_TOOLS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MODE_GRAYSCALE  0
#define MODE_COLOR      2

#define COLOR_MODE(x) ( R_has_slot(x, mkString("colormode")) ? INTEGER(GET_SLOT(x, mkString("colormode")))[0] : MODE_GRAYSCALE )

#define INDEX_FROM_XY(x, y, xsize) ((x) + (y) * (xsize))
#define POINT_FROM_INDEX(pt, index, xsize) pt.x = index % xsize; pt.y = index / xsize;

#define DISTANCE_XY(pt1, pt2) sqrt( (long double) ( (pt1.x - pt2.x) * (pt1.x - pt2.x) + (pt1.y - pt2.y) * (pt1.y - pt2.y) ) )

typedef struct {
    int x, y;
} PointXY;
  
typedef double numeric;

int validImage (SEXP x,int test);
int getNumberOfFrames (SEXP x, int type);
int getNumberOfChannels (SEXP x);
void getColorStrides(SEXP x,int index,int *redstride,int *greenstride,int *bluestride);

int isImage (SEXP x);

#ifdef __cplusplus
}
#endif

#endif
