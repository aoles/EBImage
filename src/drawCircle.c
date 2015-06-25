#include "drawCircle.h"
#include "tools.h"
#include <R_ext/Error.h>

#define SET_PIXEL(a, width, height, x, y, color) if (((x)>=0) & ((x)<(width)) & ((y)>=0) & ((y)<(height))) a[(x) + (y)*(width)] = (color);
void rasterCircle(double *a, int width, int height, int x0, int y0, int radius, double color, int fill) {
  int f = 1 - radius;
  int ddF_x = 1;
  int ddF_y = -2 * radius;
  int x = 0;
  int y = radius;
  int i;
  
  if (fill) {
    for (i=x0-radius; i<=x0+radius ;i++) SET_PIXEL(a, width, height, i, y0, color);
    for (i=y0-radius; i<=y0+radius ;i++) SET_PIXEL(a, width, height, x0, i, color);
  } else {
    SET_PIXEL(a, width, height, x0, y0 + radius, color);
    SET_PIXEL(a, width, height, x0, y0 - radius, color);
    SET_PIXEL(a, width, height, x0 + radius, y0, color);
    SET_PIXEL(a, width, height, x0 - radius, y0, color);
  }
  
  while(x < y) {
    if(f >= 0) {
      y--;
      ddF_y += 2;
      f += ddF_y;
    }
    x++;
    ddF_x += 2;
    f += ddF_x;
    if (fill) {
      for (i=x0-x; i<=x0+x ;i++) SET_PIXEL(a, width, height, i, y0+y, color);
      for (i=x0-x; i<=x0+x ;i++) SET_PIXEL(a, width, height, i, y0-y, color);
      for (i=x0-y; i<=x0+y ;i++) SET_PIXEL(a, width, height, i, y0+x, color);
      for (i=x0-y; i<=x0+y ;i++) SET_PIXEL(a, width, height, i, y0-x, color);
    }
    else {
      SET_PIXEL(a, width, height, x0 + x, y0 + y, color);
      SET_PIXEL(a, width, height, x0 - x, y0 + y, color);
      SET_PIXEL(a, width, height, x0 + x, y0 - y, color);
      SET_PIXEL(a, width, height, x0 - x, y0 - y, color);
      SET_PIXEL(a, width, height, x0 + y, y0 + x, color);
      SET_PIXEL(a, width, height, x0 - y, y0 + x, color);
      SET_PIXEL(a, width, height, x0 + y, y0 - x, color);
      SET_PIXEL(a, width, height, x0 - y, y0 - x, color);
    }
  }
}

// draw a circle on the 2D image _a using (x, y, z, radius) and color (red, green, blue)
// if colormode = Grayscale, only the first channel is used
SEXP drawCircle(SEXP _a, SEXP _xyzr, SEXP _rgb, SEXP _fill) {
  SEXP _res;
  int nprotect = 0;
  int width, height;
  int x, y, z, radius;
  int redstride, greenstride, bluestride;
  double *res;
  int fill;

  // check image validity and copy _a
  validImage(_a, 0);
  PROTECT(_res=Rf_duplicate(_a));
  nprotect++;

  width = INTEGER(GET_DIM(_res))[0];
  height = INTEGER(GET_DIM(_res))[1];

  // get strides
  x = INTEGER(_xyzr)[0];
  y = INTEGER(_xyzr)[1];
  z = INTEGER(_xyzr)[2];
  radius = INTEGER(_xyzr)[3];
  fill = INTEGER(_fill)[0];
  getColorStrides(_res, z, &redstride, &greenstride, &bluestride);
  res = REAL(_res);

  // draw circle
  if (redstride!=-1) rasterCircle(&res[redstride], width, height, x, y, radius, REAL(_rgb)[0], fill);
  if (greenstride!=-1) rasterCircle(&res[greenstride], width, height, x, y, radius, REAL(_rgb)[1], fill);
  if (bluestride!=-1)  rasterCircle(&res[bluestride], width, height, x, y, radius, REAL(_rgb)[2], fill);
  
  UNPROTECT (nprotect);
  return _res;
}
