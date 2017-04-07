#include <R.h>
#include <Rdefines.h>
#include "spatial.h"
#include "tools.h"

#define PEEKPIXEL(x, y, w, h, a, bg) ((x)<0 || (x)>=w || (y)<0 || (y)>=h) ? (bg) : a[INDEX_FROM_XY(x, y, w)]

SEXP affine(SEXP _a, SEXP _d, SEXP _b, SEXP _m, SEXP _filter, SEXP _antialias) {
  SEXP res;
  int width, height, nz, framesize;
  int owidth, oheight;
  int filter, antialias;
  double *a, *b, *m, *tgt;
  
  // check image validity
  validImage(_a, 0); 
  
  // initialize width, height, nz
  width = INTEGER(GET_DIM(_a))[0];
  height = INTEGER(GET_DIM(_a))[1];
  framesize = width * height;
  nz = getNumberOfFrames(_a, 0);
  
  const int minx = -1, miny = -1, maxx = width-1, maxy = height-1;
  
  // initialize a, m, filter
  a = REAL(_a);
  b = REAL(_b);
  m = REAL(_m);
  
  filter = INTEGER(_filter)[0];
  antialias = LOGICAL(_antialias)[0];
  
  res = PROTECT( allocArray(REALSXP, _d) );
  DUPLICATE_ATTRIB(res, _a);
  SET_DIM(res, _d); //restore proper dimensions overwritten in the previous line
  
  owidth = INTEGER(_d)[0];
  oheight = INTEGER(_d)[1];
  tgt = REAL(res);
  
  // apply transform
  for (int z=0, i=0; z<nz; z++, a += framesize) {
    double bg = b[z];
    for (int y=0; y<oheight; y++) { 
      for (int x=0; x<owidth; x++) {
        double val;
        double tx = m[0]*(x+.5) + m[1]*(y+.5) + m[2];
        double ty = m[3]*(x+.5) + m[4]*(y+.5) + m[5];
        
        // bilinear filter?
        if (filter==1) {
          tx -= .5;
          ty -= .5;
          int ftx = floor(tx);
          int fty = floor(ty);
          
          if ( ftx>=minx && fty>=miny && ftx<=maxx && fty<=maxy) {
            double dx = tx-ftx;
            double dy = ty-fty;
            
            if (antialias==1 || (ftx>minx && fty>miny && ftx<maxx && fty<maxy)) {
              double pa = PEEKPIXEL(ftx, fty, width, height, a, bg);
              double pb = PEEKPIXEL(ftx+1, fty, width, height, a, bg);
              double pc = PEEKPIXEL(ftx, fty+1, width, height, a, bg);
              double pd = PEEKPIXEL(ftx+1, fty+1, width, height, a, bg);
              val = (1-dy)*(pa*(1-dx) + pb*dx) + dy*(pc*(1-dx) + pd*dx);
            }
            else {
              if (ftx==minx) {
                // upper left corner
                if (fty==miny) {
                  val = a[INDEX_FROM_XY(ftx+1, fty+1, width)];
                }
                // lower left corner
                else if (fty==maxy) {
                  val = a[INDEX_FROM_XY(ftx+1, fty, width)];
                }
                // left border
                else {
                  val = (1-dy) * a[INDEX_FROM_XY(ftx+1, fty, width)] + dy * a[INDEX_FROM_XY(ftx+1, fty+1, width)];
                }
              }
              else if (ftx==maxx) {
                // upper right corner
                if (fty==miny) {
                  val = a[INDEX_FROM_XY(ftx, fty+1, width)];
                }
                // lower right corner
                else if (fty==maxy) {
                  val = a[INDEX_FROM_XY(ftx, fty, width)];
                }
                // right border
                else {
                  val = (1-dy) * a[INDEX_FROM_XY(ftx, fty, width)] + dy * a[INDEX_FROM_XY(ftx, fty+1, width)];
                }
              } else {
                // top border
                if (fty==miny) {
                  val = (1-dx) * a[INDEX_FROM_XY(ftx, fty+1, width)] + dx * a[INDEX_FROM_XY(ftx+1, fty+1, width)];
                }
                // bootom border
                else {
                  val = (1-dx) * a[INDEX_FROM_XY(ftx, fty, width)] + dx * a[INDEX_FROM_XY(ftx+1, fty, width)];
                }
              }
            }
          }
          else val = bg;
        }
        else {
          int ftx = floor(tx);
          int fty = floor(ty);
          val = PEEKPIXEL(ftx, fty, width, height, a, bg);
        }
        tgt[i++] = val;
      }
    }
  }
  
  UNPROTECT(1);
  return res;
}
