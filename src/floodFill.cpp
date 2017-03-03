#include "floodFill.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Flood fill for images and flood-fill-based hull filling for objects
Copyright (c) 2007 Gregoire Pau; templated code by Oleg Sklyar

See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>
#include <stack>

/* -------------------------------------------------------------------------- */
struct XYPoint {
  XYPoint() {}
  XYPoint(int xx, int yy): x(xx), y(yy) {}
  int x, y;
};

template <class T> void _floodFill(T*, XYPoint, XYPoint, T, double tol = 1e-3);
template <class T> void _fillHullT(T *, const XYPoint &);
template <class T> void _bwlabel(T *, int *, XYPoint);

/* -------------------------------------------------------------------------- */
SEXP
floodFill(SEXP x, SEXP point, SEXP col, SEXP tol) {
  int i, nz, *dim;
  int nprotect=0;
  XYPoint pt;
  SEXP res;

  // check image validity
  validImage(x,0);
  nz = getNumberOfFrames(x, 0);
  dim = INTEGER(GET_DIM(x));
  XYPoint size(dim[0], dim[1]);
  if (size.x <= 0 || size.y <= 0) error("image must have positive dimensions");
  if (LENGTH(point) != 2*nz) error("point must have a size of two times the number of frames");
  if (LENGTH(col) != nz) error("color must have the same size as the number of frames");
  
  // initialize result
  PROTECT(res = Rf_duplicate(x));
  nprotect++;
  
  // do the job over images
 for (i=0; i<nz; i++) {
    pt.x = INTEGER(point)[i]-1;
    pt.y = INTEGER(point)[nz+i]-1;

    switch (TYPEOF(res)) {
      case LGLSXP:
      case INTSXP:
        _floodFill<int>(&(INTEGER(res)[i*size.x*size.y]), size, pt, INTEGER(col)[i], REAL(tol)[0]);
        break;
      case REALSXP:
        _floodFill<double>(&(REAL(res)[i*size.x*size.y]), size, pt, REAL(col)[i], REAL(tol)[0]);
        break;
    }
  }

  UNPROTECT (nprotect);
  return res;
}

/* -------------------------------------------------------------------------- */
SEXP
fillHull(SEXP x) {
  SEXP res;
  int nprotect = 0;
  int nz;

  // check image validity
  validImage(x,0);
  nz = getNumberOfFrames(x, 0);

  int *dim=INTEGER(GET_DIM(x));
  XYPoint size(dim[0], dim[1]);

  // return itself if nothing to do
  if (size.x <= 0 || size.y <= 0 || nz < 1) return x;
 
  // do fillHull
  PROTECT(res = Rf_duplicate(x));
  nprotect++;
  if (IS_INTEGER(res)) {
    for (int i=0; i < nz; i++) _fillHullT<int>(&(INTEGER(res)[i*size.x*size.y]), size);
  }
  else if (IS_NUMERIC(res)) {
    for (int i=0; i < nz; i++) _fillHullT<double>(&(REAL(res)[i*size.x*size.y]), size);
  }
  
  UNPROTECT (nprotect);
  return res;
}

/* -------------------------------------------------------------------------- */
SEXP
bwlabel(SEXP x) {
  int i, nz, sizexy, offset, *dim, *tgt;
  int nprotect=0;
  SEXP res;

  // check image validity
  validImage(x,0);
  nz = getNumberOfFrames(x, 0);
  dim = INTEGER(GET_DIM(x));
  XYPoint size(dim[0], dim[1]);
  if (size.x <= 0 || size.y <= 0) error("image must have positive dimensions");
  
  // store results as integers
  PROTECT( res = allocVector(INTSXP, XLENGTH(x)) );
  nprotect++;
  DUPLICATE_ATTRIB(res, x);
  
  sizexy = size.x*size.y;
  offset = 0;
  
  for (i=0; i<nz; i++, offset+=sizexy) {
    tgt = &(INTEGER(res)[offset]);
  
    switch (TYPEOF(x)) {
      case LGLSXP:
      case INTSXP:
        _bwlabel<int>( &(INTEGER(x)[offset]), tgt, size);
        break;
      case REALSXP:
        _bwlabel<double>( &(REAL(x)[offset]), tgt, size);
        break;
    }
  }

  UNPROTECT (nprotect);
  return res;
}

template <class T> void _bwlabel(T *src, int *res, XYPoint size) {
  XYPoint pt;
  int pos = 0;
  int idx = 1;
  
  // assuming binary images:
  // 0 is background and everything else (background) is converted here to -1
  for (int i=0; i<size.x*size.y; i++) {
    res[i] = (src[i]==0.0) ? 0 : -1;
  }
  
  // do the job over images
  // every pixel with -1 is filled with an increasing index, starting from 1
  for (int ky=0; ky<size.y ; ky++) {
    for (int kx=0; kx<size.x ; kx++, pos++) {
      if (res[pos]==-1) {
        pt.x = kx;
        pt.y = ky;
        _floodFill<int>(res, size, pt, idx, 0);
        idx++;
      }
    }
  }
}

/* -------------------------------------------------------------------------- */
/* stack that checks the size and returns a value on pop in one line */
template <class T> class PopCheckStack {
  public:
    void push(T t) {
      vstack.push(t);
    }
    bool pop(T &t) {
      if (vstack.empty()) return false;
      t = vstack.top();
      vstack.pop();
      return true;
    }
  protected:
    std::stack<T> vstack;
};

/* stack of x-y points */
typedef PopCheckStack<XYPoint> XYStack;

/* -------------------------------------------------------------------------- */
/** 
  Floodfill 
   
  Fill the region of matrix m (of size [width,height]) with color rc, starting 
  with seed starting pixel at position x,y. Fast stacked scanline algorithm.

  gregoire.pau@ebi.ac.uk ; 10/2007
   
  Templated version by Oleg Sklyar
*/

template <class T>void 
_floodFill(T *m, XYPoint size, XYPoint xy, T rc, double tol) {
  XYStack s, offsets;
  XYPoint pt = xy;
  bool spanLeft,spanRight,offset=false;
  /* set the target color tc */
  T tc = m[pt.x+pt.y*size.x];

  /* FIXME: the offset workaround with another stack is ONLY used when
   * the reset color (rc) is the same as target color (tc). In this case
   * we reset to an offset color from rc first, keep coordinates of all
   * reset points and reset them to what we need at the end of the loop.
   * This does not affect the speed when the color is different as the 
   * stack is not used then.
   */
  T resetc = rc;
  if (fabs(tc-rc) <= tol) {
    offset=true;
    resetc = (T)(rc+tol+1);
  }
    
  // pushes the seed starting pixel
  s.push(pt);
    
  while(s.pop(pt)) {    
    // climbs up along the column x as far as possible
    while(pt.y>=0 && fabs(m[pt.x+pt.y*size.x]-tc) <= tol) pt.y--;
    pt.y++;
    spanLeft=false;
    spanRight=false;
    /* to enable users to terminate this function */
    R_CheckUserInterrupt();

    // processes the column x
    while(pt.y<size.y && fabs(m[pt.x+pt.y*size.x]-tc) <= tol) {
      m[pt.x+pt.y*size.x]=resetc;
      if (offset) offsets.push(pt);
      if(!spanLeft && pt.x>0 && fabs(m[pt.x-1+pt.y*size.x]-tc) <= tol) {
    	  s.push(XYPoint(pt.x-1,pt.y));
    	  spanLeft=true;
    	}
      else if(spanLeft && pt.x>0 && fabs(m[pt.x-1+pt.y*size.x]-tc) > tol) spanLeft=false;
      if(!spanRight && pt.x<size.x-1 && fabs(m[pt.x+1+pt.y*size.x]-tc) <= tol) {
    	  s.push(XYPoint(pt.x+1,pt.y));
    	  spanRight=true;
    	}
      else if(spanRight && pt.x<size.x-1 && fabs(m[pt.x+1+pt.y*size.x]-tc) > tol) spanRight=false;
      pt.y++;
    }
  }
  while(offsets.pop(pt)) m[pt.x+pt.y*size.x]=rc;
}

/* -------------------------------------------------------------------------- */
struct Box {
  Box(): t(0), l(0), r(0), b(0) {}
  Box(int tt, int ll, int rr, int bb): t(tt), l(ll), r(rr), b(bb) {}
  int t, l, r, b;
  void expand(int px=1) {
    t -= px;
    l -= px;
    r += px;
    b += px;
  }
};

/* -------------------------------------------------------------------------- */
/** 
  Extended floodfill version: to be used in fillHull only

  Fills image canvas (of width size.x * size.y):
  - under box 
  - with color rc
  - with points of m (of width size.x * size.y) which are NOT of color ntc
  - with a starting at top-left corner
  
*/
/* -------------------------------------------------------------------------- */
/* Templated version by Oleg Sklyar */
template <class T> void 
_fillAroundObjectHullT(T **m, T **canvas, const Box &box, int &rc) {
  XYStack s;
  XYPoint pt;
  bool spanLeft,spanRight;

  pt.x = box.l;
  pt.y = box.t;
    
  // pushes the starting pixel
  s.push(pt);
    
  while(s.pop(pt)) {    
    // climbs up along the column x as far as possible
    while(pt.y>=box.t && (int)m[pt.x][pt.y]!=rc && (int)canvas[pt.x][pt.y]!=rc) pt.y--;
    pt.y++;
    spanLeft=false;
    spanRight=false;
    // processes the column x
    while(pt.y<=box.b && (int)m[pt.x][pt.y]!=rc) {
      R_CheckUserInterrupt();
      canvas[pt.x][pt.y]=rc;
      if(!spanLeft && pt.x>box.l && (int)m[pt.x-1][pt.y]!=rc && 
                                    (int)canvas[pt.x-1][pt.y]!=rc) {
    	  s.push(XYPoint(pt.x-1,pt.y));
    	  spanLeft=true;
    	} else 
      if(spanLeft && pt.x>box.l && ((int)m[pt.x-1][pt.y]==rc || (int)canvas[pt.x-1][pt.y]==rc)) spanLeft=false;
      if(!spanRight && pt.x<box.r && (int)m[pt.x+1][pt.y]!=rc && 
                                     (int)canvas[pt.x+1][pt.y]!=rc) {
    	  s.push(XYPoint(pt.x+1,pt.y));
    	  spanRight=true;
    	} else 
    	if(spanRight && pt.x<box.r && ((int)m[pt.x+1][pt.y]==rc || (int)canvas[pt.x+1][pt.y]==rc)) spanRight=false;
      pt.y++;
    }
  }
}

/* -------------------------------------------------------------------------- */
/* Templated version by Oleg Sklyar: T assumed to be int or double (at least)
   dereferancible to those! */

template <class T> void
_fillHullT(T *_m, const XYPoint &srcsize) {
  int nobj = 0, i, x, y;
  XYPoint size = srcsize;

  // computes maximum number of different objects
  for (i=0; i < srcsize.x*srcsize.y; i++)
    if ((int)_m[i] > nobj) nobj = (int)_m[i];

  // nothing to do if no objects
  if (nobj < 1) return;

  // extend m by 2 pixels, copy content of _m inside, the frame - 0;
  // initialize temporary canvas with 0 
  size.x += 2;
  size.y += 2;

  typedef T* pT;
  T ** m = new pT[size.x];
  T ** canvas = new pT[size.x];
  for (x=0; x < size.x; x++) {
    m[x] = new T[size.y];
    canvas[x] = new T[size.y];
    for (y=0; y < size.y; y++) {
      canvas[x][y] = (T)0;
      if (x==0 || x==size.x-1 || y==0 || y==size.y-1) m[x][y] = (T)0;
      else m[x][y] = _m[x-1 + (y-1)*srcsize.x];
    }
  }

  // allocate and compute bounding boxes for all objects (the one for 0 never used)
  Box * bbox = new Box[nobj+1];

  for (i=1; i <= nobj; i++) {
    bbox[i].l = size.x-2;
    bbox[i].t = size.y-2;
  }

  for (x=1; x < size.x-1; x++)
    for (y=1; y < size.y-1; y++) {
      if ( (i=(int)m[x][y]) == 0) continue;
      if (x < bbox[i].l) bbox[i].l = x;
      else if (bbox[i].r < x) bbox[i].r = x;
      if (y < bbox[i].t) bbox[i].t = y;
      else if (bbox[i].b < y) bbox[i].b = y;
    }

  // reverse filling
  for (i=1; i <= nobj; i++) {
    Box box = bbox[i];
    box.expand(1);
    _fillAroundObjectHullT<T>(m, canvas, box, i);
    // fill back the original matrix!
  	for (x=box.l+1; x <= box.r-1; x++)
      for (y=box.t+1; y <= box.b-1; y++) {
	      // if ((int)_m[x-1+(y-1)*srcsize.x] > 0) continue;
	      if ((int)m[x][y] != 0 || (int)canvas[x][y]==i) continue;
        // this should never happen, but just in case
	      if (x-1<0 || x-1>=srcsize.x || y-1<0 || y-1>=srcsize.y) continue;
 	      _m[x-1+(y-1)*srcsize.x] = (T)i;
	    }
  }
  // cleanup
  for (x=0; x < size.x; x++) {
    delete[] m[x];
    delete[] canvas[x];
  }
  delete[] m;
  delete[] canvas;
  delete[] bbox;
}

