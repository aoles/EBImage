#include "filters_floodfill.h"

#include <R_ext/Error.h>
#include <stack>

/* -------------------------------------------------------------------------- */
struct XYPoint {
  XYPoint() {}
  XYPoint(int xx, int yy): x(xx), y(yy) {}
  int x, y;
};

template <class T>void floodFill(T*, XYPoint, XYPoint, T, double tolerance = 1e-3);
void fillHull(int *, const XYPoint &);

/* -------------------------------------------------------------------------- */
SEXP
lib_floodFill(SEXP x, SEXP point, SEXP col, SEXP tol) {
  int *dim = INTEGER(GET_DIM(x));
  XYPoint size(dim[0], dim[1]);
  if (LENGTH(GET_DIM(x))>2 && dim[2]>1)
    error("'floodFill' function is not defined for arrays or multi-frame images");
  XYPoint pt(INTEGER(point)[0], INTEGER(point)[1]);
  /* if nothing to fill quit */
  if (size.x <= 0 || pt.x >= size.x || size.y <= 0 || pt.y >= size.y) return x;
  SEXP m;
  int nprotect=0;
  /* copy existing image/matrix to result */
  PROTECT(m = Rf_duplicate(x));
  nprotect++;
  /* do floodfil, templated version used depending on the storage mode */
  if (IS_INTEGER(x)) {
    floodFill<int>(INTEGER(m), size, pt, INTEGER(col)[0], REAL(tol)[0]);
  }
  else if (IS_NUMERIC(x)) {
    floodFill<double>(REAL(m), size, pt, REAL(col)[0], REAL(tol)[0]);
  }
  
  UNPROTECT (nprotect);
  return m;
}

/* -------------------------------------------------------------------------- */
SEXP
lib_fillHull(SEXP x) {
  if (!IS_INTEGER(x)) error("'fillHull' is defined only for integer-based data");
  int *dim = INTEGER(GET_DIM(x));
  XYPoint size(dim[0], dim[1]);
  /* check if array or multiple images */
  int nz = 1;
  if (LENGTH(GET_DIM(x))>2) nz = dim[2];
  /* return itself if nothing to do */  
  if (size.x <= 0 || size.y <= 0 || nz < 1) return x;
  SEXP m;
  int nprotect = 0;
  /* do fillHull */  
  PROTECT(m=Rf_duplicate(x));
  nprotect++;
  for (int i=0; i < nz; i++)
    fillHull(&(INTEGER(m)[i*size.x*size.y]), size);
  
  UNPROTECT (nprotect);
  return m;
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
floodFill(T *m, XYPoint size, XYPoint xy, T rc, double tolerance = 1e-3) {
  XYStack s;
  XYPoint pt = xy;
  bool spanLeft,spanRight;
  /* set the target color tc */
  T tc = m[pt.x+pt.y*size.x];
    
  // pushes the seed starting pixel
  s.push(pt);
    
  while(s.pop(pt)) {    
    // climbs up along the column x as far as possible
    while(pt.y>=0 && fabs(m[pt.x+pt.y*size.x]-tc) <= tolerance) pt.y--;
    pt.y++;
    spanLeft=false;
    spanRight=false;

    // processes the column x
    while(pt.y<size.y && fabs(m[pt.x+pt.y*size.x]-tc) <= tolerance) {
      m[pt.x+pt.y*size.x]=rc;
      if(!spanLeft && pt.x>0 && fabs(m[pt.x-1+pt.y*size.x]-tc) <= tolerance) {
    	  s.push(XYPoint(pt.x-1,pt.y));
    	  spanLeft=true;
    	}
      else if(spanLeft && pt.x>0 && fabs(m[pt.x-1+pt.y*size.x]-tc) > tolerance) spanLeft=false;
      if(!spanRight && pt.x<size.x-1 && fabs(m[pt.x+1+pt.y*size.x]-tc) <= tolerance) {
    	  s.push(XYPoint(pt.x+1,pt.y));
    	  spanRight=true;
    	}
      else if(spanRight && pt.x<size.x-1 && fabs(m[pt.x+1+pt.y*size.x]-tc) > tolerance) spanRight=false;
      pt.y++;
    }
  }
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
void 
fillAroundObjectHull(int **m, int **canvas, const XYPoint &size, const Box &box, 
                     const int &rc) {
  XYStack s;
  XYPoint pt;
  bool spanLeft,spanRight;
  
  pt.x = box.l;
  pt.y = box.t;
    
  // pushes the starting pixel
  s.push(pt);
    
  while(s.pop(pt)) {    
    // climbs up along the column x as far as possible
    while(pt.y>=box.t && m[pt.x][pt.y]!=rc && canvas[pt.x][pt.y]!=rc) pt.y--;
    pt.y++;
    spanLeft=false;
    spanRight=false;
    // processes the column x
    while(pt.y<=box.b && m[pt.x][pt.y]!=rc) {
      canvas[pt.x][pt.y]=rc;
      if(!spanLeft && pt.x>box.l && m[pt.x-1][pt.y]!=rc && canvas[pt.x-1][pt.y]!=rc) {
    	  s.push(XYPoint(pt.x-1,pt.y));
    	  spanLeft=true;
    	}
      else if(spanLeft && pt.x>box.l && (m[pt.x-1][pt.y]==rc || canvas[pt.x-1][pt.y]==rc)) spanLeft=false;
      if(!spanRight && pt.x<box.r && m[pt.x+1][pt.y]!=rc && canvas[pt.x+1][pt.y]!=rc) {
    	  s.push(XYPoint(pt.x+1,pt.y));
    	  spanRight=true;
    	}
      else if(spanRight && pt.x<box.r && (m[pt.x+1][pt.y]==rc || canvas[pt.x+1][pt.y]==rc)) spanRight=false;
      pt.y++;
    }
  }
}

/* Templated version:
template <class T> void 
fillAroundObjectHull(T **m, T **canvas, const XYPoint &size, const Box &box, 
                     const T &rc, double tolerance=1e-3) {
  XYStack s;
  XYPoint pt;
  bool spanLeft,spanRight;
  T ntc = rc;
  
  pt.x = box.l;
  pt.y = box.t;
    
  // pushes the starting pixel
  s.push(pt);
    
  while(s.pop(pt)) {    
    // climbs up along the column x as far as possible
    while(pt.y>=box.t && fabs(m[pt.x][pt.y]-ntc) > tolerance && fabs(canvas[pt.x][pt.y]-rc) > tolerance) pt.y--;
    pt.y++;
    spanLeft=false;
    spanRight=false;
    // processes the column x
    while(pt.y<=box.b && fabs(m[pt.x][pt.y]-ntc) > tolerance) {
      canvas[pt.x][pt.y]=rc;
      if(!spanLeft && pt.x>box.l && fabs(m[pt.x-1][pt.y]-ntc) > tolerance && fabs(canvas[pt.x-1][pt.y]-rc) > tolerance) {
    	  s.push(XYPoint(pt.x-1,pt.y));
    	  spanLeft=true;
    	}
      else if(spanLeft && pt.x>box.l && (fabs(m[pt.x-1][pt.y]-ntc) <= tolerance || fabs(canvas[pt.x-1][pt.y]-rc) <= tolerance)) spanLeft=false;
      if(!spanRight && pt.x<box.r && fabs(m[pt.x+1][pt.y]-ntc) > tolerance && fabs(canvas[pt.x+1][pt.y]-rc) > tolerance) {
    	  s.push(XYPoint(pt.x+1,pt.y));
    	  spanRight=true;
    	}
      else if(spanRight && pt.x<box.r && (fabs(m[pt.x+1][pt.y]-ntc) <= tolerance || fabs(canvas[pt.x+1][pt.y]-rc) <= tolerance)) spanRight=false;
      pt.y++;
    }
  }
}
*/

/* -------------------------------------------------------------------------- */
void
fillHull(int *_m, const XYPoint &srcsize) {
  int nobj = 0, i, x, y;
  XYPoint size = srcsize;

  /* computes maximum number of different objects */
  for (i=0; i < srcsize.x*srcsize.y; i++)
    if (_m[i] > nobj) nobj = _m[i];

  /* nothing to do if no objects */
  if (nobj < 1) return;

  /* extend m 2 pixels, copy content of _m inside, the frame - 0;
     initialize temporary canvas with 0 */
  size.x += 2;
  size.y += 2;

  typedef int* pint;
  int ** m = new pint[size.x];
  int ** canvas = new pint[size.x];
  for (x=0; x < size.x; x++) {
    m[x] = new int[size.y];
    canvas[x] = new int[size.y];
    for (y=0; y < size.y; y++) {
      canvas[x][y] = 0;
      if (x== 0 || x == size.x-1) m[x][y] = 0;
      else m[x][y] = _m[x-1 + (y-1)*srcsize.x];
    }
  }

  /* allocate and compute bounding boxes for all objects (the one for 0 never used) */
  Box * bbox = new Box[nobj+1];

  for (i=1; i <= nobj; i++) {
    bbox[i].l = size.x-1;
    bbox[i].t = size.y-1;
  }

  for (x=1; x < size.x-1; x++)
    for (y=1; y < size.y-1; y++) {
      if ( (i=m[x][y]) == 0) continue;
      if (x < bbox[i].l) bbox[i].l = x;
      else if (bbox[i].r < x) bbox[i].r = x;
      if (y < bbox[i].t) bbox[i].t = y;
      else if (bbox[i].b < y) bbox[i].b = y;
    }

  // reverse filling
  for (i=1; i <= nobj; i++) {
    Box box = bbox[i];
    box.expand(1);
    fillAroundObjectHull(m, canvas, size, box, i);
    /* fill back the original matrix! */
  	for (x=box.l; x <= box.r; x++)
      for (y=box.t; y <= box.b; y++) {
	      if (canvas[x][y]==i || x-1<0 || x-1>=srcsize.x || y-1<0 || y-1>=srcsize.y) continue;
 	      _m[x-1+(y-1)*srcsize.x] = i;
	    }
  }

  /* cleanup */
  for (x=0; x < size.x; x++) {
    delete[] m[x];
    delete[] canvas[x];
  }
  delete[] m;
  delete[] canvas;
  delete[] bbox;
}

/* Templated version:
template <class T> void
fillHull(T *_m, const XYPoint &srcsize) {
  int nobj = 0, i, x, y;
  XYPoint size = srcsize;

  // computes maximum number of different objects
  for (i=0; i < srcsize.x*srcsize.y; i++)
    if ((int)_m[i] > nobj) nobj = (int)_m[i];

  // nothing to do if no objects
  if (nobj < 1) return;

  // extend m 2 pixels, copy content of _m inside, the frame - 0;
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
      if (x== 0 || x == size.x-1) m[x][y] = (T)0;
      else m[x][y] = _m[x-1 + (y-1)*srcsize.x];
    }
  }

  // allocate and compute bounding boxes for all objects (the one for 0 never used)
  Box * bbox = new Box[nobj+1];

  for (i=1; i <= nobj; i++) {
    bbox[i].l = size.x-1;
    bbox[i].t = size.y-1;
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
    T col = (T)i;
    fillAroundObjectHull<T>(m, canvas, size, box, col, col);
    // fill back the original matrix!
  	for (x=box.l; x <= box.r; x++)
      for (y=box.t; y <= box.b; y++) {
	      if (canvas[x][y]==i || x-1<0 || x-1>=srcsize.x || y-1<0 || y-1>=srcsize.y) continue;
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
*/

