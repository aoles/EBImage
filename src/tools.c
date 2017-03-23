#include <R.h>
#include <Rdefines.h>
#include "tools.h"
#include "EBImage.h"

SEXP numberOfFrames(SEXP x, SEXP type) {
  SEXP res = PROTECT( ScalarInteger(getNumberOfFrames(x, INTEGER(type)[0])) );
  UNPROTECT(1);
  return res;
}

char * _validImageObject (SEXP x) {
  int colorMode;
  
  // check Nil
  if (x==R_NilValue) return("object is NULL");
  else {
    // check array
    if(!isArray(x)) return("object must be an array");
    
    // check dim
    if (LENGTH(GET_DIM(x))<2) return("object must have at least two dimensions");
    if (INTEGER(GET_DIM(x))[0]<1 || INTEGER(GET_DIM(x))[1]<1) return("spatial dimensions of object must be higher than zero"); 
    if (getNumberOfFrames(x,0)<1) return("object must contain at least one frame");
    
    // check colormode
    colorMode=COLOR_MODE(x);
    if (colorMode!=0 && colorMode!=2) return("invalid colormode");
  }
  return(NULL);
}

// test=0 will make validImage fail if x is not an image
// test=1 will return 0 if x is not an Image
int validImage (SEXP x,int test) {
  char *msg = _validImageObject(x);
  
  if (test==0 && msg!=NULL) error(msg);
  if (msg!=NULL) return(0);
  else return(1);
}

SEXP validImageObject (SEXP x) {
  SEXP res;
  char *msg = _validImageObject(x);
  res = (msg==NULL) ? PROTECT( ScalarLogical(1) ) : PROTECT( mkString(msg) );
  UNPROTECT (1);
  return(res);
}

// If type=0, returns the total number of frames
// If type=1, returns the number of frames to be rendered, according to the colorMode
int getNumberOfFrames(SEXP x, int type) {
  int k;
  int kp = (type==1 && COLOR_MODE(x)==MODE_COLOR) ? 3 : 2;
  int n = 1;
  int p = LENGTH(GET_DIM(x));
  if (p > kp) {
    for (k = kp; k < p; k++) n*=INTEGER(GET_DIM(x))[k];
  }
  
  return(n);
}

int getNumberOfChannels(SEXP x, int colormode) {
  if (colormode!=MODE_COLOR)
    return(1);
  else
    return(LENGTH(GET_DIM(x))<3 ? 1 : INTEGER(GET_DIM(x))[2]);
}

void getColorStrides(SEXP x,int index,int *redstride,int *greenstride,int *bluestride) {
  int width, height, nbChannels;
  
  width=INTEGER(GET_DIM(x))[0];
  height=INTEGER(GET_DIM(x))[1];
  nbChannels=getNumberOfChannels(x, COLOR_MODE(x));
  
  *redstride=index*nbChannels*width*height;
  *greenstride=-1;
  *bluestride=-1;
  
  if (nbChannels>1) *greenstride=index*nbChannels*width*height+width*height;
  if (nbChannels>2) *bluestride=index*nbChannels*width*height+2*width*height;
}

int isImage(SEXP x) {
  static const char *valid[] = { "Image", ""};
  return R_check_class_etc(x, valid) + 1;
}
