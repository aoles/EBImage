#include "tools.h"
#include <R.h>
#include <Rdefines.h>

// test=0 will make validImage fail if x is not an image
// test=1 will return 0 if x is not an Image
int validImage (SEXP x,int test) {
  int colorMode;
  char *msg=NULL;

  // check Nil
  if (x==R_NilValue) msg="object is NULL";
  else {
    // check colormode
    colorMode=COLOR_MODE(x);
    if (colorMode<0 || colorMode >2) msg="invalid colormode";
    
    // check dim
    if (LENGTH(GET_DIM(x))<2) msg="object must contain at least two dimensions";
    if (INTEGER(GET_DIM(x))[0]<1 || INTEGER(GET_DIM(x))[1]<1) msg="spatial dimensions of object must be higher than zero"; 
    if (getNumberOfFrames(x,0)<1) msg="object must contain at least one frame";
  }

  if (test==0 && msg!=NULL) error(msg);
  if (msg!=NULL) return(0);
  else return(1);
}

// If type=0, returns the total number of frames
// If type=1, returns the number of frames to be rendered, according to the colorMode
int getNumberOfFrames(SEXP x, int type) {
  int n,colorMode;
  int k,p,kp;
  colorMode=COLOR_MODE(x);

  if (type==1 && colorMode==MODE_COLOR) kp=3;
  else kp=2;
  
  n=1;
  p=GET_LENGTH(GET_DIM(x));
  if (p>kp) {
    for (k=kp;k<p;k++) n=n*INTEGER(GET_DIM(x))[k];
  }

  return(n);
}

int getNumberOfChannels(SEXP x) {
  int colorMode;
  int nbChannels;
  colorMode=COLOR_MODE(x);

  if (colorMode!=MODE_COLOR) nbChannels=1;
  else {
    if (LENGTH(GET_DIM(x))<3) nbChannels=1;
    else nbChannels=INTEGER(GET_DIM(x))[2];
  }
  return(nbChannels);
}

void getColorStrides(SEXP x,int index,int *redstride,int *greenstride,int *bluestride) {
  int width, height, nbChannels;

  width=INTEGER(GET_DIM(x))[0];
  height=INTEGER(GET_DIM(x))[1];
  nbChannels=getNumberOfChannels(x);
  
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
