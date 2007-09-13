#include "drawable.h"

/* -------------------------------------------------------------------------
Drawing on images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"

#include <R_ext/Error.h>
/* this module uses MagickWand API instead of MagickCore */
#include <wand/magick-wand.h>

SEXP
lib_drawText (SEXP obj, SEXP xylist, SEXP textlist, SEXP thefont, SEXP thecol) {
  SEXP res, xy, text;
  int i, im, nz, mode, nval, nappended=0, nprotect=0, colcount=0;
  DrawingWand * dwand;
  MagickWand  * mwand;
  PixelWand * pwand;
  double * dxy;
  Image * images, * image, * newimages;
  const char * str;
  
  if ( !isImage(obj) )
    error("'obj' must be an Image");

  nz = INTEGER(GET_DIM(obj))[2];

  if ( LENGTH(xylist) != LENGTH(textlist) || LENGTH(xylist) != nz )
    error("lists of coordinates 'xy' labels 'labels' must be of the same length as the number of frames");

  mode = INTEGER( GET_SLOT(obj, mkString("colormode")) )[0];
  images = sexp2Magick(obj);
  newimages = NewImageList();

  /* start magick wand */
  MagickWandGenesis();
  /* create empty wand */
  dwand = NewDrawingWand();
  pwand = NewPixelWand();
  /* loop through images */
  for ( im = 0; im < nz; im++ ) {
    /* create magick wand from one image, this does NOT copy */
    mwand = NewMagickWandFromImage(GetFirstImageInList(images));
    /* get pointers to current list elements if those are lists */
    xy = VECTOR_ELT(xylist, im);
    dxy = REAL(xy);
    text = VECTOR_ELT(textlist, im);
    nval = LENGTH(text);
    if ( nval > 0 && LENGTH(xy) >= 2 * nval ) {
      /* clear drawing wand */
      ClearDrawingWand(dwand);
      str = CHAR(STRING_ELT(VECTOR_ELT(thefont, 0),0));
      if (str)
        DrawSetFontFamily(dwand, str);
      switch ( INTEGER(VECTOR_ELT(thefont, 1))[0]) {
        case 1:  DrawSetFontStyle(dwand, ItalicStyle); break;
        case 2:  DrawSetFontStyle(dwand, ObliqueStyle); break;
        default: DrawSetFontStyle(dwand, NormalStyle);
      }
      DrawSetStrokeAntialias(dwand, INTEGER(VECTOR_ELT(thefont, 4))[0]);
      DrawSetFontWeight(dwand, REAL(VECTOR_ELT(thefont, 3))[0]);
      DrawSetFontSize(dwand, REAL(VECTOR_ELT(thefont, 2))[0]);
      PixelSetColor(pwand, CHAR(STRING_ELT(thecol, colcount)));
      colcount++;
      if ( colcount >= LENGTH(thecol) ) colcount = 0;
      DrawSetFillColor(dwand, pwand);
      /* no need: DrawSetStrokeColor(dwand, PixelWand); */
      /* add text to the wand */
      for ( i = 0; i < nval; i++ ) {
        str = CHAR(STRING_ELT(text, i));
        if (str)
          DrawAnnotation(dwand, dxy[i], dxy[i + nval], (unsigned char *)str );
      }
      /* draw the wand */
      MagickDrawImage(mwand, dwand);
    }
    else {
      /* do not draw if more text labels than coordinates */
      warning("not enough coordinate points to output all labels");
    }
    image = GetImageFromMagickWand(mwand);
    AppendImageToList (&newimages, image);
    if ( nappended == 0 ) {
      /* copy attributes once and only if image contains more than 1 element */
      nappended = 1;
      newimages->compression = images->compression;
      strcpy (newimages->filename, image->filename);
      newimages->x_resolution = images->x_resolution;
      newimages->y_resolution = images->y_resolution;
    }
    /* the image is destroyed below in DestroyImage 
    mwand = DestroyMagickWand(mwand);
    */
    image = GetFirstImageInList(images);
    RemoveFirstImageFromList(&images);
    image = DestroyImage(image);
  }
  pwand = DestroyPixelWand(pwand);
  dwand = DestroyDrawingWand(dwand);
  MagickWandTerminus();
  
  images = DestroyImageList(images);

  PROTECT( res = magick2SEXP(newimages, mode) );
  nprotect++;
  SET_SLOT(res, install("features"), Rf_duplicate(GET_SLOT(obj, mkString("features"))) );
  newimages = DestroyImageList(newimages);

  UNPROTECT( nprotect );
  return res;
}

