#include "drawable.h"

/* -------------------------------------------------------------------------
Drawing on images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"

#include <R_ext/Error.h>
//#include <magick/ImageMagick.h>

/* unfortunately I have to use MAgickWand here. I do not like it to be honest
 * and it also means one extra copying of images - bad thing */
#include <wand/magick-wand.h>



SEXP
lib_drawText (SEXP obj, SEXP xylist, SEXP textlist) {
  SEXP res, xy, text;
  int i, im, nz, mode, nval, nappended=0, nprotect=0;
  DrawingWand * dwand;
  MagickWand  * mwand;
  double * dxy;
  Image * images, * image, * newimages;
  
  if ( !isImage(obj) )
    error("'obj' must be an Image");

  nz = INTEGER(GET_DIM(obj))[2];

  if ( LENGTH(xylist) != LENGTH(textlist) || LENGTH(xylist) != nz )
    error("mismatch of the number of coordinates, labels or frames provided");

//  ExceptionInfo exception;

  mode = INTEGER( GET_SLOT(obj, mkString("colormode")) )[0];
  images = sexp2Magick(obj);
  newimages = NewImageList();


  MagickWandGenesis();

  /* create empty wand */
  dwand = NewDrawingWand();

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
      /* add text to the wand */
      for ( i = 0; i < nval; i++ ) {
        DrawAnnotation(dwand, dxy[i], dxy[i + nval],
          (unsigned char *)CHAR(STRING_ELT(text, i)) );
      }
      /* draw the wand */
      MagickDrawImage(mwand, dwand);
    }
    else {
      /* do not draw if more text labels than coordinates */
      warning("not enough coordinate points to output all text");
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
    // FIXME: I believe image is destroyed below in DestroyImage, 
    // but this needs testing
    // mwand = DestroyMagickWand(mwand);
    image = GetFirstImageInList(images);
    RemoveFirstImageFromList(&images);
    image = DestroyImage(image);
  }
  dwand = DestroyDrawingWand(dwand);
  MagickWandTerminus();
  
  images = DestroyImageList(images);

  PROTECT( res = magick2SEXP(newimages, mode) );
  nprotect++;
  SET_SLOT(res, install("features"), Rf_duplicate(GET_SLOT(obj, mkString("features"))) );
  newimages = DestroyImageList(newimages);

//  DestroyExceptionInfo(&exception);

  UNPROTECT( nprotect );
  return res;
}

