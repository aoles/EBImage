#include "io.h"

/* -------------------------------------------------------------------------
Image I/O
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <magick/ImageMagick.h>

/* These are to use GTK */
#ifdef USE_GTK
#   include <gtk/gtk.h>
#   ifdef WIN32
        typedef unsigned long ulong;
#       include <sys/types.h>
#   else
#       include <gdk/gdkx.h>
#   endif
#endif

/*----------------------------------------------------------------------- */
// GP: mode = -1 will be automatically guessed from the file
SEXP
lib_readImages (SEXP files, SEXP mode) {
    SEXP res;
    int _mode, i, nappends;
    Image * image, * images;
    ImageInfo * image_info;
    ExceptionInfo exception;
    const char * file;
    ImageType it;

    if ( LENGTH(files) < 1 )
        error ( "please supply at least one file name or URL" );
    _mode = INTEGER (mode)[0];
    if ( _mode < -1 || _mode > MODE_MAX)
        error ( "requested mode is not supported" );
    image_info = (ImageInfo *) NULL;
    /* images loaded into image and moved into this list */
    images = NewImageList ();
    GetExceptionInfo (&exception);
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    nappends = 0;

    for ( i = 0; i < LENGTH (files); i++ ) {
        if ( LENGTH (files) > 1 )
            file = CHAR ( STRING_ELT(files, i) );
        else
            file = CHAR ( asChar(files) );
        strcpy (image_info->filename, file);

	// Prevent an ImageMagick bug when file is an empty string or NULL
	if (file==NULL) image=NULL;
	else if (strlen(file)==0) image=NULL;
	else {
	  image = ReadImage (image_info, &exception);
	  CatchException (&exception);
	}
        if ( image == (Image *)NULL ) {
            warning ("requested image not found or could not be loaded" );
            continue;
        }

	// Automatic color mode guess
	if (_mode==-1) {
	  it = GetImageType(image,&exception);
	  // Rprintf("it=%d G=%d P=%d PM=%d\n",it, GrayscaleType, PaletteType, PaletteMatteType);
	  if (it==BilevelType || it==GrayscaleType || it==GrayscaleMatteType) _mode=MODE_GRAYSCALE;
	  else _mode=MODE_COLOR;
	}
       
        /* do not destroy image here */
        AppendImageToList (&images, image);

        if ( nappends == 0 ) {
            /* set all attributes from the first image */
            strcpy (images->filename, image->filename);
            images->compression = image->compression;
            images->x_resolution = image->x_resolution;
            images->y_resolution = image->y_resolution;
        }
        nappends++;
    }
    /* do not update image properties here because if no image was added to
    the list it will cause segfault, or use GetImageListLength first to check size */
    image_info = DestroyImageInfo (image_info);

    /* convert image list into R object */
    res = magick2SEXP (images, _mode);
    images = DestroyImageList (images);

    DestroyExceptionInfo(&exception);

    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_writeImages (SEXP x, SEXP files, SEXP quality) {
    int nz, nfiles, i;
    Image * images, * image;
    ImageInfo *image_info;
    ExceptionInfo exception;

    /* basic checks */
    validImage(x,0);

    images = sexp2Magick (x);
    nz = GetImageListLength(images);
 
    nfiles = LENGTH (files);
    if ( nfiles != 1 && nfiles != nz)
        error ( "number of files must be 1, or equal to the size of the image stack" );
    
    if ( images == NULL || GetImageListLength (images) < 1 )
        error ( "cannot write an empty image" );
    GetExceptionInfo (&exception);
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    /* set attributes in image_info*/
    image_info->compression = images->compression;
    image_info->quality = (unsigned int) INTEGER (quality)[0];
    if ( nfiles == 1 ) {
    /* save into a single file, TIFF, GIF, or automatically add file suffixes */
        strcpy (image_info->filename, CHAR(STRING_ELT(files, 0)) );
        /* we want to overwrite the feature imported from SEXP image */
        strcpy (images->filename, image_info->filename);
        WriteImages(image_info, images, CHAR(STRING_ELT(files, 0)), &exception);
        CatchException (&exception);
    }
    else {
    /* save each frame into a separate file */
        for ( i = 0; i < nz; i++ ) {
            image = GetImageFromList (images, i);
            if ( image == NULL || GetImageListLength (image) < 1 ) {
                warning ( "cannot write an empty image, skipping" );
                continue;
            }
            strcpy (image_info->filename, CHAR(STRING_ELT(files, i)));
            /* we want to overwrite the feature imported from SEXP image */
            strcpy (image->filename, image_info->filename);
            WriteImage (image_info, image);
            CatchException (&image->exception);
            // WriteImages(image_info, image, CHAR(STRING_ELT(files, i)), &exception);
            // CatchException (&exception);

        }
    }

    image_info = DestroyImageInfo (image_info);
    images = DestroyImageList (images);
    DestroyExceptionInfo(&exception);
    return R_NilValue;
}
