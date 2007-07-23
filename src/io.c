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
SEXP
lib_readImages (SEXP files, SEXP mode) {
    SEXP res;
    int _mode, i, nappends;
    Image * image, * images;
    ImageInfo * image_info;
    ExceptionInfo exception;
    const char * file;

    if ( LENGTH(files) < 1 )
        error ( "please supply at least one file name or URL" );
    _mode = INTEGER (mode)[0];
    if ( _mode < 0 || _mode > MAX_MODE )
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
        image = ReadImage (image_info, &exception);
        CatchException (&exception);
        if ( image == (Image *)NULL ) {
            warning ( "requested image not found or could not be loaded" );
            continue;
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
lib_chooseImages (SEXP mode) {
#ifndef USE_GTK
  error ( "'choose.image' is only available if package is compiled with GTK+ support" );
  return R_NilValue;
#else
  SEXP res = R_NilValue, filename = R_NilValue;
  int nprotect = 0, nfiles = 0, i;

  if ( !GTK_OK ) error ("GTK+ was not properly initialised" );

  GSList * fileNameList;

  GtkWidget * dialog = gtk_file_chooser_dialog_new ("Select images to read into the R session",
      NULL, //parent_window,
      GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL);
  gtk_file_chooser_set_select_multiple ( GTK_FILE_CHOOSER(dialog), TRUE);
  //    GtkImage * preview;
  // CHECK   GtkFileFilter* gtk_file_filter_new
  // CHECK   gtk_file_chooser_set_preview_widget ( GTK_FILE_CHOOSER(dialog), preview);
  //     g_signal_connect (my_file_chooser, "update-preview", G_CALLBACK (update_preview_cb), preview);

  if ( gtk_dialog_run( GTK_DIALOG(dialog) ) == GTK_RESPONSE_ACCEPT ) {
    fileNameList = gtk_file_chooser_get_filenames ( GTK_FILE_CHOOSER(dialog) );
    nfiles = g_slist_length (fileNameList);
    if ( nfiles >= 1 ) {
      PROTECT ( filename = allocVector(STRSXP, nfiles) );
      nprotect++;
      for ( i = 0; i < nfiles; i++ ) {
        SET_STRING_ELT (filename, i, mkChar( (char *) g_slist_nth_data (fileNameList, i) ) );
      }
    }
    else error ( "no files were selected" );
    g_slist_free (fileNameList);
  }
  gtk_widget_destroy (dialog);
  if ( filename != R_NilValue && mode != R_NilValue )
    res = lib_readImages(filename, mode);
  UNPROTECT (nprotect);
  if ( res == R_NilValue )
    error ( "cancel pressed or no image could be loaded" );
  return res;
#endif
}

/*----------------------------------------------------------------------- */
SEXP
lib_writeImages (SEXP x, SEXP files, SEXP quality) {
    int nz, nfiles, i;
    Image * images, * image;
    ImageInfo *image_info;
    ExceptionInfo exception;

    /* basic checks */
    if ( !isImage(x) )
        error ( "argument must be of class 'Image'" );
    nz = INTEGER ( GET_DIM(x) )[2];
    nfiles = LENGTH (files);
    if ( nfiles != 1 && nfiles != nz)
        error ( "number of files must be 1, or equal to the size of the image stack" );
    images = sexp2Magick (x);
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
