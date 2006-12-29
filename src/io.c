/* -------------------------------------------------------------------------
Image I/O
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */
SEXP
lib_readImages (SEXP files, SEXP mode) {
    SEXP res;
    int _mode, i, nappends;
    Image * image, * images;
    ImageInfo * image_info;
    ExceptionInfo exception;
    char * file;

    if ( LENGTH(files) < 1 )
        error ( _("please supply at least one file name or URL") );
    _mode = INTEGER (mode)[0];
    if ( _mode < 0 || _mode > MAX_MODE )
        error ( _("requested mode is not supported") );
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
            warning ( _("requested image not found or could not be loaded") );
            continue;
        }
        /* do not destroy image here */
        AppendImageToList (&images, image);
        if ( nappends == 0 ) {
            /* set all attributes from the first image */ 
            strcpy (images->filename, image->filename);
            images->compression = image->compression;
            images->filter = image->filter;
            images->x_resolution = image->x_resolution;
            images->y_resolution = image->y_resolution;
            images->filter = LanczosFilter;
        }
        nappends++;
    }
    /* do not update image properties here because if no image was added to 
    the list it will cause segfault, or use GetImageListLength first to check size */
    DestroyImageInfo (image_info);
    /* convert image list into R object */
    res = magick2SEXP (images, _mode);
    DestroyImageList (images);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_chooseImages () {
#ifndef USE_GTK
    error ( _("'choose.image' is only available if package is compiled with GTK+ support" ) );
    return R_NilValue;
#else
    SEXP res, filename, mode;
    int argc, nprotect, nfiles, i;
    char ** argv;
    GtkWidget * dialog;
    GSList * fileNameList;
#   ifndef WIN32
    InputHandler * hdlr;
#   endif

    /* initialize gtk */
    argc = 1;
    argv = (char **) R_alloc (1, sizeof(char *) );
    argv[0] = R_alloc (255, sizeof(char) );
    strcpy (argv[0], "R session\0");

    if ( !gtk_init_check(&argc, &argv) )
        error ( _("failed to initialize GTK+, use 'read.image' instead") );
        
    /* add R event handler to enable automatic window redraw */
#   ifndef WIN32
    hdlr = addInputHandler(R_InputHandlers, ConnectionNumber(GDK_DISPLAY()), _doIter, -1);
#   else
    R_tcldo = _doIterWin32;
#   endif

    dialog = gtk_file_chooser_dialog_new ("Select images to read into the R session",
				      NULL, //parent_window,
				      GTK_FILE_CHOOSER_ACTION_OPEN,
				      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
				      NULL);  
    gtk_file_chooser_set_select_multiple ( GTK_FILE_CHOOSER(dialog), TRUE);
//    GtkImage * preview;
// CHECK   GtkFileFilter* gtk_file_filter_new
// CHECK   gtk_file_chooser_set_preview_widget ( GTK_FILE_CHOOSER(dialog), preview);
//     g_signal_connect (my_file_chooser, "update-preview", G_CALLBACK (update_preview_cb), preview);
    
    res = R_NilValue;
    filename = R_NilValue;
    mode = R_NilValue;
    nprotect = 0;
    nfiles = 0;
    
    if ( gtk_dialog_run( GTK_DIALOG(dialog) ) == GTK_RESPONSE_ACCEPT ) {
        fileNameList = gtk_file_chooser_get_filenames ( GTK_FILE_CHOOSER(dialog) );
        nfiles = g_slist_length (fileNameList);
        if ( nfiles >= 1 ) {
            PROTECT ( filename = allocVector(STRSXP, nfiles) );
            nprotect++;
            for ( i = 0; i < nfiles; i++ ) {
                SET_STRING_ELT (filename, i, mkChar( (char *) g_slist_nth_data (fileNameList, i) ) );
            }
            PROTECT ( mode = allocVector(INTSXP, 1) );
            nprotect++;
            INTEGER(mode)[0] = 1;
        }
        else
            error ( _("no files were selected") );
        g_slist_free (fileNameList);
    }
    gtk_widget_destroy (dialog);
    if ( filename != R_NilValue && mode != R_NilValue )
        res = lib_readImages(filename, mode);
    UNPROTECT (nprotect);
    if ( res == R_NilValue )
        error ( _("cancel pressed or no image could be loaded") );
    return res;
#endif
}

/*----------------------------------------------------------------------- */
SEXP
lib_writeImages (SEXP x, SEXP files, SEXP quality) {
    int nz, nfiles, i;
    Image * images, * image;
    ImageInfo *image_info;
    char * file;
        
    /* basic checks */
    if ( !isImage(x) )
        error ( _("argument must be of class 'Image'") );
    nz = INTEGER ( GET_DIM(x) )[2];
    nfiles = LENGTH (files);
    if ( nfiles != 1 && nfiles != nz)
        error ( _("number of files must be 1, or equal to the size of the image stack") );
    images = sexp2Magick (x);
    if ( images == NULL )
        error ( _("cannot write an empty image") );
    if ( GetImageListLength (images) < 1 )
        error ( _("cannot write an empty image") );
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    /* set attributes in image_info*/
    image_info->compression = images->compression;
    image_info->quality = (unsigned int) INTEGER (quality)[0];
    if ( nfiles == 1 ) {
    /* save into a single file, TIFF, GIF, or automatically add file suffixes */
        strcpy (image_info->filename, CHAR( asChar(files) ) );
        /* we want to overwrite the feature imported from SEXP image */
        strcpy (images->filename, image_info->filename);
        if ( WriteImage (image_info, images) == 0 )
            error ( _("cannot write image, check path and file name (UNIX home directories with ~ are not supported)") );
        CatchException (&images->exception);
    }
    else {
    /* save each frame into a separate file */
        for ( i = 0; i < nz; i++ ) {
            file = CHAR ( asChar( STRING_ELT(files, i) ) );
            image = GetImageFromList (images, i);
            if ( image == NULL ) {
                warning ( _("cannot write an empty image, skipping") );
                continue;
            }
            if ( GetImageListLength (image) < 1 ) {
                warning ( _("cannot write an empty image, skipping") );
                continue;
            }
            strcpy (image_info->filename, file);
            /* we want to overwrite the feature imported from SEXP image */
            strcpy (image->filename, image_info->filename);
            if ( WriteImage (image_info, image) == 0 )
                warning ( _("cannot write image, check path and file name (UNIX home directories with ~ are not supported") );
            CatchException (&image->exception);
        }    
    }
    
    DestroyImageInfo (image_info);
    DestroyImageList (images);
    return R_NilValue;
}
