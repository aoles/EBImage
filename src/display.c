/* -------------------------------------------------------------------------
Image conversions between MagickCore and R
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"
#include <R_ext/Memory.h>

#ifndef WIN32
#   include <pthread.h>
#endif

int THREAD_ON = 0;

/*----------------------------------------------------------------------- */
void * _showInImageMagickWindow (void *);
void * _animateInImageMagickWindow (void *);
#ifdef USE_GTK
void _showInGtkWindow (SEXP);
#endif
/*----------------------------------------------------------------------- */
SEXP
lib_display(SEXP x, SEXP nogtk) {
#ifndef WIN32
    pthread_t res;
#endif

    if ( !isImage(x) )
        error ( _("argument must be of class 'Image'") );

#ifdef USE_GTK
    if ( !LOGICAL(nogtk)[0] ) {
        _showInGtkWindow (x);
        return R_NilValue;
    }
#endif   

#ifdef WIN32
    error ( _("only GTK+ display is awailable on Windows") );
#else
    if ( THREAD_ON )        
        error ( _("cannot display concurent windows. Close currently displayed window first.") );
    if ( pthread_create(&res, NULL, _showInImageMagickWindow, (void *)x ) != 0 )
        error ( _("cannot create display thread") );
#endif
    return R_NilValue;
}

/*----------------------------------------------------------------------- */
SEXP
lib_animate (SEXP x) {
#ifndef WIN32
    pthread_t res;
#endif

    if ( !isImage(x) )
        error ( _("argument must be of class 'Image'") );

#ifdef WIN32
    error ( _("animate function is not available on Windows because it uses ImageMagick interactive display") );
#else
    if ( THREAD_ON )        
        error ( _("cannot display concurent windows. Close currently displayed window first.") );
    if ( pthread_create(&res, NULL, _animateInImageMagickWindow, (void *)x ) != 0 )
        error ( _("cannot animate display thread") );
#endif
    return R_NilValue;
}

/*----------------------------------------------------------------------- */
void *
_showInImageMagickWindow (void * ptr) {
    SEXP x;
    Image * images;
    ImageInfo * image_info;
        
    x = (SEXP) ptr;
    THREAD_ON = 1;
    images = sexp2Magick (x);
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    strcpy (image_info->filename, "\0");
    DisplayImages (image_info, images);
    THREAD_ON = 0;
    images = DestroyImageList (images);
    image_info = DestroyImageInfo (image_info);
    return NULL;
}

/*----------------------------------------------------------------------- */
void *
_animateInImageMagickWindow (void * ptr) {
    SEXP x;
    Image * images;
    ImageInfo * image_info;
        
    x = (SEXP) ptr;
    THREAD_ON = 1;
    images = sexp2Magick (x);
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    strcpy (image_info->filename, "\0");
    AnimateImages (image_info, images);
    THREAD_ON = 0;
    images = DestroyImageList (images);
    image_info = DestroyImageInfo (image_info);
    return NULL;
}

/*----------------------------------------------------------------------- */
#ifdef USE_GTK

/* forward declarations */
gboolean onWinDestroy   (GtkWidget *, GdkEvent *, gpointer); // window "destroy-event"
gboolean onZoomInPress  (GtkToolButton *, gpointer);         // "button-press-event"
gboolean onZoomOutPress (GtkToolButton *, gpointer);         // "button-press-event"
gboolean onZoomOnePress (GtkToolButton *, gpointer);         // "button-press-event"
gboolean onNextImPress  (GtkToolButton *, gpointer);         // "button-press-event"
gboolean onPrevImPress  (GtkToolButton *, gpointer);         // "button-press-event"

typedef gpointer * ggpointer;

/*----------------------------------------------------------------------- */
void 
_showInGtkWindow (SEXP x) {
    int nx, ny, nz, width, height;
    SEXP dim;
    Image * images;
    GdkPixbuf * pxbuf;
    GtkWidget * imgWG, * winWG, * vboxWG, * tbarWG, * scrollWG, 
              * btnZoomInWG, * btnZoomOutWG, * btnZoomOneWG,
              * btnNextWG, * btnPrevWG;
    GtkIconSize iSize;
    gpointer ** winStr; /* 4 pointers, 0 - window, 1 - imageWG, 2 - images, *int - index of current image on display */
    
    if ( !GTK_OK )
        error ( _("failed to initialize GTK+, use 'read.image' instead") );

    /* get image in magick format and get image size */
    images = sexp2Magick (x);
    dim = GET_DIM (x);
    nx = INTEGER (dim)[0];
    ny = INTEGER (dim)[1];
    nz = INTEGER (dim)[2];

    /* create pixbuf from image data */    
    pxbuf = newPixbufFromImages (images, 0);
    if ( pxbuf == NULL )
        error ( _("cannot copy image data to display window") );

    /* create window structure */
    winStr = g_new ( ggpointer, 4 );
    winStr[3] = (gpointer *) g_new0 (int, 1);
    winStr[2] = (gpointer *) images;
    
    /* create image display */
    imgWG = gtk_image_new_from_pixbuf (pxbuf);
    winStr[1] = (gpointer *) imgWG;
    g_object_unref (pxbuf);
    /* create main window */
    winWG =  gtk_window_new (GTK_WINDOW_TOPLEVEL);   
    winStr[0] = (gpointer *) winWG;
    gtk_window_set_title ( GTK_WINDOW(winWG), _("R image display") );
    /* set destroy event handler for the window */
    g_signal_connect ( G_OBJECT(winWG), "delete-event", G_CALLBACK(onWinDestroy), winStr );

    /* create controls and set event handlers */
    /* create general horizontal lyout with a toolbar and add it to the window */
    vboxWG = gtk_vbox_new (FALSE, 0);
    gtk_container_add ( GTK_CONTAINER(winWG), vboxWG);
    /* create toolbar and push it to layout */
    tbarWG = gtk_toolbar_new ();
    gtk_box_pack_start ( GTK_BOX(vboxWG), tbarWG, FALSE, FALSE, 0);
    /* create scrollbox that occupies and extends and push it to layout */
    scrollWG = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start ( GTK_BOX(vboxWG), scrollWG, TRUE, TRUE, 5);
    gtk_scrolled_window_set_policy ( GTK_SCROLLED_WINDOW(scrollWG), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    /* add image to scroll */
    gtk_scrolled_window_add_with_viewport ( GTK_SCROLLED_WINDOW(scrollWG), imgWG);
    
    /* add zoom buttons */
    iSize = gtk_toolbar_get_icon_size ( GTK_TOOLBAR(tbarWG) );
    btnZoomInWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-zoom-in", iSize), _("Zoom in") );
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomInWG);
    g_signal_connect ( G_OBJECT(btnZoomInWG), "clicked", G_CALLBACK(onZoomInPress), winStr);
    btnZoomOutWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-zoom-out", iSize), _("Zoom out") );
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomOutWG);
    g_signal_connect ( G_OBJECT(btnZoomOutWG), "clicked", G_CALLBACK(onZoomOutPress), winStr);
    btnZoomOneWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-yes", iSize), "1:1");
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomOneWG);
    g_signal_connect ( G_OBJECT(btnZoomOneWG), "clicked", G_CALLBACK(onZoomOnePress), winStr);

    /* add browsing buttons */
    if ( nz > 1 ) {
        btnPrevWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-go-back", iSize), _("Previous") );
        gtk_container_add ( GTK_CONTAINER(tbarWG), btnPrevWG);
        g_signal_connect ( G_OBJECT(btnPrevWG), "clicked", G_CALLBACK(onPrevImPress), winStr);
        btnNextWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-go-forward", iSize), _("Next") );
        gtk_container_add ( GTK_CONTAINER(tbarWG), btnNextWG);
        g_signal_connect ( G_OBJECT(btnNextWG), "clicked", G_CALLBACK(onNextImPress), winStr);
    }
    /* resize to fit image */
    width = gdk_screen_get_width ( gdk_screen_get_default() );
    height = gdk_screen_get_height ( gdk_screen_get_default () );
    width = ( nx + 20 < width - 20 ) ? ( nx + 20 ) : ( width - 20 );
    height = ( ny + 80 < height - 20 ) ? ( ny + 80 ) : ( height - 20 );
    if ( width < 150 ) width = 150;
    if ( height < 100 ) height = 100;
    gtk_window_resize ( GTK_WINDOW(winWG), width, height);

    /* show window */
    gtk_widget_show_all (winWG);
    gdk_flush();
}

/*----------------------------------------------------------------------- */
gboolean
onWinDestroy (GtkWidget * wnd, GdkEvent * event, gpointer ptr) {
    gpointer ** winStr;
    Image * images;
    winStr = (gpointer **) ptr;
    g_free (winStr[3]);
    images = (Image *) winStr[2];
    g_free (winStr);
    images = DestroyImageList (images);
/*    Rprintf ( _("R image display closed...\n") ); */
    return FALSE;
}

/*----------------------------------------------------------------------- */
gboolean 
onZoomInPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, index;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    Image * images;
    
    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromImages (images, index );

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width * 1.25), (int)(height * 1.25), GDK_INTERP_BILINEAR );
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
gboolean
onZoomOutPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, index;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    Image * images;
    
    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromImages (images, index );

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width * 0.75), (int)(height * 0.75), GDK_INTERP_BILINEAR );
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
gboolean
onZoomOnePress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    GdkPixbuf * pxbuf;
    GtkImage * imgWG;
    Image * images;
    int index;
    
    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromImages (images, index );

    gtk_image_set_from_pixbuf (imgWG, pxbuf);
    g_object_unref (pxbuf);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
gboolean 
onNextImPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, nz, index;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    Image * images;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );
    
    nz = GetImageListLength (images);
    index = *(int *)winStr[3] + 1;
    if ( index == nz ) 
        return TRUE;
    pxbuf = newPixbufFromImages (images, index);
    *(int *)winStr[3] = index;

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, width, height, GDK_INTERP_BILINEAR );
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    gdk_flush();
    return TRUE;
}
/*----------------------------------------------------------------------- */
gboolean 
onPrevImPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, nz, index;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    Image * images;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );
    
    nz = GetImageListLength (images);
    index = *(int *)winStr[3] - 1;
    if ( index < 0 ) 
        return TRUE;
    pxbuf = newPixbufFromImages (images, index);
    *(int *)winStr[3] = index;

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, width, height, GDK_INTERP_BILINEAR );
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    gdk_flush();
    return TRUE;
}

#endif

