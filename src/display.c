#include "display.h"

/* -------------------------------------------------------------------------
Image conversions between MagickCore and R
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"

#include <stdio.h>

#include <R_ext/Memory.h>
#include <R_ext/Error.h>
#include <magick/ImageMagick.h>

#ifndef WIN32
#   include <pthread.h>
#endif

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

int THREAD_ON = 0;

/*----------------------------------------------------------------------- */
void * _showInImageMagickWindow (void *);
void * _animateInImageMagickWindow (void *);
#ifdef USE_GTK
void _showInGtkWindow (SEXP, SEXP);
GdkPixbuf * newPixbufFromImages (Image *, int);
#endif
/*----------------------------------------------------------------------- */
SEXP
lib_display(SEXP x, SEXP caption, SEXP nogtk) {
#ifndef WIN32
    pthread_t res;
#endif

    if ( !isImage(x) )
        error ( "argument must be of class 'Image'" );

#ifdef USE_GTK
    if ( !LOGICAL(nogtk)[0] ) {
        if ( GTK_OK )
            _showInGtkWindow (x, caption);
        else
            error ( "GTK+ was not properly initialised" );
        return R_NilValue;
    }
#endif

#ifdef WIN32
    error ( "only GTK+ display is awailable on Windows" );
#else
    if ( THREAD_ON )
        error ( "cannot display concurent windows. Close currently displayed window first." );
    if ( pthread_create(&res, NULL, _showInImageMagickWindow, (void *)x ) != 0 )
        error ( "cannot create display thread" );
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
        error ( "argument must be of class 'Image'" );

#ifdef WIN32
    error ( "animate function is not available on Windows because it uses ImageMagick interactive display" );
#else
    if ( THREAD_ON )
        error ( "cannot display concurent windows. Close currently displayed window first." );
    if ( pthread_create(&res, NULL, _animateInImageMagickWindow, (void *)x ) != 0 )
        error ( "cannot animate display thread" );
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
gboolean onMouseMove    (GtkWidget *, GdkEventMotion *, gpointer); // "motion-notify-event"
void     updateStatusBar(GtkStatusbar * stbarWG, double * stats);

typedef gpointer * ggpointer;

/*----------------------------------------------------------------------- */
void
_showInGtkWindow (SEXP x, SEXP caption) {
    int nx, ny, nz, width, height;
    double * stats;    /* 0: nx, 1: ny, 2: nz, 3: x, 4: y, 5: zoom, 6: index */
    SEXP dim;
    Image * images;
    GdkPixbuf * pxbuf;
    GtkWidget * imgWG, * evBox, * winWG, * vboxWG, * tbarWG, * stbarWG, * scrollWG,
              * btnZoomInWG, * btnZoomOutWG, * btnZoomOneWG,
              * btnNextWG, * btnPrevWG;
    GtkIconSize iSize;
    gpointer ** winStr; /* 5 pointers, 0 - window, 1 - imageWG, 2 - images, 3 - *int - index of current image on display,
                                       4 - statusbar, 5 - pointer to doubles of status bar values  */

    if ( !GTK_OK )
        error ( "failed to initialize GTK+, use 'read.image' instead" );

    /* get image in magick format and get image size */
    images = sexp2Magick (x);
    dim = GET_DIM (x);
    nx = INTEGER (dim)[0];
    ny = INTEGER (dim)[1];
    nz = INTEGER (dim)[2];

    /* create pixbuf from image data */
    pxbuf = newPixbufFromImages (images, 0);
    if ( pxbuf == NULL )
        error ( "cannot copy image data to display window" );

    /* create window structure */
    winStr = g_new ( ggpointer, 6 );
    winStr[3] = (gpointer *) g_new0 (int, 1);
    winStr[2] = (gpointer *) images;

    /* create image display */
    imgWG = gtk_image_new_from_pixbuf (pxbuf);
//    gtk_misc_set_alignment(GTK_MISC(imgWG),0.0,0.0);
    
    winStr[1] = (gpointer *) imgWG;
    g_object_unref (pxbuf);
    /* create main window */
    winWG =  gtk_window_new (GTK_WINDOW_TOPLEVEL);
    winStr[0] = (gpointer *) winWG;
    if ( caption != R_NilValue )
      gtk_window_set_title ( GTK_WINDOW(winWG), CHAR( asChar(caption) ) );
    else
      gtk_window_set_title ( GTK_WINDOW(winWG), "R image display" );
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
    /* add image to event box */
    evBox = gtk_event_box_new();
    gtk_container_add(GTK_CONTAINER(evBox), imgWG);
    /* add image to scroll */
    gtk_scrolled_window_add_with_viewport ( GTK_SCROLLED_WINDOW(scrollWG), evBox);
    
    
    /* create status bar and push it to layout */
    stbarWG = gtk_statusbar_new ();
    gtk_box_pack_start ( GTK_BOX(vboxWG), stbarWG, FALSE, FALSE, 0);
    stats = g_new ( gdouble, 7 );
    stats[0] = nx;
    stats[1] = ny;
    stats[2] = nz;
    stats[3] = 0; /* x coord */
    stats[4] = 0; /* y coord */
    stats[5] = 1; /* zoom */
    stats[6] = 0; /* index */
    winStr[4] = (gpointer *) stbarWG;
    winStr[5] = (gpointer *) stats;
    /* add zoom buttons */
    iSize = gtk_toolbar_get_icon_size ( GTK_TOOLBAR(tbarWG) );
    btnZoomInWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-zoom-in", iSize), "Zoom in" );
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomInWG);
    g_signal_connect ( G_OBJECT(btnZoomInWG), "clicked", G_CALLBACK(onZoomInPress), winStr);
    btnZoomOutWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-zoom-out", iSize), "Zoom out" );
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomOutWG);
    g_signal_connect ( G_OBJECT(btnZoomOutWG), "clicked", G_CALLBACK(onZoomOutPress), winStr);
    btnZoomOneWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-yes", iSize), "1:1");
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomOneWG);
    g_signal_connect ( G_OBJECT(btnZoomOneWG), "clicked", G_CALLBACK(onZoomOnePress), winStr);

    /* add browsing buttons */
    if ( nz > 1 ) {
        btnPrevWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-go-back", iSize), "Previous" );
        gtk_container_add ( GTK_CONTAINER(tbarWG), btnPrevWG);
        g_signal_connect ( G_OBJECT(btnPrevWG), "clicked", G_CALLBACK(onPrevImPress), winStr);
        btnNextWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-go-forward", iSize), "Next" );
        gtk_container_add ( GTK_CONTAINER(tbarWG), btnNextWG);
        g_signal_connect ( G_OBJECT(btnNextWG), "clicked", G_CALLBACK(onNextImPress), winStr);
    }

    gtk_signal_connect( GTK_OBJECT(evBox), "motion-notify-event", GTK_SIGNAL_FUNC(onMouseMove), winStr);
    gtk_widget_set_events(evBox, GDK_POINTER_MOTION_MASK ); // GDK_BUTTON_PRESS_MASK | 
    
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
    updateStatusBar((GtkStatusbar *)stbarWG, stats);
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
    g_free (winStr[5]); /* stats */
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
    double * stats;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    Image * images;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromImages (images, index );
    stats = (double *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width * 1.25), (int)(height * 1.25), GDK_INTERP_BILINEAR );
    stats[5] *= 1.25;
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
gboolean
onZoomOutPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, index;
    double * stats;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    Image * images;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromImages (images, index );
    stats = (double *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width * 0.75), (int)(height * 0.75), GDK_INTERP_BILINEAR );
    stats[5] *= 0.75;
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
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
    double * stats;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromImages (images, index );
    stats = (double *)winStr[5];

    gtk_image_set_from_pixbuf (imgWG, pxbuf);
    stats[5] = 1.0;
    g_object_unref (pxbuf);
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
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
    double * stats;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    stats = (double *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    nz = GetImageListLength (images);
    index = *(int *)winStr[3] + 1;
    if ( index == nz )
        return TRUE;
    pxbuf = newPixbufFromImages (images, index);
    *(int *)winStr[3] = index;
    stats[6] = index;

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, width, height, GDK_INTERP_BILINEAR );
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
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
    double * stats;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    images = (Image *) winStr[2];
    stats = (double *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    nz = GetImageListLength (images);
    index = *(int *)winStr[3] - 1;
    if ( index < 0 )
        return TRUE;
    pxbuf = newPixbufFromImages (images, index);
    *(int *)winStr[3] = index;
    stats[6] = index;

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, width, height, GDK_INTERP_BILINEAR );
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
    g_object_unref (pxbuf);
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
gboolean 
onMouseMove(GtkWidget * widget, GdkEventMotion * event, gpointer ptr) {
    gpointer ** winStr;
    double * stats;

    winStr = (gpointer **) ptr;
    GtkWidget * imgWG = GTK_WIDGET(winStr[1]);
    stats = (double *)winStr[5];
    gint x, y, x0=0, y0=0;

    x0 = (imgWG->allocation.width - stats[0]*stats[5])/2;
    if (x0 < 0) x0 = 0;
    y0 = (imgWG->allocation.height - stats[1]*stats[5])/2;
    if (y0 < 0) y0 = 0;

    gtk_widget_get_pointer(imgWG, &x, &y);
    stats[3] = (x-x0) / stats[5] + 1;
    stats[4] = (y-y0) / stats[5] + 1;
    if (stats[3]<1) stats[3] = 1;
    if (stats[4]<1) stats[4] = 1;
    if (stats[3]>stats[0]) stats[3] = stats[0];
    if (stats[4]>stats[1]) stats[4] = stats[1];
    
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
GdkPixbuf * 
newPixbufFromImages (Image * images, int index) {
    GdkPixbuf * res;
    Image * image;
    int nx, ny;
    ExceptionInfo exception;

    if ( images == NULL )
        return NULL;
    res = NULL;
    image = GetImageFromList (images, index);
    nx = image->columns;
    ny = image->rows;
    GetExceptionInfo(&exception);
    res = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, nx, ny);
    if ( GetImageType(images, &exception) == GrayscaleType )
        DispatchImage (image, 0, 0, nx, ny, "IIIA", CharPixel, gdk_pixbuf_get_pixels(res), &exception);
    else
        DispatchImage (image, 0, 0, nx, ny, "RGBA", CharPixel, gdk_pixbuf_get_pixels(res), &exception);
    if (exception.severity != UndefinedException) {
        CatchException (&exception);
        g_object_unref (res);
        res = NULL;
    }

    DestroyExceptionInfo(&exception);
    return res;
}

/*----------------------------------------------------------------------- */
void updateStatusBar(GtkStatusbar * stbarWG, double * stats) {
  gchar str[255];
  /* 0: nx, 1: ny, 2: nz, 3: x, 4: y, 5: zoom, 6: index */
  sprintf(str, "Frame: %d/%d\tImage: %dx%dx%d\tZoom: %d%%\t Position: (%d:%d)", 
    (int)(stats[6]+1), (int)stats[2], (int)stats[0], (int)stats[1], (int)stats[2], (int)(stats[5]*100), (int)stats[3], (int)stats[4]); 
/*  sprintf(str, "Frame: %d/%d    Image: %dx%dx%d    Zoom: %d%%", 
    (int)(stats[6]+1), (int)stats[2], (int)stats[0], (int)stats[1], (int)stats[2], (int)(stats[5]*100));
*/
  gtk_statusbar_pop(stbarWG, 0);
  gtk_statusbar_push(stbarWG, 0, str);
}

#endif

