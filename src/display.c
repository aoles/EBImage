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

typedef struct {
  double nx,ny,nz;
  double x,y;
  double zoom;
  double index;
} dstats;

#define INTERP_METHOD GDK_INTERP_NEAREST
int THREAD_ON = 0;

/*----------------------------------------------------------------------- */
void * _showInImageMagickWindow (void *);
void * _animateInImageMagickWindow (void *);
#ifdef USE_GTK
void _showInGtkWindow (SEXP, SEXP);
GdkPixbuf *newPixbufFromSEXP (SEXP x, int index);
#endif
/*----------------------------------------------------------------------- */
SEXP
lib_display(SEXP x, SEXP caption, SEXP useGTK) {
#ifndef WIN32
    pthread_t res;
#endif

    validImage(x,0);

#ifdef USE_GTK
    if ( LOGICAL(useGTK)[0] ) {
        if ( GTK_OK )
            _showInGtkWindow (x, caption);
        else
            error ( "GTK+ was not properly initialised" );
        return R_NilValue;
    }
#endif

#ifdef WIN32
    error ( "ImageMagick 'display' is not available on Windows" );
#else
    if ( THREAD_ON )
        error ( "Cannot display multiple windows. Please close the currently displayed window first." );
    if ( pthread_create(&res, NULL, _showInImageMagickWindow, (void *)x ) != 0 )
        error ( "Failed to create 'display' thread" );
#endif
    return R_NilValue;
}

/*----------------------------------------------------------------------- */
SEXP
lib_animate (SEXP x) {
#ifndef WIN32
    pthread_t res;
#endif

    validImage(x,0);

#ifdef WIN32
    error ( "ImageMagick 'animate' is not available on Windows." );
#else
    if ( THREAD_ON )
        error ( "Cannot display multiple windows. Please close the currently displayed window first." );
    if ( pthread_create(&res, NULL, _animateInImageMagickWindow, (void *)x ) != 0 )
        error ( "Failed to create 'animate' thread" );
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
gboolean onScroll       (GtkAdjustment *adjustment, gpointer ptr) ;  // "value-changed"
gboolean onMouseMove    (GtkWidget *, GdkEventMotion *, gpointer); // "motion-notify-event"
void     updateStatusBar(GtkStatusbar * stbarWG, dstats * stats);

typedef gpointer * ggpointer;

/*----------------------------------------------------------------------- */
void
_showInGtkWindow (SEXP xx, SEXP caption) {
    int nx, ny, nz, width, height;
    dstats *stats;
    SEXP dim;
    GdkPixbuf * pxbuf;
    GtkWidget * imgWG, * evBox, * winWG, * vboxWG, * tbarWG, * stbarWG, * scrollWG,
              * btnZoomInWG, * btnZoomOutWG, * btnZoomOneWG,
              * btnNextWG, * btnPrevWG;
    GtkIconSize iSize;
    gpointer ** winStr; /* 6 pointers, 0 - window, 1 - imageWG, 2 - images, 3 - *int - index of current image on display,
                                       4 - statusbar, 5 - pointer to dstats  */

    if ( !GTK_OK )
        error ( "failed to initialize GTK+, use 'read.image' instead" );

    dim = GET_DIM (xx);
    nx = INTEGER (dim)[0];
    ny = INTEGER (dim)[1];
    nz = getNumberOfFrames(xx,1);

    // xx is preserved from garbage collection til the windows is closed
    R_PreserveObject(xx);

    /* create pixbuf from image data */
    pxbuf=newPixbufFromSEXP(xx,0);

    if ( pxbuf == NULL )
        error ( "cannot copy image data to display window" );

    /* create window structure */
    winStr = g_new ( ggpointer, 6 );
    winStr[3] = (gpointer *) g_new0 (int, 1);
    winStr[2] = (gpointer *) xx;

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
    gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrollWG))),"value-changed", GTK_SIGNAL_FUNC(onScroll), winStr);
    gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(scrollWG))),"value-changed", GTK_SIGNAL_FUNC(onScroll), winStr);

    /* create status bar and push it to layout */
    stbarWG = gtk_statusbar_new ();
    gtk_box_pack_start ( GTK_BOX(vboxWG), stbarWG, FALSE, FALSE, 0);

    stats = g_new ( dstats, 1 );
    stats->nx=nx;
    stats->ny=ny;
    stats->nz=nz;
    stats->x=0;
    stats->y=0;
    stats->zoom=1.0;
    stats->index=0;

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
    SEXP xx;
    //Image * images;
    winStr = (gpointer **) ptr;
    g_free (winStr[3]);
    //images = (Image *) winStr[2];
    xx=(SEXP)winStr[2];
    R_ReleaseObject(xx);
    g_free (winStr[5]); /* stats */
    g_free (winStr);
    //images = DestroyImageList (images);
/*    Rprintf ( _("R image display closed...\n") ); */
    return FALSE;
}

/*----------------------------------------------------------------------- */
gboolean
onZoomInPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, index;
    dstats * stats;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    SEXP xx;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    xx = (SEXP) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromSEXP (xx, index );
    stats = (dstats *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );
    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width *2), (int)(height * 2), INTERP_METHOD);
    stats->zoom *= 2;
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
    dstats * stats;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    SEXP xx;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    xx = (SEXP ) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromSEXP (xx, index );
    stats = (dstats *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );
    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width/2), (int)(height/2), INTERP_METHOD);
    stats->zoom /= 2;

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
    SEXP xx;
    int index;
    dstats * stats;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    xx = (SEXP) winStr[2];
    index = *(int *)winStr[3];
    pxbuf = newPixbufFromSEXP (xx, index );
    stats = (dstats *)winStr[5];

    gtk_image_set_from_pixbuf (imgWG, pxbuf);
    stats->zoom = 1.0;
    g_object_unref (pxbuf);
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
gboolean onScroll(GtkAdjustment *adjustment, gpointer ptr)
{
  return onMouseMove(NULL,NULL,ptr);
}

/*----------------------------------------------------------------------- */
gboolean
onNextImPress (GtkToolButton * btn, gpointer ptr) {
    gpointer ** winStr;
    int width, height, nz, index;
    GdkPixbuf * pxbuf, * newPxbuf;
    GtkImage * imgWG;
    SEXP xx;
    dstats * stats;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    xx = (SEXP) winStr[2];
    stats = (dstats *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    nz = getNumberOfFrames(xx,1);
    index = *(int *)winStr[3] + 1;
    if ( index == nz )
        return TRUE;
    pxbuf = newPixbufFromSEXP (xx, index);
    *(int *)winStr[3] = index;
    stats->index = index;

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, width, height, INTERP_METHOD);
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
    SEXP xx;
    dstats * stats;

    winStr = (gpointer **) ptr;
    imgWG = GTK_IMAGE (winStr[1]);
    xx = (SEXP) winStr[2];
    stats = (dstats *)winStr[5];

    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );

    nz = getNumberOfFrames(xx,1);
    index = *(int *)winStr[3] - 1;
    if ( index < 0 )
        return TRUE;
    pxbuf = newPixbufFromSEXP (xx, index);
    *(int *)winStr[3] = index;
    stats->index = index;

    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, width, height, INTERP_METHOD);
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
    dstats * stats;

    winStr = (gpointer **) ptr;
    GtkWidget * imgWG = GTK_WIDGET(winStr[1]);
    stats = (dstats *)winStr[5];
    gint x, y, x0=0, y0=0;

    x0 = (imgWG->allocation.width - stats->nx*stats->zoom)/2;
    if (x0 < 0) x0 = 0;
    y0 = (imgWG->allocation.height - stats->ny*stats->zoom)/2;
    if (y0 < 0) y0 = 0;

    gtk_widget_get_pointer(imgWG, &x, &y);
    stats->x = (x-x0) / stats->zoom + 1;
    stats->y = (y-y0) / stats->zoom + 1;

    if (stats->x<1) stats->x = 1;
    if (stats->y<1) stats->y = 1;
    if (stats->x>stats->nx) stats->x = stats->nx;
    if (stats->y>stats->ny) stats->y = stats->ny;
    
    updateStatusBar((GtkStatusbar *)winStr[4], stats);
    gdk_flush();
    return TRUE;
}

/*----------------------------------------------------------------------- */
GdkPixbuf * 
newPixbufFromSEXP (SEXP xx, int index) {
  GdkPixbuf * pixbuf;
  int width, height, rowstride, x, y;
  numeric *dx,dr,dg,db;
  unsigned char *dpixbuf,*data;
  int colorMode;
  int *ix,*idata;
  int redstride=-1,greenstride=-1,bluestride=-1;

  width=INTEGER(GET_DIM(xx))[0];
  height=INTEGER(GET_DIM(xx))[1];
   
  colorMode=getColorMode(xx);

  pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, width, height);
  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  dpixbuf=(unsigned char *)gdk_pixbuf_get_pixels(pixbuf);

  // TrueColor
  if (colorMode==MODE_TRUECOLOR) {
    ix=INTEGER(xx);
    for (y=0;y<height;y++) {
      idata=(int *)(dpixbuf+y*rowstride);
      for (x=0;x<width;x++) {
	*idata=ix[x+y*width+index*width*height] | 0xff000000;
	idata++;
      }
    }
  } else {
    dx=REAL(xx);
    
    getColorStrides(xx,index,&redstride,&greenstride,&bluestride);

    for (y=0;y<height;y++) {
      data=dpixbuf+y*rowstride;
      for (x=0;x<width;x++) {
	if (redstride!=-1) dr=256*dx[x+y*width+redstride];	
	else dr=0;
	if (greenstride!=-1) dg=256*dx[x+y*width+greenstride];	
	else dg=0;
	if (bluestride!=-1) db=256*dx[x+y*width+bluestride];
	else db=0;
	if (dr<0.0) dr=0.0;
	if (dr>255.0) dr=255.0;
	if (dg<0.0) dg=0.0;
	if (dg>255.0) dg=255.0;
	if (db<0.0) db=0.0;
	if (db>255.0) db=255.0;
	*data++=(unsigned char)dr;   // R
	*data++=(unsigned char)dg;   // G
	*data++=(unsigned char)db;   // B
	*data++=255;                 // A
      }
    }
  } 
    
  return pixbuf;
}

/*----------------------------------------------------------------------- */
void updateStatusBar(GtkStatusbar * stbarWG, dstats * stats) {
  gchar str[255];

  /* 0: nx, 1: ny, 2: nz, 3: x, 4: y, 5: zoom, 6: index */
  sprintf(str, "Frame: %d/%d\tImage: %dx%dx%d\tZoom: %d%%\t Position: (%d,%d)", 
	  (int)(stats->index+1), (int)stats->nz, (int)stats->nx, (int)stats->ny, (int)stats->nz, (int)(stats->zoom*100), (int)stats->x, (int)stats->y); 

  gtk_statusbar_pop(stbarWG, 0);
  gtk_statusbar_push(stbarWG, 0, str);
}

#endif

