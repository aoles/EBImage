#include "display.h"
#include "tools.h"
#include "conversions.h"

#include <stdio.h>
#include <R_ext/Memory.h>
#include <R_ext/Error.h>
#include <magick/ImageMagick.h>

#ifndef WIN32
#   include <pthread.h>
#endif

// GTK includes
#ifdef USE_GTK
  #include <gtk/gtk.h>
  #ifdef WIN32
    typedef unsigned long ulong;
    #include <sys/types.h>
  #else
    #include <gdk/gdkx.h>
  #endif
#endif

int THREAD_ON = 0;

// C preferred way to get rid of 'unused parameter' warnings
#define UNUSED(expr) do { (void)(expr); } while (0)

void * _showInImageMagickWindow (void *);
void * _animateInImageMagickWindow (void *);

// GTK declarations
#ifdef USE_GTK
  void _showInGtkWindow (SEXP, SEXP);
  GdkPixbuf *newPixbufFromSEXP (SEXP x, int index);

  typedef struct {
    double nx,ny,nz;
    double x,y;
    double zoom;
    GtkWidget *hSlider;
    GtkWidget *imgWG;
    GtkWidget *stbarWG;
    int index;
    SEXP xx;
  } udata;

  #define INTERP_METHOD GDK_INTERP_NEAREST

#endif

SEXP lib_display(SEXP x, SEXP caption, SEXP useGTK) {
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

SEXP lib_animate (SEXP x) {
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

void *_showInImageMagickWindow (void * ptr) {
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

void *_animateInImageMagickWindow (void * ptr) {
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
gboolean onSlide(GtkRange *range, gpointer user_data);
void updateStatusBar(gpointer user_data);
void updateFrame(gpointer user_data,double factor);


void
_showInGtkWindow (SEXP xx, SEXP caption) {
    int nx, ny, nz, width, height;
    udata *dat;
    SEXP dim;
    GdkPixbuf * pxbuf;
    GtkWidget *evBox, *winWG, *vboxWG, *tbarWG, *scrollWG,
      *btnZoomInWG, *btnZoomOutWG, *btnZoomOneWG,
      *btnNextWG, *btnPrevWG;
    GtkObject *hAdjustment;
    GtkIconSize iSize;
    if ( !GTK_OK )
        error ( "failed to initialize GTK+, use 'read.image' instead" );

    dim = GET_DIM (xx);
    nx = INTEGER (dim)[0];
    ny = INTEGER (dim)[1];
    nz = getNumberOfFrames(xx,1);

    dat=g_new(udata,1);
    dat->nx=nx;
    dat->ny=ny;
    dat->nz=nz;
    dat->x=0;
    dat->y=0;
    dat->zoom=1.0;
    dat->index=0;
    dat->hSlider=NULL;
    dat->xx=xx;
   
    // xx is preserved from garbage collection til the windows is closed
    R_PreserveObject(xx);

    /* create pixbuf from image data */
    pxbuf=newPixbufFromSEXP(xx,0);

    if ( pxbuf == NULL )
        error ( "cannot copy image data to display window" );

    /* create imae display */
    dat->imgWG = gtk_image_new_from_pixbuf (pxbuf);
    g_object_unref (pxbuf);

    /* create main window */
    winWG =  gtk_window_new (GTK_WINDOW_TOPLEVEL);
    if ( caption != R_NilValue )
      gtk_window_set_title ( GTK_WINDOW(winWG), CHAR( asChar(caption) ) );
    else
      gtk_window_set_title ( GTK_WINDOW(winWG), "R image display" );
    /* set destroy event handler for the window */
    g_signal_connect ( G_OBJECT(winWG), "delete-event", G_CALLBACK(onWinDestroy), dat);

    /* create controls and set event handlers */
    /* create general horizontal lyout with a toolbar and add it to the window */
    vboxWG = gtk_vbox_new (FALSE, 0);
    gtk_container_add ( GTK_CONTAINER(winWG), vboxWG);

    /* create toolbar and push it to layout */
    tbarWG = gtk_toolbar_new ();
    gtk_box_pack_start ( GTK_BOX(vboxWG), tbarWG, FALSE, FALSE, 0);

    // add a horizontal slider
    if (nz>1) {
      hAdjustment=gtk_adjustment_new(1,1,nz,1,1,0);
      dat->hSlider=gtk_hscale_new(GTK_ADJUSTMENT(hAdjustment));
      gtk_scale_set_digits(GTK_SCALE(dat->hSlider),0);
      gtk_box_pack_start(GTK_BOX(vboxWG), dat->hSlider, FALSE,FALSE, 0);
      gtk_signal_connect(GTK_OBJECT(dat->hSlider),"value-changed", GTK_SIGNAL_FUNC(onSlide), dat);
    }

    /* create scrollbox that occupies and extends and push it to layout */
    scrollWG = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start ( GTK_BOX(vboxWG), scrollWG, TRUE, TRUE, 5);
    gtk_scrolled_window_set_policy ( GTK_SCROLLED_WINDOW(scrollWG), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    /* add image to event box */
    evBox = gtk_event_box_new();
    gtk_container_add(GTK_CONTAINER(evBox), dat->imgWG);
    /* add image to scroll */
    gtk_scrolled_window_add_with_viewport ( GTK_SCROLLED_WINDOW(scrollWG), evBox);
    gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(scrollWG))),"value-changed", GTK_SIGNAL_FUNC(onScroll), dat);
    gtk_signal_connect(GTK_OBJECT(gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(scrollWG))),"value-changed", GTK_SIGNAL_FUNC(onScroll), dat);
    
    /* create status bar and push it to layout */
    dat->stbarWG = gtk_statusbar_new ();
    gtk_box_pack_start ( GTK_BOX(vboxWG), dat->stbarWG, FALSE, FALSE, 0);

    /* add zoom buttons */
    iSize = gtk_toolbar_get_icon_size ( GTK_TOOLBAR(tbarWG) );
    btnZoomInWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-zoom-in", iSize), "Zoom in" );
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomInWG);
    g_signal_connect ( G_OBJECT(btnZoomInWG), "clicked", G_CALLBACK(onZoomInPress), dat);
    btnZoomOutWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-zoom-out", iSize), "Zoom out" );
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomOutWG);
    g_signal_connect ( G_OBJECT(btnZoomOutWG), "clicked", G_CALLBACK(onZoomOutPress), dat);
    btnZoomOneWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-yes", iSize), "1:1");
    gtk_container_add ( GTK_CONTAINER(tbarWG), btnZoomOneWG);
    g_signal_connect ( G_OBJECT(btnZoomOneWG), "clicked", G_CALLBACK(onZoomOnePress), dat);

    /* add browsing buttons */
    if ( nz > 1 ) {
        btnPrevWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-go-back", iSize), "Previous" );
        gtk_container_add ( GTK_CONTAINER(tbarWG), btnPrevWG);
        g_signal_connect ( G_OBJECT(btnPrevWG), "clicked", G_CALLBACK(onPrevImPress), dat);
        btnNextWG = (GtkWidget *) gtk_tool_button_new ( gtk_image_new_from_stock("gtk-go-forward", iSize), "Next" );
        gtk_container_add ( GTK_CONTAINER(tbarWG), btnNextWG);
        g_signal_connect ( G_OBJECT(btnNextWG), "clicked", G_CALLBACK(onNextImPress), dat);
    }
    
    gtk_signal_connect( GTK_OBJECT(evBox), "motion-notify-event", GTK_SIGNAL_FUNC(onMouseMove), dat);
    gtk_widget_set_events(evBox, GDK_BUTTON_PRESS_MASK | GDK_POINTER_MOTION_MASK );
    
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
    updateStatusBar(dat);
    gdk_flush();
}

gboolean onWinDestroy (GtkWidget * wnd, GdkEvent * event, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(wnd);
  UNUSED(event);

  R_ReleaseObject(dat->xx);
  g_free (dat);
  return FALSE;
}

gboolean onSlide(GtkRange *range, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(range);

  dat->index=gtk_range_get_value (GTK_RANGE(dat->hSlider))-1;
  updateFrame(user_data,1);
  updateStatusBar(user_data);
  return TRUE;
}

// factor = -1 resets the size
void updateFrame(gpointer user_data,double factor) {
  udata *dat=(udata *)user_data;
  GtkImage *imgWG=GTK_IMAGE(dat->imgWG);
  SEXP xx=dat->xx;
  int width, height;
  GdkPixbuf * pxbuf, * newPxbuf;    
  
  pxbuf = newPixbufFromSEXP (xx, dat->index);
  if (factor==-1) {
    gtk_image_set_from_pixbuf (imgWG, pxbuf);
  } else { 
    width = gdk_pixbuf_get_width ( gtk_image_get_pixbuf(imgWG) );
    height = gdk_pixbuf_get_height ( gtk_image_get_pixbuf(imgWG) );
    newPxbuf = gdk_pixbuf_scale_simple ( pxbuf, (int)(width*factor), (int)(height*factor), INTERP_METHOD);
    gtk_image_set_from_pixbuf (imgWG, newPxbuf);
    g_object_unref (newPxbuf);
  }
  
  g_object_unref (pxbuf);
  gdk_flush();
}

gboolean onZoomInPress (GtkToolButton * btn, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(btn);

  dat->zoom *= 2;
  updateFrame(user_data,2);
  updateStatusBar(user_data);
  return TRUE;
}

gboolean onZoomOutPress (GtkToolButton * btn, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(btn);

  dat->zoom *= 0.5;
  updateFrame(user_data,0.5);
  updateStatusBar(user_data);
  return TRUE;
}

gboolean onZoomOnePress (GtkToolButton * btn, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(btn);

  dat->zoom=1.0;
  updateFrame(user_data,-1);
  updateStatusBar(user_data);
  return TRUE;
}

gboolean onScroll(GtkAdjustment *adjustment, gpointer user_data)
{
  UNUSED(adjustment);

  return onMouseMove(NULL,NULL,user_data);
}

gboolean onNextImPress (GtkToolButton * btn, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(btn);

  dat->index=dat->index+1;
  if (dat->index>=dat->nz-1) dat->index=(dat->nz)-1;
  if (dat->hSlider!=NULL) {
    gtk_range_set_value (GTK_RANGE(dat->hSlider),(double)(dat->index+1));
  }
  updateFrame(user_data,1);
  updateStatusBar(user_data);
  return TRUE;
}

gboolean onPrevImPress (GtkToolButton * btn, gpointer user_data) {
  udata *dat=(udata *)user_data;
  UNUSED(btn);

  dat->index=dat->index-1;
  if (dat->index<0) dat->index=0;
  if (dat->hSlider!=NULL) {
    gtk_range_set_value (GTK_RANGE(dat->hSlider),(double)(dat->index+1));
  }
  updateFrame(user_data,1);
  updateStatusBar(user_data);
  return TRUE;
}

gboolean onMouseMove(GtkWidget * widget, GdkEventMotion * event, gpointer user_data) {
  udata *dat=(udata *)user_data;
  gint x, y, x0=0, y0=0;
  UNUSED(widget);
  UNUSED(event);

  x0 = (dat->imgWG->allocation.width - dat->nx*dat->zoom)/2;
  if (x0 < 0) x0 = 0;
  y0 = (dat->imgWG->allocation.height - dat->ny*dat->zoom)/2;
  if (y0 < 0) y0 = 0;

  gtk_widget_get_pointer(dat->imgWG, &x, &y);
  dat->x = (x-x0) / dat->zoom + 1;
  dat->y = (y-y0) / dat->zoom + 1;

  if (dat->x<1) dat->x = 1;
  if (dat->y<1) dat->y = 1;
  if (dat->x>dat->nx) dat->x = dat->nx;
  if (dat->y>dat->ny) dat->y = dat->ny;
  
  updateStatusBar(user_data);
  gdk_flush();
  return TRUE;
}

GdkPixbuf * newPixbufFromSEXP (SEXP xx, int index) {
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

void updateStatusBar(gpointer user_data) {
  udata *dat=(udata *)user_data;
  gchar str[255];
  sprintf(str, "Frame: %d/%d\tImage: %dx%dx%d\tZoom: %d%%\t Position: (%d,%d)", 
	  (int)(dat->index+1), (int)dat->nz, (int)dat->nx, (int)dat->ny, (int)dat->nz, (int)(dat->zoom*100), (int)dat->x, (int)dat->y); 
  gtk_statusbar_pop(GTK_STATUSBAR(dat->stbarWG), 0);
  gtk_statusbar_push(GTK_STATUSBAR(dat->stbarWG), 0, str);
} 

#endif

