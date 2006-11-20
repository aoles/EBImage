/* -------------------------------------------------------------------------
Image display
Copyright (c) 2005 Oleg Sklyar
See: display.h for license
------------------------------------------------------------------------- */
#ifdef USEGTK
    #include <gtk/gtk.h>
    #include "R_ext/eventloop.h"
    #include <gdk/gdkx.h>
#endif

#include "display.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <pthread.h>
#include <iostream>

using namespace std;

/* FIXME
   One display at a time is allowed! The problem is that we cannot interrupt
   ->display() or displayImages() command programmatically. To allow for
   resources cleanup, the window must be closed manually!
   The check is done by THREAD_ON = NULL
*/
bool THREAD_ON = false;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void * THREAD_FUN_display(void * ptr) {
    THREAD_ON = true;
    try {
        MagickStack stack = SEXP2Stack((SEXP)ptr);
        if (stack.size() > 1)
            displayImages(stack.begin(), stack.end());
        else {
            MagickStack::iterator it = stack.begin();
            MagickImage image = *it;
            /* it = NULL; // FC5/GCC4.1 error */
            image.display();
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    THREAD_ON = false;
    return NULL;
}

#ifdef USEGTK

void mage_gtk_eventHandler(void *userData) {
    while (gtk_events_pending()) gtk_main_iteration();
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void mage_gtkDisplay(SEXP rimage) {
    int argc = 1;
    typedef char* pxchar;
    char ** argv = new pxchar[1];
    argv[0] = "R";
    if(!gtk_init_check(&argc, &argv))
        error("Failed to initialize GTK+. Use display(x, TRUE) instead");
    InputHandler * hdlr = addInputHandler(R_InputHandlers, ConnectionNumber(GDK_DISPLAY()), mage_gtk_eventHandler, -1);
    MagickImage image = SEXP2Image(rimage);
    int * dim = INTEGER(GET_DIM(rimage));
    GdkPixbuf * gdkpx = gdk_pixbuf_new(GDK_COLORSPACE_RGB, true, 8, dim[0], dim[1]);
    if (image.type() == GrayscaleType)
        image.write(0, 0, dim[0], dim[1], "IIIA", CharPixel, gdk_pixbuf_get_pixels(gdkpx));
    else
        image.write(0, 0, dim[0], dim[1], "RGBA", CharPixel, gdk_pixbuf_get_pixels(gdkpx));
    GtkWidget * wnd = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_container_set_border_width (GTK_CONTAINER (wnd), 10);
    gtk_window_set_title(GTK_WINDOW(wnd), "GTK+ R-Image display");
    GtkWidget * img = gtk_image_new_from_pixbuf(gdkpx);
    g_object_unref(gdkpx);
    gtk_container_add(GTK_CONTAINER (wnd), img);
    gtk_widget_show_all(wnd);
    gdk_flush();
    // FIXME -- check if I need finalizers for this window, probably not, but check
}

gboolean onWinDestroy(GtkWidget *, GdkEvent *, gpointer); // "destroy-event"
gboolean onZoomInPress(GtkButton *, gpointer); // "button-press-event"
gboolean onZoomOutPress(GtkButton *, gpointer); // "button-press-event"
gboolean onZoomOnePress(GtkButton *, gpointer); // "button-press-event"
//gboolean onZoomOutPress(GtkWidget *widget,GdkEventButton *event, gpointer user_data) // "button-press-event"

struct TheWindow {
    GtkWindow * wnd;
    GtkImage * img;
    GdkPixbuf * gdkpx;
};

void theGtkDisplay(SEXP rimage) {
    int argc = 1;
    typedef char* pxchar;
    char ** argv = new pxchar[1];
    argv[0] = "R";
    if(!gtk_init_check(&argc, &argv))
        error("Failed to initialize GTK+. Use display(x, TRUE) instead");
    InputHandler * hdlr = addInputHandler(R_InputHandlers, ConnectionNumber(GDK_DISPLAY()), mage_gtk_eventHandler, -1);
    MagickImage image = SEXP2Image(rimage);
    int * dim = INTEGER(GET_DIM(rimage));
    GdkPixbuf * gdkpx = gdk_pixbuf_new(GDK_COLORSPACE_RGB, true, 8, dim[0], dim[1]);
    if (image.type() == GrayscaleType)
        image.write(0, 0, dim[0], dim[1], "IIIA", CharPixel, gdk_pixbuf_get_pixels(gdkpx));
    else
        image.write(0, 0, dim[0], dim[1], "RGBA", CharPixel, gdk_pixbuf_get_pixels(gdkpx));
    GtkWidget * wnd = gtk_window_new(GTK_WINDOW_TOPLEVEL);
//    gtk_container_set_border_width (GTK_CONTAINER (wnd), 10);
    gtk_window_set_resizable(GTK_WINDOW(wnd), FALSE);

    gtk_window_set_title(GTK_WINDOW(wnd), "GTK+ R-Image display");
    GtkWidget * img = gtk_image_new_from_pixbuf(gdkpx);

    TheWindow * winptr = new TheWindow();
    winptr->wnd = GTK_WINDOW(wnd);
    winptr->img = GTK_IMAGE(img);
    winptr->gdkpx = gdkpx;

    GtkWidget * btnZoomIn = gtk_button_new_with_label("Zoom in");
    g_signal_connect(G_OBJECT(btnZoomIn), "pressed", G_CALLBACK(onZoomInPress), winptr);
    GtkWidget * btnZoomOut = gtk_button_new_with_label("Zoom out");
    g_signal_connect(G_OBJECT(btnZoomOut), "pressed", G_CALLBACK(onZoomOutPress), winptr);
    GtkWidget * btnZoomOne = gtk_button_new_with_label("1:1");
    g_signal_connect(G_OBJECT(btnZoomOne), "pressed", G_CALLBACK(onZoomOnePress), winptr);

    GtkWidget * vbox = gtk_vbox_new (FALSE, 0);
    GtkWidget * hbtnbox = gtk_hbutton_box_new ();
    gtk_container_add (GTK_CONTAINER (wnd), vbox);
    gtk_box_pack_start (GTK_BOX (vbox), img, TRUE, TRUE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), hbtnbox, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (hbtnbox), btnZoomIn);
    gtk_container_add (GTK_CONTAINER (hbtnbox), btnZoomOut);
    gtk_container_add (GTK_CONTAINER (hbtnbox), btnZoomOne);

//    gtk_container_add(GTK_CONTAINER (wnd), btnZoomIn);
//    gtk_container_add(GTK_CONTAINER (wnd), img);

    // now let's add a callback that will unref the pixbuf when the window is closed, "destroy-event"
    g_signal_connect(G_OBJECT(wnd), "delete-event", G_CALLBACK(onWinDestroy), winptr);

    gtk_widget_show_all(wnd);
    gdk_flush();
}

gboolean onWinDestroy(GtkWidget * wnd, GdkEvent * event, gpointer winptr) {
    TheWindow * winstruct = (TheWindow *)winptr;
    g_object_unref(winstruct->gdkpx);
    delete winstruct;
//    cout << "GTK+ R-Image display closed\n";
    return FALSE;
}

gboolean onZoomInPress(GtkButton * btn, gpointer winptr) {
    TheWindow * winstruct = (TheWindow *)winptr;
    int w = gdk_pixbuf_get_width(gtk_image_get_pixbuf(winstruct->img));
    int h = gdk_pixbuf_get_height(gtk_image_get_pixbuf(winstruct->img));

    GdkPixbuf * newpx = gdk_pixbuf_scale_simple(winstruct->gdkpx, (int)(w*1.2), (int)(h*1.2), GDK_INTERP_BILINEAR);
    gtk_image_set_from_pixbuf(winstruct->img, newpx);
    g_object_unref(newpx);
    gdk_flush();
    return TRUE;
}

gboolean onZoomOutPress(GtkButton * btn, gpointer winptr) {
    TheWindow * winstruct = (TheWindow *)winptr;
    int w = gdk_pixbuf_get_width(gtk_image_get_pixbuf(winstruct->img));
    int h = gdk_pixbuf_get_height(gtk_image_get_pixbuf(winstruct->img));

    GdkPixbuf * newpx = gdk_pixbuf_scale_simple(winstruct->gdkpx, (int)(w*0.8), (int)(h*0.8), GDK_INTERP_BILINEAR);
    gtk_image_set_from_pixbuf(winstruct->img, newpx);
    g_object_unref(newpx);
    gtk_window_resize (winstruct->wnd, 100, 20);
    gdk_flush();
    return TRUE;
}
gboolean onZoomOnePress(GtkButton * btn, gpointer winptr) {
    TheWindow * winstruct = (TheWindow *)winptr;
    gtk_image_set_from_pixbuf(winstruct->img, winstruct->gdkpx);
    gtk_window_resize (winstruct->wnd, 100, 20);
    gdk_flush();
    return TRUE;
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP displayImages(SEXP rimage, SEXP nogtk) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    try {
#ifdef USEGTK
        if (!LOGICAL(nogtk)[0] && assertImage2D(rimage)) {
            theGtkDisplay(rimage);
            return R_NilValue;
        }
#endif
        if (THREAD_ON)
            error("Close currently displayed image first");
        pthread_t res;
        if (pthread_create(&res, NULL, THREAD_FUN_display, (void *)rimage) != 0)
            error("Cannot create display thread");
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
