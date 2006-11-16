/* -------------------------------------------------------------------------
Image display
Copyright (c) 2005 Oleg Sklyar
See: display.h for license
------------------------------------------------------------------------- */
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
void * THREAD_FUN_displaySingle(void * ptr) {
    THREAD_ON = true;
    try {
        MagickImage image = SEXP2Image((SEXP)ptr);
        image.display();
    }
    catch(exception &error_) {
        error(error_.what());
    }
    THREAD_ON = false;
    return NULL;
}
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
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void * THREAD_FUN_animate(void * ptr) {
    THREAD_ON = true;
    try {
        MagickStack stack = SEXP2Stack((SEXP)ptr);
        if (stack.size() > 1)
            animateImages(stack.begin(), stack.end());
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
/*
    if (NAMED(rimage) > 1)
        gtk_window_set_title(GTK_WINDOW(wnd), CHAR(PRINTNAME(rimage)));
    else
*/
        gtk_window_set_title(GTK_WINDOW(wnd), "GTK+ R-Image display");
    GtkWidget * img = gtk_image_new_from_pixbuf(gdkpx);
    g_object_unref(gdkpx);
    gtk_container_add(GTK_CONTAINER (wnd), img);
    gtk_widget_show_all(wnd);
    gdk_flush();

    // FIXME -- check if I need finalizers for this window, probably not, but check
}
#endif
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP displayImages(SEXP rimage, SEXP nogtk) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    try {
        /* test dimensionality: 2D and 3D only can be displayed */
        /* FIXME This must be uncommented to force closing image before opening a new one!!!
        */
#ifdef USEGTK
        if (!LOGICAL(nogtk)[0] && assertImage2D(rimage))
            mage_gtkDisplay(rimage);
        else {
#endif
        if (THREAD_ON)
            error("Close currently displayed image first");
        pthread_t res;
        if (assertImage2D(rimage)) {
            if (pthread_create(&res, NULL, THREAD_FUN_displaySingle, (void *)rimage) != 0)
                error("Cannot create display thread");
        }
        else {
            if (pthread_create(&res, NULL, THREAD_FUN_display, (void *)rimage) != 0)
                error("Cannot create display thread");
        }
#ifdef USEGTK
        }
#endif
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
