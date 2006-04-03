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
    catch(exception &error) {
        cout << "Caught exception in display thread: " << error.what() << endl;
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
    catch(exception &error) {
        cout << "Caught exception in display thread: " << error.what() << endl;
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
    catch(exception &error) {
        cout << "Caught exception in display thread: " << error.what() << endl;
    }
    THREAD_ON = false;
    return NULL;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP displayImages(SEXP rimage, SEXP animate) {
    if (rimage == R_NilValue) {
        warning("nothing to display and animate: exiting");
        return R_NilValue;
    }
    try {
        bool is2D = (strcmp(CHAR(asChar(GET_CLASS(rimage))), "Image2D") == 0);
        bool is3D = (strcmp(CHAR(asChar(GET_CLASS(rimage))), "Image3D") == 0);
        if (!is2D && !is3D)
            error("'image' argument is expected to be of class 'Image2D' or 'Image3D'");
        bool doanimate = LOGICAL(animate)[0];
        /* test dimensionality: 2D and 3D only can be displayed */
        int ndim = LENGTH(GET_DIM(GET_SLOT(rimage, mkString(".Data"))));
        if (ndim < 2 || ndim > 3)
            error("max 'image' dimension for display is 3D, select a subset and try again");
        /* FIXME This must be uncommented to force closing image before opening a new one!!!
        */
        if (THREAD_ON)
            error("another image is currently displayed, close it first! Read help on 'display'");

        pthread_t res;
        if (is3D) {
            if (doanimate) {
                if (pthread_create(&res, NULL, THREAD_FUN_animate, (void *)rimage) != 0)
                    error("pthread problem. Ccannot create display thread");
            }
            else
                if (pthread_create(&res, NULL, THREAD_FUN_display, (void *)rimage) != 0)
                    error("pthread problem. Cannot create display thread");
        }
        if (is2D)
            if (pthread_create(&res, NULL, THREAD_FUN_displaySingle, (void *)rimage) != 0)
                error("pthread problem. Ccannot create display thread");
    }
    catch(...) {
        error("unidentified problem in 'displayImages' c++ routine");
    }
    return R_NilValue;
}
