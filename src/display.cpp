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
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP displayImages(SEXP rimage, SEXP animate) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    try {
        bool doanimate = LOGICAL(animate)[0];
        /* test dimensionality: 2D and 3D only can be displayed */
        /* FIXME This must be uncommented to force closing image before opening a new one!!!
        */
        if (THREAD_ON)
            error("Close currently displayed image first");
        pthread_t res;
        if (assertImage2D(rimage)) {
            if (pthread_create(&res, NULL, THREAD_FUN_displaySingle, (void *)rimage) != 0)
                error("Cannot create display thread");
        }
        else {
            if (doanimate) {
                if (pthread_create(&res, NULL, THREAD_FUN_animate, (void *)rimage) != 0)
                    error("Cannot create display thread");
            }
            else
                if (pthread_create(&res, NULL, THREAD_FUN_display, (void *)rimage) != 0)
                    error("Cannot create display thread");
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
