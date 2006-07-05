/* -------------------------------------------------------------------------
Implementation of ImageMagick drawables in EBImage-R
Copyright (c) 2006 Oleg Sklyar
See: drawables.h for license
------------------------------------------------------------------------- */
#include "drawables.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <iostream>

using namespace std;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* THIS FUNCTION MODIFIES ITS rimage ARGUMENT, COPY BEFORE IF REQUIRED */
SEXP drawShapes(SEXP rimage, SEXP drawable) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
/*    bool isRGB = false;
    try {
        isRGB = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
    }
    catch(exception &error_) {
        error(error_.what());
    }
*/
    list<Drawable> objects;
    try {
        int shape = 0;
        char * drawablestr = CHAR(asChar(GET_CLASS(drawable)));
        if (strcmp(drawablestr, "DrawableText") == 0)    shape = 1;
        if (strcmp(drawablestr, "DrawableCircle") == 0)  shape = 2;
        if (strcmp(drawablestr, "DrawableLine") == 0)    shape = 3;
        if (strcmp(drawablestr, "DrawableRect") == 0)    shape = 4;
        if (strcmp(drawablestr, "DrawableEllipse") == 0) shape = 5;
        if (shape <= 0)
            error("Wrong class of argument drawable");
        //cout << shape << endl;
        SEXP dataSlot = GET_SLOT(drawable, mkString("x"));
        double * data = REAL(dataSlot);
        Color col = ColorGray(0);
        SEXP strokeColor = GET_SLOT(drawable, mkString("strokeColor"));
        try {
            col = Color(CHAR(asChar(strokeColor)));
        }
        catch (exception &error_) {
            warning(error_.what());
        }
        objects.push_back(DrawableStrokeColor(col));
        double width = REAL(GET_SLOT(drawable, mkString("strokeWidth")))[0];
        objects.push_back(DrawableStrokeWidth(width));
        bool doFill = LOGICAL(GET_SLOT(drawable, mkString("doFill")))[0];
        if (doFill) {
            col = ColorGray(0.5);
            SEXP fillColor = GET_SLOT(drawable, mkString("fillColor"));
            try {
                col = Color(CHAR(asChar(fillColor)));
            }
            catch (exception &error_) {
                warning(error_.what());
            }
            objects.push_back(DrawableFillColor(col));
            double opacity = REAL(GET_SLOT(drawable, mkString("fillOpacity")))[0];
            objects.push_back(DrawableFillOpacity(opacity));
        }
        else {
            objects.push_back(DrawableFillOpacity(0));
        }
        switch(shape) {
            case 1: {
                error("not implemented yet");
            }; break;
            /* circle */
            case 2: {
                int nobjects = LENGTH(dataSlot) / 3;
                Drawable obj;
                double x, y, r;
                for (int i = 0; i < nobjects; i++) {
                    x = data[i];
                    y = data[i + nobjects];
                    r = data[i + 2 * nobjects];
                    obj = DrawableEllipse(x, y, r, r, 0, 360);
                    objects.push_back(obj);
                }
            }; break;
            /* line */
            case 3: {
                int nobjects = LENGTH(dataSlot) / 4;
                Drawable obj;
                double x1, y1, x2, y2;
                for (int i = 0; i < nobjects; i++) {
                    x1 = data[i];
                    y1 = data[i + nobjects];
                    x2 = data[i + 2 * nobjects];
                    y2 = data[i + 3 * nobjects];
                    obj = DrawableLine(x1, y1, x2, y2);
                    objects.push_back(obj);
                }
            }; break;
            /* Rect */
            case 4: {
                int nobjects = LENGTH(dataSlot) / 4;
                Drawable obj;
                double x1, y1, x2, y2;
                for (int i = 0; i < nobjects; i++) {
                    x1 = data[i];
                    y1 = data[i + nobjects];
                    x2 = data[i + 2 * nobjects];
                    y2 = data[i + 3 * nobjects];
                    obj = DrawableRectangle(x1, y1, x2, y2);
                    objects.push_back(obj);
                }
            }; break;
            /* ellipse */
            case 5: {
                int nobjects = LENGTH(dataSlot) / 6;
                Drawable obj;
                double x1, y1, x2, y2, s, e;
                for (int i = 0; i < nobjects; i++) {
                    x1 = data[i];
                    y1 = data[i + nobjects];
                    x2 = data[i + 2 * nobjects];
                    y2 = data[i + 3 * nobjects];
                    s =  data[i + 4 * nobjects];
                    e =  data[i + 5 * nobjects];
                    obj = DrawableEllipse(0.5 * (x1 + x2), 0.5 * (y1 + y2), 0.5 * fabs(x2 - x1), 0.5 * fabs(y2 - y1), s, e);
                    objects.push_back(obj);
                }
            }; break;
            default: error("Undefined drawable type/unimplemented");
        }
        SEXP dim = GET_DIM(rimage);
        int nimages = INTEGER(dim)[2];
        for (int i = 0; i < nimages; i++) {
            /* apply operation to a single image */
            MagickImage image = pullImageData(rimage, i);
            try {
                image.draw(objects);
            }
            catch(WarningUndefined &magickWarning) {
                if (verbose)
                    warning(magickWarning.what());
            }
            catch(ErrorUndefined &magickError) {
                error(magickError.what());
            }
            catch(exception &error_) {
                error(error_.what());
            }
            /* push modified image back */
            pushImageData(image, rimage, i);
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    /* return modified image */
    return(rimage);
}
