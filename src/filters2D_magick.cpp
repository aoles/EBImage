#include "filters2D_magick.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <iostream>

using namespace std;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* THIS FUNCTION MODIFIES ITS rimage ARGUMENT, COPY BEFORE IF REQUIRED */
SEXP stdFilter2D(SEXP rimage, SEXP filterNo, SEXP param) {
    /* parse param here */
    double * _param = NULL;
    int npar = 0;
    int _filterNo = 0;
    bool isRGB = false;
    try {
        _filterNo = INTEGER(filterNo)[0];
        isRGB = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
        if (param != R_NilValue) {
            npar = LENGTH(param);
            if (npar > 0) {
                _param = new double[npar];
                for (int i = 0; i < npar; i++)
                    _param[i] = REAL(param)[i];
            }
        }
    }
    catch(exception &cerror) {
        cout << "Caught c++ exception: " << cerror.what() << " when parsing function arguments in c++" << endl;
        error("Arguments passed to the function are wrong");
    }
    catch(...) {
        error("Caught unknown error when parsing function arguments in c++");
        return R_NilValue;
    }
    SEXP dim = GET_DIM(rimage);
    int nimages = 1;
    if (LENGTH(dim) > 2)
            nimages = INTEGER(dim)[2];
    for (int i = 0; i < nimages; i++) {
        /* apply operation to a single image */
        MagickImage image = pullImageData(rimage, i);
        try {
            switch(_filterNo) {
                /* adaptive threshold */
                case  1: {
                    if (npar < 3)
                        error("wrong number of parameters in call to 'treshold.adaptive'");
                    image.adaptiveThreshold((unsigned int)_param[0], (unsigned int)_param[1], (unsigned int)_param[2]);
                }; break;
                /* blur */
                case  2: {
                    if (npar < 2)
                        error("wrong number of parameters in call to 'blur'");
                    image.blur(_param[0], _param[1]);
                }; break;
                /* contrast */
                case  3: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'contrast'");
                    image.contrast((unsigned int)_param[0]);
                }; break;
                /* despeckle */
                case  4: {
                    image.despeckle();
                }; break;
                /* edge */
                case  5: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'edge'");
                    image.edge(_param[0]);
                }; break;
                /* enhance */
                case  6: {
                    image.enhance();
                }; break;
                /* equalize */
                case  7: {
                    image.equalize();
                }; break;
                /* floodfill */
                case  8: {
                    if (npar < 3)
                        error("wrong number of parameters in call to 'floodfill'");
                    error("'floodfill' function has not yet been implemented");
                    // image.floodFillColor(_param[0], _param[1], );
                }; break;
                /* floodfill.border */
                case  9: {
                    if (npar < 3)
                        error("wrong number of parameters in call to 'floodfill.border'");
                    error("'floodfill.border' function has not yet been implemented");
                    // image.floodFillColor(_param[0], _param[1], );
                }; break;
                /* gamma */
                case 10: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'gamma.image'");
                    image.gamma(_param[0]);
                }; break;
                /* gaussian.blur */
                case 11: {
                    if (npar < 2)
                        error("wrong number of parameters in call to 'gaussian.blur'");
                    image.gaussianBlur(_param[0], _param[1]);
                }; break;
                /* median */
                case 12: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'median.image'");
                    image.medianFilter(_param[0]);
                }; break;
                /* modulate */
                case 13: {
                    if (npar < 3)
                        error("wrong number of parameters in call to 'modulate.image'");
                    image.modulate(_param[0], _param[1], _param[2]);
                }; break;
                /* reduce noise */
                case 14: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'reduce.noise'");
                    if (_param[0] == -1.0)
                        image.reduceNoise();
                    else
                        image.reduceNoise(_param[0]);
                }; break;
                /* segment */
                case 18: {
                    if (npar < 2)
                        error("wrong number of parameters in call to 'segment.image'");
                    image.segment(_param[0], _param[1]);
                }; break;
                /* shade */
                case 19: {
                    if (npar < 3)
                        error("wrong number of parameters in call to 'shade'");
                    bool shading = false;
                    if (_param[2] == 1.0)
                        shading = true;
                    image.shade(_param[0], _param[1], shading);
                }; break;
                /* sharpen */
                case 20: {
                    if (npar < 2)
                        error("wrong number of parameters in call to 'sharpen'");
                    image.sharpen(_param[0], _param[1]);
                }; break;
                /* solarize */
                case 21: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'solarize'");
                    image.solarize(_param[0]);
                }; break;
                /* spread */
                case 22: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'spread'");
                    image.spread((unsigned int)_param[0]);
                }; break;
                /* unsharp mask */
                case 23: {
                    if (npar < 4)
                        error("wrong number of parameters in call to 'unsharp.mask'");
                    image.unsharpmask(_param[0], _param[1], _param[2], _param[3]);
                }; break;
                /* add noise */
                case 24: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'image.addnoise'");
                    switch((int)_param[0]) {
                        case 1: image.addNoise(UniformNoise); break;
                        case 2: image.addNoise(GaussianNoise); break;
                        case 3: image.addNoise(MultiplicativeGaussianNoise); break;
                        case 4: image.addNoise(ImpulseNoise); break;
                        case 5: image.addNoise(LaplacianNoise); break;
                        case 6: image.addNoise(PoissonNoise); break;
                        default: image.addNoise(GaussianNoise);
                    }
                }; break;
                default: error("wrong image processing filter in 'stdFilter2D' c++ routine");
            }
        }
        catch(WarningUndefined &magickWarning) {
            cout << "Caught Magick++ warning: " << magickWarning.what() << "... should not affect the result. Continuing..." << endl;
        }
        catch(ErrorUndefined &magickError) {
            cout << "Caught Magick++ error: " << magickError.what() << "... result will be affected" << endl;
            error("Caught Magick++ error that would definitely affect the result");
        }
        catch(exception &error) {
            cout << "Caught c++ error: " << error.what() << "... result may be affected. Continuing..." << endl;
        }
        catch(...) {
            cout << "Caught unknown error: result may be affected. Continuing..." << endl;
        }
        /* push modified image back */
        pushImageData(image, rimage, i);
    }
    if (_param != NULL)
        delete[] _param;
    /* return modified image */
    return(rimage);
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* This function changes image size and generates a new image! */
SEXP stdFilter2DRedim(SEXP rimage, SEXP filterNo, SEXP param) {
    /* parse param here */
    double * _param = NULL;
    int npar = 0;
    int _filterNo = 0;
    bool isRGB = false;
    try {
        _filterNo = INTEGER(filterNo)[0];
        isRGB = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
        if (param != R_NilValue) {
            npar = LENGTH(param);
            if (npar > 0) {
                _param = new double[npar];
                for (int i = 0; i < npar; i++)
                    _param[i] = REAL(param)[i];
            }
        }
    }
    catch(exception &cerror) {
        cout << "Caught c++ exception: " << cerror.what() << " when parsing function arguments in c++" << endl;
        error("Arguments passed to the function are wrong");
    }
    catch(...) {
        error("Caught unknown error when parsing function arguments in c++");
        return R_NilValue;
    }
    MagickStack stack = SEXP2Stack(rimage);
    MagickStack resStack;
    MagickImage image;
    for (MagickStack::iterator it = stack.begin(); it != stack.end(); it++) {
        image = *it;
        /* apply operation to a single image */
        try {
            switch(_filterNo) {
                /* rotate */
                case 15: {
                    if (npar < 1)
                        error("wrong number of parameters in call to 'rotate'");
                    image.rotate(_param[0]);
                }; break;
                /* sample */
                case 16: {
                    if (npar < 2)
                        error("wrong number of parameters in call to 'sample'");
                        Geometry geom((unsigned int)_param[0], (unsigned int)_param[1]);
                    image.sample(geom);
                }; break;
                /* scale */
                case 17: {
                    if (npar < 2)
                        error("wrong number of parameters in call to 'scale'");
                        Geometry geom((unsigned int)_param[0], (unsigned int)_param[1]);
                    image.scale(geom);
                }; break;
                default: error("wrong image processing filter in 'stdFilter2DRedim' c++ routine");
            }
        }
        catch(WarningUndefined &magickWarning) {
            cout << "Caught Magick++ warning: " << magickWarning.what() << "... should not affect the result. Continuing..." << endl;
        }
        catch(ErrorUndefined &magickError) {
            cout << "Caught Magick++ error: " << magickError.what() << "... result will be affected" << endl;
            error("Caught Magick++ error that would definitely affect the result");
        }
        catch(exception &error) {
            cout << "Caught c++ error: " << error.what() << "... result may be affected. Continuing..." << endl;
        }
        catch(...) {
            cout << "Caught unknown error: result may be affected. Continuing..." << endl;
        }
        /* push modified image back */
        resStack.push_back(image);
    }
    if (_param != NULL)
        delete[] _param;
    /* return modified image */
    return(stack2SEXP(resStack, isRGB));
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void normalizeDataset(double * data, double * range, int length) {
    if (data == NULL || range == NULL || length <= 0) return;
    double max = data[0];
    double min = data[0];
    double value;
    for (int i = 0; i < length; i++) {
        value = data[i];
        if (value < min) min = value;
        if (value > max) max = value;
    }
    if (min == max) return;
    double factor = (range[1] - range[0]) / (max - min);
    for (int i = 0; i < length; i++)
        data[i] = (data[i] - min) * factor + range[0];
}

/* this function modifies the object - must be copied before if required! */
SEXP normalizeImages(SEXP rimage, SEXP range, SEXP independent) {
    try {
        int * dim = INTEGER(GET_DIM(rimage));
        int ndim = LENGTH(GET_DIM(rimage));
        int ncol = dim[0];
        int nrow = dim[1];
        int nimages = 1;
        if (ndim > 2)
            nimages = dim[2];
        /* grayscale images assumed of the type double */
        double * data;
        switch (LOGICAL(independent)[0]) {
            case true: {
                for (int i = 0; i < nimages; i++) {
                    data = &(REAL(rimage)[i * ncol * nrow]);
                    normalizeDataset(data, &(REAL(range)[0]), ncol * nrow);
                }
            }; break;
            default: {
                data = &(REAL(rimage)[0]);
                normalizeDataset(data, &(REAL(range)[0]), ncol * nrow * nimages);
            }
        }
    }
    catch(...) {
        error("exception within normalizeImages c++ routine");
    }
    return rimage;
}

