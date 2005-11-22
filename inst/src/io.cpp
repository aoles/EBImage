#include "io.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <iostream>

using namespace std;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP readImages(SEXP files, SEXP rgb) {
    bool isrgb;
    isrgb = LOGICAL(rgb)[0];
    int nfiles = LENGTH(files);
    if (nfiles < 1)
        error("length(files) must be non-zero");
    try {
        /* create an empty stack for images */
        MagickStack stack;
        /* load every file as stack (single frame files are supported) and push images into the above stack */
        for (int i = 0; i < nfiles; i++) {
            MagickStack pushstack;
            try {
                readImages(&pushstack, CHAR(STRING_ELT(files, i)));
            }
            catch(WarningFileOpen &magickOpenWarning) {
                cout << "\tImageMagick file I/O warning: " << magickOpenWarning.what() << endl;
            }
            catch(WarningUndefined &magickWarning) {
                cout << "\tImageMagick warning: " << magickWarning.what() << endl;
            }
            catch (ErrorUndefined &magickError) {
                cout << "\tCaught ImageMagick error: " << magickError.what() << "... skipping file!" << endl;
                continue;
            }
            catch (exception &cerror) {
                cout << "\tCaught c++ error/warning: " << cerror.what() << "... trying to process the file!" << endl;
            }
            catch (...) {
                cout << "\tUnidentified error/warning... trying to process the file!" << endl;
            }
            for (MagickStack::iterator it = pushstack.begin(); it != pushstack.end(); it++) {
                MagickImage image = *it;
                stack.push_back(image);
            }
        }
        return stack2SEXP(stack, isrgb);
    }
    catch(...) {
        error("some image(s) could not be loaded - unknown error in 'readImages' c++ routine");
    }
    /* the control should never come to this point */
    error("'readImages' c++ code is wrong - it should have never come to this point");
    return R_NilValue;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP writeImages(SEXP rimage, SEXP files) {
    if (rimage == R_NilValue) {
        warning("nothing to write: exiting");
        return R_NilValue;
    }
    try {
        SEXP dim = GET_DIM(rimage);
        int ndim = LENGTH(dim);
        if (ndim < 2 || ndim > 3)
            error("'object' argument is expected to be of class 'Image2D' or 'Image3D'");
        int nfiles = LENGTH(files);
        if (ndim == 2 && nfiles != 1)
            error("incorrect number of files supplied (must be 1)");
        if (ndim == 3 && nfiles != 1 && nfiles != INTEGER(dim)[2])
            error("incorrect number of files supplied (must be 1 or match exactly dim(object)[[2]])");
        if (ndim == 2) {
            MagickImage image = SEXP2Image(rimage);
            image.write(CHAR(asChar(files)));
            return(R_NilValue);
        }
        MagickStack stack = SEXP2Stack(rimage);
        if (nfiles == 1) {
            writeImages(stack.begin(), stack.end(), CHAR(asChar(files)));
            return(R_NilValue);
        }
        int i = 0;
        for (MagickStack::iterator it = stack.begin(); it != stack.end(); it++) {
            MagickImage image = *it;
            image.write(CHAR(STRING_ELT(files, i)));
            i++;
        }
        return R_NilValue;
    }
    catch(WarningUndefined &magickWarning) {
        cout << "\tImageMagick warning: " << magickWarning.what() << endl;
    }
    catch (ErrorUndefined &magickError) {
        cout << "\tCaught ImageMagick error: " << magickError.what() << endl;
    }
    catch (exception &cerror) {
        cout << "\tCaught c++ error/warning: " << cerror.what() << endl;
    }
    catch (...) {
        cout << "\tUnidentified error/warning..." << endl;
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP pingImages(SEXP files, SEXP showComments) {
    int nfiles = LENGTH(files);
    int fileSize = 0;
    bool comments = LOGICAL(showComments)[0];
    if (nfiles < 1)
        error("length(files) must be non-zero");
    for (int i = 0; i < nfiles; i++) {
        MagickImage image;
        try {
            image.ping(CHAR(STRING_ELT(files, i)));
        }
        catch(WarningFileOpen &magickOpenWarning) {
            cout << "\tImageMagick file I/O warning: " << magickOpenWarning.what() << endl;
        }
        catch(WarningUndefined &magickWarning) {
            cout << "\tImageMagick warning: " << magickWarning.what() << endl;
        }
        catch (ErrorUndefined &magickError) {
            cout << "\tCaught ImageMagick error: " << magickError.what() << "... skipping file!" << endl;
            continue;
        }
        catch (exception &cerror) {
            cout << "\tCaught c++ error/warning: " << cerror.what() << "... trying to process the file!" << endl;
        }
        catch (...) {
            cout << "\tUnidentified error/warning... trying to process the file!" << endl;
        }
        try {
            cout << "File: " << CHAR(STRING_ELT(files, i)) << endl;
            cout << "\tsize: width=" << image.columns() << "  height=" << image.rows() << endl;
            cout << "\tcommpression: type=" << image.compressType() << "  quality=" << image.quality() << endl;
            Geometry dens = image.density();
            cout << "\tdensity: hor=" << dens.width() << "  ver=" << dens.height() << endl;
            cout << "\tformat: " << image.format() << endl;
            cout << "\tmagick: " << image.magick() << endl;
            if (comments)
                cout << "\tcomment: " << image.comment() << endl;
            cout << "\tfile.size=" << image.fileSize() << endl;
            cout << endl;
            fileSize += image.fileSize();
        }
        catch(...) {
            cout << "\tSome image attributes could not be read... skipping" << endl;
            continue;
        }
    }
    cout << "TOTAL size of all files: " << fileSize << endl << endl;
    return R_NilValue;
}


