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
        error("At least one file/URL must be supplied");
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
                if (verbose)
                    warning(magickOpenWarning.what());
            }
            catch(WarningUndefined &magickWarning) {
                if (verbose)
                    warning(magickWarning.what());
            }
            catch (ErrorUndefined &magickError) {
                warning(magickError.what());
//                continue;  // commented: maybe something was loaded
            }
            catch (exception &error_) {
                if (verbose)
                    warning(error_.what());
            }
            catch (...) {
                if (verbose)
                    warning("Unidentified non-critical IO problem");
            }
            for (MagickStack::iterator it = pushstack.begin(); it != pushstack.end(); it++) {
                MagickImage image = *it;
                stack.push_back(image);
            }
        }
        return stack2SEXP(stack, isrgb);
    }
    catch (exception &error_) {
        error(error_.what());
    }
    catch (...) {
        error("Unidentified critical memory problem");
    }
    /* the control should never come to this point */
    error("Bug in readImages, please contact EBImage package developers");
    return R_NilValue;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP writeImages(SEXP rimage, SEXP files) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    try {
        SEXP dim = GET_DIM(rimage);
        int nimages = INTEGER(dim)[2];
        int nfiles = LENGTH(files);
        if (nfiles != 1 && nfiles != nimages)
            error("Number of files must equal number of images or 1");
        if (nimages == 1) {
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
        if (verbose)
            warning(magickWarning.what());
    }
    catch (ErrorUndefined &magickError) {
        error(magickError.what());
    }
    catch (exception &error_) {
        error(error_.what());
    }
    catch (...) {
        error("Unidentified critical memory problem");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP pingImages(SEXP files, SEXP showComments) {
    int nfiles = LENGTH(files);
    int fileSize = 0;
    bool comments = LOGICAL(showComments)[0];
    if (nfiles < 1)
        error("At least one file/URL must be supplied");
    for (int i = 0; i < nfiles; i++) {
        MagickImage image;
        try {
            image.ping(CHAR(STRING_ELT(files, i)));
        }
        catch(WarningFileOpen &magickOpenWarning) {
            if (verbose)
                warning(magickOpenWarning.what());
        }
        catch(WarningUndefined &magickWarning) {
            if (verbose)
                warning(magickWarning.what());
        }
        catch (ErrorUndefined &magickError) {
            warning(magickError.what());
            continue;
        }
        catch (exception &error_) {
            warning(error_.what());
        }
        catch (...) {
            error("Unidentified non-critical IO problem");
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
        catch (exception &error_) {
            warning(error_.what());
            continue;
        }
        catch (...) {
            error("Unidentified non-critical IO problem");
        }
    }
    cout << "TOTAL size of all files: " << fileSize << endl << endl;
    return R_NilValue;
}


