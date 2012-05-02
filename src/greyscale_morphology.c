/* -------------------------------------------------------------------------
Morphological filters for greyscale images
Copyright (c) 2012 Ilia Kats
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <math.h>

#define ERODE  0
#define DILATE 1
#define OPENING  0
#define CLOSING 1
#define TOPHAT_WHITE  0
#define TOPHAT_BLACK 1
#define TOPHAT_SELFCOMPLEMENTARY 2

#define BUF_LENGTH 10

#define CHECK_BUFFER(pointer, occupied, buffer, type) \
if (occupied == buffer) {\
    buffer += BUF_LENGTH;\
    pointer = R_Realloc(pointer, buffer, type);\
}

typedef struct {
    int yOffset;
    int xOffset;
    int length;
    int rIndex;
} chord;

typedef struct {
    chord *C;
    int *R;
    int CLength;
    int RLength;
    int minYoffset;
    int maxYoffset;
    int minXoffset;
    int maxXoffset;
    int maxChordLength;
} chordSet;


typedef numeric (*compare) (numeric, numeric);

int contains_int(int *array, int what, int array_length) {
    for (int i = 0; i < array_length; i++) {
        if (array[i] == what)
            return i;
    }
    return -1;
}

void insert_in_int(int *array, int what, int where, int array_length) {
    for (int i = array_length - 1; i >= where; --i) {
        array[i + 1] = array[i];
    }
    array[where] = what;
}

void compute_lookup_table_for_line(numeric ***T, numeric *image, int yOff, int line, chordSet *set, PointXY size, compare minmax) {
    int effectiveY;
    if (line + yOff >= size.y)
        effectiveY = size.y - 1;
    else if (line + yOff >= 0)
        effectiveY = line + yOff;
    else
        effectiveY = 0;
    for (int i = 0; i < size.x; ++i) {

        T[yOff][0][i] = image[indexFromXY(i, effectiveY, size.x)];
    }
    for (int i = set->minXoffset; i < 0; ++i) {
        T[yOff][0][i] = image[indexFromXY(0, effectiveY, size.x)];
    }
    for (int i = size.x; i < size.x + set->maxXoffset; ++i) {
        T[yOff][0][i] = image[indexFromXY(size.x - 1, effectiveY, size.x)];
    }



    for (int i = 1; i < set->RLength; ++i) {
        for (int j = set->minXoffset; j < size.x + set->maxXoffset; ++j) {
            int d = set->R[i] - set->R[i - 1];
            T[yOff][i][j] = minmax(T[yOff][i - 1][j], T[yOff][i - 1][j + d]);
        }
    }
}

void erode_line(numeric ***T, numeric *output, chordSet *set, int line, int width, compare minmax) {
    for (int i = 0; i < width; ++i) {
        for (int j = 0; j < set->CLength; ++j) {
            numeric v = T[set->C[j].yOffset][set->C[j].rIndex][i + set->C[j].xOffset];
            int index = indexFromXY(i, line, width);
            output[index] = minmax(output[index], v);
        }
    }
}

chordSet buildChordSet(SEXP kernel) {
    numeric *kern = REAL (kernel);
    PointXY ksize, korigin;
    ksize.x = INTEGER ( GET_DIM(kernel) )[0];
    ksize.y = INTEGER ( GET_DIM(kernel) )[1];
    korigin.x = (int) ceil((float)ksize.x / 2) - 1; // -1 due to 0-based indices
    korigin.y = (int) ceil((float)ksize.y / 2) - 1;

    chordSet set = {.RLength = 0, .CLength = 0, .minYoffset = 0, .maxYoffset = 0, .minXoffset = 0, .maxXoffset = 0, .maxChordLength = 0,};

    int RBufLength = 0, CBufLength = 0;
    set.R = R_Calloc(BUF_LENGTH, int);
    RBufLength = BUF_LENGTH;
    set.C = R_Calloc(BUF_LENGTH, chord);
    CBufLength = BUF_LENGTH;
    for (int i = 0; i < ksize.y; ++i) {
        int prevValue = 0, beginChord = 0;
        for (int j = 0; j < ksize.x; ++j) {
            int index = indexFromXY(j, i, ksize.x);
            if ((kern[index] == 0 && prevValue == 1) || (j == ksize.x - 1 && (prevValue == 1 || j == 0))) {
                int length = j - beginChord;
                int yOff = i - korigin.y;
                int xOff = beginChord - korigin.x;
                int xEnd = length - korigin.x;
                chord c;
                if (j == ksize.x -1)
                    length++;
                int rindex = contains_int(set.R, length, set.RLength);
                if (rindex < 0) {
                    set.R[set.RLength] = length;
                    c.rIndex = set.RLength++;
                } else {
                    c.rIndex = rindex;
                }
                CHECK_BUFFER(set.R, set.RLength, RBufLength, int)

                c.yOffset = yOff;
                c.xOffset = xOff;
                c.length = length;
                set.C[set.CLength++] = c;
                CHECK_BUFFER(set.C, set.CLength, CBufLength, chord)

                if (yOff < set.minYoffset)
                    set.minYoffset = yOff;
                else if (yOff > set.maxYoffset)
                    set.maxYoffset = yOff;
                if (xOff < set.minXoffset)
                    set.minXoffset = xOff;
                else if (xEnd > set.maxXoffset)
                    set.maxXoffset = xEnd;
                if (length > set.maxChordLength)
                    set.maxChordLength = length;
            } else if (kern[index] == 1 && prevValue == 0) {
                beginChord = j;
            }
            prevValue = kern[index];
        }
    }

    int begin = 1;
    while (set.R[0] > 1) {
        insert_in_int(set.R, (int)ceil((float)set.R[0] / 2), 0, set.RLength++);
        for (int j = 0; j < set.CLength; ++j) {
            if (set.C[j].rIndex >= 0)
                ++set.C[j].rIndex;
        }
        CHECK_BUFFER(set.R, set.RLength, RBufLength, int)
        ++begin;
    }
    for (int i = begin; i < set.RLength; ++i) {
        if (2 * set.R[i - 1] < set.R[i]) {
            insert_in_int(set.R, (int)ceil((float)set.R[i] / 2), i, set.RLength++);
            for (int j = 0; j < set.CLength; ++j) {
                if (set.C[j].rIndex >= i)
                    ++set.C[j].rIndex;
            }
            CHECK_BUFFER(set.R, set.RLength, RBufLength, int)
            --i;
        }
    }
    return set;
}

void free_set(chordSet *set) {
    R_Free(set->R);
    R_Free(set->C);
}

numeric*** allocate_lookup_table(chordSet *set, int width) {
    numeric ***T;
    T = R_Calloc(abs(set->minYoffset) + abs(set->maxYoffset) + 1, numeric**); // + 1 for offset of 0
    T = T + abs(set->minYoffset);

    int Txlength = width + abs(set->minXoffset) + abs(set->maxXoffset);
    for (int i = set->minYoffset; i <= set->maxYoffset; ++i) {
        T[i] = R_Calloc(set->RLength, numeric*);
        for (int j = 0; j < set->RLength; j++) {
            T[i][j] = R_Calloc(Txlength, numeric);
            T[i][j] = T[i][j] + abs(set->minXoffset);
        }
    }
    return T;
}

void free_lookup_table(numeric ***T, chordSet *set) {
    for (int i = set->minYoffset; i <= set->maxYoffset; ++i) {
        for (int j = 0; j < set->RLength; j++) {
            numeric *first = T[i][j] - abs(set->minXoffset);
            R_Free(first);
        }
        R_Free(T[i]);
    }
    numeric ***first = T - abs(set->minYoffset);
    R_Free(first);
}

/*----------------------------------------------------------------------- */
SEXP lib_erode_dilate_greyscale_internal (SEXP x, int what, chordSet *set, numeric ***T) {
    numeric * tgt, * src;
    int nz, nprotect;
    int * dim;
    PointXY size;
    SEXP res;

    compare minmax;
    if (what == ERODE)
        minmax = &fmin;
    else
        minmax = &fmax;
    dim = INTEGER ( GET_DIM(x) );
    size.x = dim[0];
    size.y = dim[1];
    nz = getNumberOfFrames(x,0);

    nprotect = 0;

    PROTECT ( res = Rf_duplicate(x) );
    ++nprotect;

    for (int i = 0; i < nz; i++ ) {
        tgt = &( REAL(res)[i * size.x * size.y] );
        src = &( REAL(x)[i * size.x * size.y] );
        for (int j = 0; j < size.x * size.y; ++j) {
            tgt[j] = 1 - what;
        }
        for (int j = set->minYoffset; j <= set->maxYoffset; ++j) {
            compute_lookup_table_for_line(T, src, j, 0, set, size, minmax);
        }
        erode_line(T, tgt, set, 0, size.x, minmax);
        for (int j = 1; j < size.y; ++j) {
            numeric **first = T[set->minYoffset];
            for (int k = set->minYoffset; k < set->maxYoffset; ++k) {
                T[k] = T[k + 1];
            }
            T[set->maxYoffset] = first;
            compute_lookup_table_for_line(T, src, set->maxYoffset, j, set, size, minmax);
            erode_line(T, tgt, set, j, size.x, minmax);
        }
    }

    UNPROTECT (nprotect);
    return res;
}

SEXP lib_opening_closing_greyscale_internal(SEXP x, int what, chordSet *set, numeric ***T) {
    if (what == OPENING)
        return lib_erode_dilate_greyscale_internal(lib_erode_dilate_greyscale_internal(x, ERODE, set, T), DILATE, set, T);
    else
        return lib_erode_dilate_greyscale_internal(lib_erode_dilate_greyscale_internal(x, DILATE, set, T), ERODE, set, T);
}

SEXP lib_erode_dilate_greyscale (SEXP x, SEXP kernel, SEXP what) {
    validImage(x,0);
    validImage(kernel,0);
    int operation = INTEGER(what)[0];
    chordSet set = buildChordSet(kernel);
    numeric ***T = allocate_lookup_table(&set, INTEGER ( GET_DIM(x) )[0]);
    SEXP img = lib_erode_dilate_greyscale_internal(x, operation, &set, T);
    free_lookup_table(T, &set);
    free_set(&set);
    return img;
}

SEXP lib_opening_closing_greyscale (SEXP x, SEXP kernel, SEXP what) {
    int operation = INTEGER(what)[0];
    chordSet set = buildChordSet(kernel);
    numeric ***T = allocate_lookup_table(&set, INTEGER ( GET_DIM(x) )[0]);
    SEXP img = lib_opening_closing_greyscale_internal(x, operation, &set, T);
    free_lookup_table(T, &set);
    free_set(&set);
    return img;
}

SEXP lib_tophat_greyscale (SEXP x, SEXP kernel, SEXP what) {
    int operation = INTEGER(what)[0];
    chordSet set = buildChordSet(kernel);
    numeric ***T = allocate_lookup_table(&set, INTEGER ( GET_DIM(x) )[0]);
    PointXY size;
    numeric *img1, *img2, *output;
    int *dim = INTEGER ( GET_DIM(x) );
    size.x = dim[0];
    size.y = dim[1];
    int nz = getNumberOfFrames(x,0);
    int nprotect = 0;

    SEXP res;
    PROTECT ( res = Rf_duplicate(x) );
    ++nprotect;
    if (operation == TOPHAT_WHITE) {
        SEXP img = lib_opening_closing_greyscale_internal(x, OPENING, &set, T);
        PROTECT(img);
        ++nprotect;
        for (int i = 0; i < nz; ++i) {
            img1 = &(REAL(x)[i * size.x * size.y]);
            img2 = &(REAL(img)[i * size.x * size.y]);
            output = &(REAL(res)[i * size.x * size.y]);
            for (int j = 0; j < size.x * size.y; ++j) {
                output[j] = img1[j] - img2[j];
            }
        }
    } else if (operation == TOPHAT_BLACK) {
        SEXP img = lib_opening_closing_greyscale_internal(x, CLOSING, &set, T);
        PROTECT(img);
        ++nprotect;
        for (int i = 0; i < nz; ++i) {
            img1 = &(REAL(x)[i * size.x * size.y]);
            img2 = &(REAL(img)[i * size.x * size.y]);
            output = &(REAL(res)[i * size.x * size.y]);
            for (int j = 0; j < size.x * size.y; ++j) {
                output[j] = img2[j] - img1[j];
            }
        }
    } else if (operation == TOPHAT_SELFCOMPLEMENTARY) {
        SEXP img_open = lib_opening_closing_greyscale_internal(x, OPENING, &set, T);
        PROTECT(img_open);
        ++nprotect;
        SEXP img_close = lib_opening_closing_greyscale_internal(x, CLOSING, &set, T);
        PROTECT(img_close);
        ++nprotect;
        for (int i = 0; i < nz; ++i) {
            img1 = &(REAL(img_close)[i * size.x * size.y]);
            img2 = &(REAL(img_open)[i * size.x * size.y]);
            output = &(REAL(res)[i * size.x * size.y]);
            for (int j = 0; j < size.x * size.y; ++j) {
                output[j] = img1[j] - img2[j];
            }
        }
    }

    free_lookup_table(T, &set);
    free_set(&set);
    UNPROTECT (nprotect);
    return res;
}

