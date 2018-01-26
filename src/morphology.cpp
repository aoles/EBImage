/* -------------------------------------------------------------------------
 Morphological filters for greyscale images
 Copyright (c) 2012-2015 Ilia Kats, Andrzej Oles
 See: ../LICENSE for license, LGPL
 ------------------------------------------------------------------------- */

#include "morphology.h"
#include "tools.h"

#define DILATE 0
#define ERODE 1
#define OPENING 2
#define CLOSING 3
#define TOPHAT_WHITE  4
#define TOPHAT_BLACK 5
#define TOPHAT_SELFCOMPLEMENTARY 6

#define BUF_LENGTH 10

#define CHECK_BUFFER(pointer, occupied, buffer, type) \
if (occupied == buffer) {                             \
    buffer += BUF_LENGTH;                             \
    pointer = R_Realloc(pointer, buffer, type);       \
}

#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

typedef struct {
    int yOffset;
    int xOffset1;
    int xOffset2;
    int n;
} chord;

typedef struct {
    chord *C;
    int CLength;
    int minYoffset;
    int maxYoffset;
    int minXoffset;
    int maxXoffset;
    int maxN;
} chordSet;

/* use custom templates rather than std::numeric_limits to avoid dependency on C++11 due to lowest() */
#define MIN_VALUE true
#define MAX_VALUE false

template <typename type> const type limits(const bool);

template <> const int limits(const bool min) {
    return min ? INT_MIN : INT_MAX;
}

template <> const double limits(const bool min) {
    return min ? -DBL_MAX : DBL_MAX;
}

template <typename type> chordSet buildChordSet(type *, PointXY);
template <typename type> type*** allocate_lookup_table(chordSet *, int);
template <typename type> void free_lookup_table(type ***, chordSet *);
template <typename type> void compute_lookup_table_for_line_dilate(type ***, type *, int, int, chordSet *, PointXY);
template <typename type> void compute_lookup_table_for_line_erode(type ***, type *, int, int, chordSet *, PointXY);  
template <typename type> void dilate_line(type ***, type *, type *, chordSet *, int, int);
template <typename type> void erode_line(type ***, type *, type *, chordSet *, int, int);
template <typename type> void erode_dilate(type *, type *, PointXY, int, int, chordSet *, type ***);
template <typename type> void opening_closing(type *, type *, PointXY, int, int, chordSet *, type ***);
template <typename type> void tophat(type *, type *, PointXY, int, int, chordSet *, type ***);
template <typename type> void _morphology(type *, type *, PointXY, int, SEXP kernel, int what);


template <typename type> chordSet buildChordSet(type * kern, PointXY ksize) {
    PointXY korigin;
    korigin.x = (int) ceil((float)ksize.x / 2) - 1; // -1 due to 0-based indices
    korigin.y = (int) ceil((float)ksize.y / 2) - 1;
    
    chordSet set = {NULL, 0, korigin.y, -korigin.y, korigin.x, -korigin.x, 0};
    
    int CBufLength = 0;
    set.C = R_Calloc(BUF_LENGTH, chord);
    CBufLength = BUF_LENGTH;
    for (int i = 0; i < ksize.y; ++i) {
        type prevValue = 0;
        int beginChord = 0;
        for (int j = 0; j <= ksize.x; ++j) {
            type value = (j < ksize.x ? kern[INDEX_FROM_XY(j, i, ksize.x)] : 0);
            if (value == 0 && prevValue != 0) {
                chord c;
                c.yOffset = i - korigin.y;
                c.xOffset1 = beginChord - korigin.x;
                c.n = 0;
                int length = j - beginChord;
                if (length > 1) c.n = (int) floor(log2(length-1));
                c.xOffset2 = j - korigin.x - (int) pow(2.0, c.n);
                int xEnd = j - korigin.x - 1;
                
                set.C[set.CLength++] = c;
                CHECK_BUFFER(set.C, set.CLength, CBufLength, chord);
                
                if (c.yOffset < set.minYoffset)
                    set.minYoffset = c.yOffset;
                else if (c.yOffset > set.maxYoffset)
                    set.maxYoffset = c.yOffset;
                if (c.xOffset1 < set.minXoffset)
                    set.minXoffset = c.xOffset1;
                if (xEnd > set.maxXoffset)
                    set.maxXoffset = xEnd;
                if (c.n > set.maxN)
                    set.maxN = c.n;
            } else if (value != 0 && prevValue == 0) {
                beginChord = j;
            }
            prevValue = value;
        }
    }
    
    return set;
}

template <typename type> type*** allocate_lookup_table(chordSet *set, int width) {
    type ***T;
    T = R_Calloc(set->maxYoffset - set->minYoffset + 1, type**); // + 1 for offset of 0
    T = T - set->minYoffset;
    
    int Txlength = width - set->minXoffset + set->maxXoffset + 1;
    for (int i = set->minYoffset; i <= set->maxYoffset; ++i) {
        T[i] = R_Calloc(set->maxN + 1, type*);
        for (int j = 0, d = 1; j <= set->maxN; ++j, d *= 2) {
            T[i][j] = R_Calloc(Txlength - d, type);
            T[i][j] = T[i][j] - set->minXoffset;
        }
    }
    return T;
}

template <typename type> void free_lookup_table(type ***T, chordSet *set) {
    for (int i = set->minYoffset; i <= set->maxYoffset; ++i) {
        for (int j = 0; j < set->maxN; j++) {
            type *first = T[i][j] + set->minXoffset;
            R_Free(first);
        }
        R_Free(T[i]);
    }
    type ***first = T + set->minYoffset;
    R_Free(first);
}

template <typename type> void compute_lookup_table_for_line_dilate(type ***T, type *image, int yOff, int line, chordSet *set, PointXY size) {
    const type MIN_VAL = limits<type>(MIN_VALUE);
    
    int y = line + yOff;
    
    if (y < 0 || y >= size.y) {
        for (int i = set->minXoffset; i < size.x + set->maxXoffset; ++i) {
            T[yOff][0][i] = MIN_VAL;
        }
    }
    else {
        int maxX = MIN(size.x, size.x + set->maxXoffset);
        int i = set->minXoffset;
        
        for (i; i < 0; ++i) {
            T[yOff][0][i] = MIN_VAL;
        }
        for (i; i < maxX; ++i) {
            type val = image[INDEX_FROM_XY(i, y, size.x)];
            T[yOff][0][i] = ISNA(val) ? MIN_VAL : val;
        }
        for (i; i < size.x + set->maxXoffset; ++i) {
            T[yOff][0][i] = MIN_VAL;
        }
    }
    
    for (int i = 1, d = 1; i <= set->maxN; ++i, d *= 2) {
        for (int j = set->minXoffset; j <= size.x + set->maxXoffset - 2 * d; ++j) {
            T[yOff][i][j] = MAX(T[yOff][i - 1][j], T[yOff][i - 1][j + d]);
        }
    }
}

template <typename type> void compute_lookup_table_for_line_erode(type ***T, type *image, int yOff, int line, chordSet *set, PointXY size) {
    const type MAX_VAL = limits<type>(MAX_VALUE);
    
    int y = line + yOff;
    
    if (y < 0 || y >= size.y) {
        for (int i = set->minXoffset; i < size.x + set->maxXoffset; ++i) {
            T[yOff][0][i] = MAX_VAL;
        }
    }
    else {
        int maxX = MIN(size.x, size.x + set->maxXoffset);
        int i = set->minXoffset;
        
        for (i; i < 0; ++i) {
            T[yOff][0][i] = MAX_VAL;
        }
        for (i; i < maxX; ++i) {
            type val = image[INDEX_FROM_XY(i, y, size.x)];
            T[yOff][0][i] = ISNA(val) ? MAX_VAL : val;
        }
        for (i; i < size.x + set->maxXoffset; ++i) {
            T[yOff][0][i] = MAX_VAL;
        }
    }
    
    for (int i = 1, d = 1; i <= set->maxN; ++i, d *= 2) {
        for (int j = set->minXoffset; j <= size.x + set->maxXoffset - 2 * d; ++j) {
            T[yOff][i][j] = MIN(T[yOff][i - 1][j], T[yOff][i - 1][j + d]);
        }
    }
}

template <typename type> void dilate_line(type ***T, type *input, type *output, chordSet *set, int line, int width) {
    for (int i = 0; i < width; ++i) {
        int index = INDEX_FROM_XY(i, line, width);
        if (ISNA(input[index])) {
            output[index] = input[index];
        }
        else {
            for (int j = 0; j < set->CLength; ++j) {
                type v = MAX(T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset1], T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset2]);
                output[index] = MAX(output[index], v);
            }
        }
    }
}

template <typename type> void erode_line(type ***T, type *input, type *output, chordSet *set, int line, int width) {
    for (int i = 0; i < width; ++i) {
        int index = INDEX_FROM_XY(i, line, width);
        if (ISNA(input[index])) {
            output[index] = input[index];
        }
        else {
            for (int j = 0; j < set->CLength; ++j) {
                type v = MIN(T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset1], T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset2]);
                output[index] = MIN(output[index], v);
            }
        }
    }
}

template <typename type> void erode_dilate (type *x, type *res, PointXY size, int nz, int what, chordSet *set, type ***T) {
    type *tgt, *src;
    
    void (*compute_lookup_table_for_line)(type ***, type *, int, int, chordSet *, PointXY);
    void (*process_line)(type ***, type *, type *, chordSet *, int, int);
    
    if (what == ERODE) {
        process_line = &erode_line<type>;
        compute_lookup_table_for_line = &compute_lookup_table_for_line_erode<type>;
    }
    else {
        process_line = &dilate_line<type>;
        compute_lookup_table_for_line = &compute_lookup_table_for_line_dilate<type>;
    }
    
    for (int i = 0; i < nz; i++ ) {
        tgt = &( res[i * size.x * size.y] );
        src = &( x[i * size.x * size.y] );
        for (int j = 0; j < size.x * size.y; ++j) {
            tgt[j] = what;
        }
        for (int j = set->minYoffset; j <= set->maxYoffset; ++j) {
            compute_lookup_table_for_line(T, src, j, 0, set, size);
        }
        process_line(T, src, tgt, set, 0, size.x);
        for (int j = 1; j < size.y; ++j) {
            type **first = T[set->minYoffset];
            for (int k = set->minYoffset; k < set->maxYoffset; ++k) {
                T[k] = T[k + 1];
            }
            T[set->maxYoffset] = first;
            compute_lookup_table_for_line(T, src, set->maxYoffset, j, set, size);
            process_line(T, src, tgt, set, j, size.x);
        }
    }
}

template <typename type> void opening_closing(type *x, type *res, PointXY size, int nz, int what, chordSet *set, type ***T) {
    type * tmp = R_Calloc(size.x * size.y * nz, type);
    
    erode_dilate<type>(x, tmp, size, nz, (what-1) % 2, set, T);
    erode_dilate<type>(tmp, res, size, nz, what % 2, set, T);
    
    R_Free(tmp);
}

template <typename type> void tophat(type *x, type *res, PointXY size, int nz, int what, chordSet *set, type ***T) {
    int n = size.x * size.y * nz;
    
    switch(what) {
    case TOPHAT_WHITE:
        opening_closing<type>(x, res, size, nz, OPENING, set, T);
        for (int i = 0; i < n; ++i) {
            res[i] = x[i] - res[i];
        }
        break;
    case TOPHAT_BLACK:
        opening_closing<type>(x, res, size, nz, CLOSING, set, T);
        for (int i = 0; i < n; ++i) {
            res[i] = res[i] - x[i];
        }
        break;
    case TOPHAT_SELFCOMPLEMENTARY:
        type * tmp = R_Calloc(n, type);
        opening_closing<type>(x, res, size, nz, OPENING, set, T);
        opening_closing<type>(x, tmp, size, nz, CLOSING, set, T);
        for (int i = 0; i < n; ++i) {
            res[i] = res[i] + tmp[i];
        }
        R_Free(tmp);
        break;
    }
}

template <typename type> void _morphology(type *x, type *res, PointXY size, int nz, SEXP kernel, int what) {
    PointXY ksize;
    ksize.x = INTEGER ( GET_DIM(kernel) )[0];
    ksize.y = INTEGER ( GET_DIM(kernel) )[1];
    
    chordSet set;
    switch (TYPEOF(kernel)) {
    case LGLSXP:
    case INTSXP:
        set = buildChordSet<int>(INTEGER(kernel), ksize);
        break;
    case REALSXP:
        set = buildChordSet<double>(REAL(kernel), ksize);
        break;
    }
    
    type ***T = allocate_lookup_table<type>(&set, size.x);
    
    switch( what ) {
    case DILATE:
    case ERODE:
        erode_dilate<type>(x, res, size, nz, what, &set, T);
        break;
    case OPENING:
    case CLOSING:
        opening_closing<type>(x, res, size, nz, what, &set, T);
        break;
    case TOPHAT_WHITE:
    case TOPHAT_BLACK:
    case TOPHAT_SELFCOMPLEMENTARY:
        tophat<type>(x, res, size, nz, what, &set, T);
        break;
    }
    
    free_lookup_table<type>(T, &set);
    R_Free(set.C);
}

SEXP morphology (SEXP x, SEXP kernel, SEXP what) {
    validImage(x,0);
    validImage(kernel,0);
    
    SEXP res = PROTECT( allocVector(TYPEOF(x), XLENGTH(x)) );
    DUPLICATE_ATTRIB(res, x);
    
    int operation = INTEGER(what)[0];
    
    PointXY size;
    size.x = INTEGER ( GET_DIM(x) )[0];
    size.y = INTEGER ( GET_DIM(x) )[1];
    int nz = getNumberOfFrames(x,0);
    
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
        _morphology<int>(INTEGER(x), INTEGER(res), size, nz, kernel, operation);
        break;
    case REALSXP:
        _morphology<double>(REAL(x), REAL(res), size, nz, kernel, operation);
        break;
    }
    
    UNPROTECT(1);
    return res;
}
