/* -------------------------------------------------------------------------
Morphological filters for greyscale images
Copyright (c) 2012-2015 Ilia Kats, Andrzej Oles
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <math.h>

#define DILATE 0
#define ERODE 1
#define OPENING 2
#define CLOSING 3
#define TOPHAT_WHITE  4
#define TOPHAT_BLACK 5
#define TOPHAT_SELFCOMPLEMENTARY 6

#define BUF_LENGTH 10

#define CHECK_BUFFER(pointer, occupied, buffer, type) \
if (occupied == buffer) {\
    buffer += BUF_LENGTH;\
    pointer = R_Realloc(pointer, buffer, type);\
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


void compute_lookup_table_for_line_dilate(numeric ***T, numeric *image, int yOff, int line, chordSet *set, PointXY size) {
    int y = line + yOff;

    if (y < 0 || y >= size.y) {
      for (int i = set->minXoffset; i < size.x + set->maxXoffset; ++i) {
        T[yOff][0][i] = -DBL_MAX;
      }
    }
    else {
      int maxX = MIN(size.x, size.x + set->maxXoffset);
      int i = set->minXoffset;
      
      for (i; i < 0; ++i) {
        T[yOff][0][i] = -DBL_MAX;
      }
      for (i; i < maxX; ++i) {
        numeric val = image[INDEX_FROM_XY(i, y, size.x)];
        T[yOff][0][i] = ISNA(val) ? -DBL_MAX : val;
      }
      for (i; i < size.x + set->maxXoffset; ++i) {
        T[yOff][0][i] = -DBL_MAX;
      }
    }
    
    for (int i = 1, d = 1; i <= set->maxN; ++i, d *= 2) {
      for (int j = set->minXoffset; j <= size.x + set->maxXoffset - 2 * d; ++j) {
          T[yOff][i][j] = MAX(T[yOff][i - 1][j], T[yOff][i - 1][j + d]);
      }
    }
}

void compute_lookup_table_for_line_erode(numeric ***T, numeric *image, int yOff, int line, chordSet *set, PointXY size) {
  int y = line + yOff;
  
  if (y < 0 || y >= size.y) {
    for (int i = set->minXoffset; i < size.x + set->maxXoffset; ++i) {
      T[yOff][0][i] = DBL_MAX;
    }
  }
  else {
    int maxX = MIN(size.x, size.x + set->maxXoffset);
    int i = set->minXoffset;
    
    for (i; i < 0; ++i) {
      T[yOff][0][i] = DBL_MAX;
    }
    for (i; i < maxX; ++i) {
      numeric val = image[INDEX_FROM_XY(i, y, size.x)];
      T[yOff][0][i] = ISNA(val) ? DBL_MAX : val;
    }
    for (i; i < size.x + set->maxXoffset; ++i) {
      T[yOff][0][i] = DBL_MAX;
    }
  }
  
  for (int i = 1, d = 1; i <= set->maxN; ++i, d *= 2) {
    for (int j = set->minXoffset; j <= size.x + set->maxXoffset - 2 * d; ++j) {
      T[yOff][i][j] = MIN(T[yOff][i - 1][j], T[yOff][i - 1][j + d]);
    }
  }
}

void dilate_line(numeric ***T, numeric *input, numeric *output, chordSet *set, int line, int width) {
    for (int i = 0; i < width; ++i) {
        int index = INDEX_FROM_XY(i, line, width);
        if (ISNA(input[index])) {
          output[index] = input[index];
        }
        else {
          for (int j = 0; j < set->CLength; ++j) {
            numeric v = MAX(T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset1], T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset2]);
            output[index] = MAX(output[index], v);
          }
        }
    }
}

void erode_line(numeric ***T, numeric *input, numeric *output, chordSet *set, int line, int width) {
  for (int i = 0; i < width; ++i) {
    int index = INDEX_FROM_XY(i, line, width);
    if (ISNA(input[index])) {
      output[index] = input[index];
    }
    else {
      for (int j = 0; j < set->CLength; ++j) {
        numeric v = MIN(T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset1], T[set->C[j].yOffset][set->C[j].n][i + set->C[j].xOffset2]);
        output[index] = MIN(output[index], v);
      }
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

    chordSet set = {.CLength = 0, .minYoffset = korigin.y, .maxYoffset = -korigin.y, .minXoffset = korigin.x, .maxXoffset = -korigin.x, .maxN = 0,};
    
    int CBufLength = 0;
    set.C = R_Calloc(BUF_LENGTH, chord);
    CBufLength = BUF_LENGTH;
    for (int i = 0; i < ksize.y; ++i) {
        double prevValue = 0;
        int beginChord = 0;
        for (int j = 0; j <= ksize.x; ++j) {
            double value = (j < ksize.x ? kern[INDEX_FROM_XY(j, i, ksize.x)] : 0);
            if (value == 0 && prevValue != 0) {
                chord c;
                c.yOffset = i - korigin.y;
                c.xOffset1 = beginChord - korigin.x;
                c.n = 0;
                int length = j - beginChord;
                if (length > 1) c.n = (int) floor(log2(length-1));
                c.xOffset2 = j - korigin.x - (int) pow(2, c.n);
                int xEnd = j - korigin.x - 1;
                
                set.C[set.CLength++] = c;
                CHECK_BUFFER(set.C, set.CLength, CBufLength, chord)
                  
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

numeric*** allocate_lookup_table(chordSet *set, int width) {
    numeric ***T;
    T = R_Calloc(set->maxYoffset - set->minYoffset + 1, numeric**); // + 1 for offset of 0
    T = T - set->minYoffset;

    int Txlength = width - set->minXoffset + set->maxXoffset + 1;
    for (int i = set->minYoffset; i <= set->maxYoffset; ++i) {
        T[i] = R_Calloc(set->maxN + 1, numeric*);
        for (int j = 0, d = 1; j <= set->maxN; ++j, d *= 2) {
            T[i][j] = R_Calloc(Txlength - d, numeric);
            T[i][j] = T[i][j] - set->minXoffset;
        }
    }
    return T;
}

void free_lookup_table(numeric ***T, chordSet *set) {
    for (int i = set->minYoffset; i <= set->maxYoffset; ++i) {
        for (int j = 0; j < set->maxN; j++) {
            numeric *first = T[i][j] + set->minXoffset;
            R_Free(first);
        }
        R_Free(T[i]);
    }
    numeric ***first = T + set->minYoffset;
    R_Free(first);
}

/*----------------------------------------------------------------------- */
void erode_dilate_internal (SEXP x, SEXP res, int what, chordSet *set, numeric ***T) {
    numeric * tgt, * src;
    int nz;
    int * dim;
    PointXY size;

    void (*compute_lookup_table_for_line)(numeric ***, numeric *, int, int, chordSet *, PointXY);
    void (*process_line)(numeric ***, numeric *, numeric *, chordSet *, int, int);
    
    if (what == ERODE) {
      process_line = &erode_line;
      compute_lookup_table_for_line = &compute_lookup_table_for_line_erode;
    }
    else {
      process_line = &dilate_line;
      compute_lookup_table_for_line = &compute_lookup_table_for_line_dilate;
    }
    
    dim = INTEGER ( GET_DIM(x) );
    size.x = dim[0];
    size.y = dim[1];
    nz = getNumberOfFrames(x,0);

    for (int i = 0; i < nz; i++ ) {
        tgt = &( REAL(res)[i * size.x * size.y] );
        src = &( REAL(x)[i * size.x * size.y] );
        for (int j = 0; j < size.x * size.y; ++j) {
            tgt[j] = what;
        }
        for (int j = set->minYoffset; j <= set->maxYoffset; ++j) {
            compute_lookup_table_for_line(T, src, j, 0, set, size);
        }
        process_line(T, src, tgt, set, 0, size.x);
        for (int j = 1; j < size.y; ++j) {
            numeric **first = T[set->minYoffset];
            for (int k = set->minYoffset; k < set->maxYoffset; ++k) {
                T[k] = T[k + 1];
            }
            T[set->maxYoffset] = first;
            compute_lookup_table_for_line(T, src, set->maxYoffset, j, set, size);
            process_line(T, src, tgt, set, j, size.x);
        }
    }
}

void opening_closing_internal(SEXP x, SEXP res, int what, chordSet *set, numeric ***T) {
    SEXP tmp;
    PROTECT( tmp = allocVector(TYPEOF(x), XLENGTH(x)) );
    DUPLICATE_ATTRIB(tmp, x);
    
    if (what == OPENING) {
        erode_dilate_internal(x, tmp, ERODE, set, T);
        erode_dilate_internal(tmp, res, DILATE, set, T);
    }
    else {
        erode_dilate_internal(x, tmp, DILATE, set, T);
        erode_dilate_internal(tmp, res, ERODE, set, T);
    }
    
    UNPROTECT(1);
}

SEXP morphology (SEXP x, SEXP kernel, SEXP what) {
    SEXP res;
    double * img1, * img2, * output;
    
    validImage(x,0);
    validImage(kernel,0);
    
    chordSet set = buildChordSet(kernel);
    numeric ***T = allocate_lookup_table(&set, INTEGER ( GET_DIM(x) )[0]);
    
    int nprotect = 0;
    
    PROTECT( res = allocVector(TYPEOF(x), XLENGTH(x)) );
    ++nprotect;
    DUPLICATE_ATTRIB(res, x);
    
    switch( INTEGER(what)[0] ) {
    case DILATE:
        erode_dilate_internal(x, res, DILATE, &set, T);
        break;
    case ERODE:
        erode_dilate_internal(x, res, ERODE, &set, T);
        break;
    case OPENING:
        opening_closing_internal(x, res, OPENING, &set, T);
        break;
    case CLOSING:
        opening_closing_internal(x, res, CLOSING, &set, T);
        break;
    case TOPHAT_WHITE:
        opening_closing_internal(x, res, OPENING, &set, T);
        img1 = REAL(x);
        img2 = REAL(res);
        output = REAL(res);
        for (int i = 0; i < length(x); ++i) {
            output[i] = img1[i] - img2[i];
        }
        break;
    case TOPHAT_BLACK:
        opening_closing_internal(x, res, CLOSING, &set, T);
        img1 = REAL(x);
        img2 = REAL(res);
        output = REAL(res);
        for (int i = 0; i < length(x); ++i) {
            output[i] = img2[i] - img1[i];
        }
        break;
    case TOPHAT_SELFCOMPLEMENTARY:
        opening_closing_internal(x, res, OPENING, &set, T);
        
        SEXP img;
        PROTECT( img = allocVector(TYPEOF(x), XLENGTH(x)) );
        ++nprotect;
        DUPLICATE_ATTRIB(img, x);
        opening_closing_internal(x, img, CLOSING, &set, T);
        
        img1 = REAL(img);
        img2 = REAL(res);
        output = REAL(res);
        for (int i = 0; i < length(x); ++i) {
            output[i] = img1[i] + img2[i];
        }
        break;
    }

    free_lookup_table(T, &set);
    R_Free(set.C);
    UNPROTECT (nprotect);
    return res;
}
