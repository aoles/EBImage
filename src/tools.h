#ifndef EBIMAGE_TOOLS_H
#define EBIMAGE_TOOLS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef USE_GTK
#   ifdef __cplusplus
    extern
#   endif
    int GTK_OK;
#endif

#define MAX_MODE  1
#define MODE_GRAY 0
#define MODE_RGB  1

typedef struct {
    int x, y;
} PointXY;

double distanceXY (const PointXY, const PointXY);
double distancexy (int, int, int, int);
PointXY pointFromIndex (const int, const int);
int indexFromPoint (const PointXY, const int);
int indexFromXY (const int, const int, const int);

int isImage (SEXP);

#ifdef __cplusplus
};
#endif

#endif
