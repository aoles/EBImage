#ifndef _COMMON_H_LOCAL
#define _COMMON_H_LOCAL
/* -------------------------------------------------------------------------

Common definitions for the 'EBImage' project

Copyright (c) 2005 Oleg Sklyar

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
as published by the Free Software Foundation; either version 2.1
of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU Lesser General Public License for more details.
LGPL license wording: http://www.gnu.org/licenses/lgpl.html

------------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* imagemagick includes */
#include <magick/ImageMagick.h>

/* R includes */
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

/* GTK+ includes */
#ifdef USE_GTK
#   include <gtk/gtk.h>
//#   include <glib.h>
//#   include <glib-object.h>
#   include <gdk/gdk.h>
//#   include <gdk-pixbuf/gdk-pixbuf.h>
#   ifdef WIN32
        typedef unsigned long ulong;
        extern  __declspec(dllimport) void (* R_tcldo) ();
#       include <sys/types.h>
#       include <gdk/gdkwin32.h>
#   else
#       include "R_ext/eventloop.h"
#       include <gdk/gdkx.h>
#   endif
#   ifdef GLIB_GETTEXT
#       define GETTEXT_PACKAGE "gtk20"
#       include <glib/gi18n-lib.h>
#   endif
#endif

#define MAX_MODE  1
#define MODE_GRAY 0
#define MODE_RGB  1

typedef struct {
    int x, y;
} PointXY;

#ifdef __cplusplus
extern "C" {
#endif

/* R-lib exports to use with .Call */
SEXP         lib_ (SEXP);                               /* tools.c - _( in R messages */
SEXP         lib_readImages (SEXP, SEXP);               /* io.c */
SEXP         lib_writeImages (SEXP, SEXP, SEXP);        /* -"- */
SEXP         lib_chooseImages ();                       /* -"- */
SEXP         lib_display (SEXP, SEXP);                  /* display.c */
SEXP         lib_channel (SEXP, SEXP);                  /* colors.c - extract color channels etc */
SEXP         lib_filterMagick (SEXP, SEXP, SEXP);       /* filters_magick.c */
SEXP         lib_filterFill (SEXP, SEXP, SEXP, SEXP, SEXP); /* -"- */
SEXP         lib_erode_dilate (SEXP, SEXP, SEXP, SEXP); /* filters_morph.c */
SEXP         lib_filterThresh (SEXP, SEXP);             /* filters_thresh.c */
SEXP         lib_distMap (SEXP, SEXP, SEXP, SEXP);      /* filters_distmap.c */
SEXP         lib_filterInvWS (SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);  /* filters_watershed.c */


/* library exports and internal functions */
Image *      sexp2Magick (SEXP);                  /* conversions.c */
SEXP         magick2SEXP (Image *, int);          /* conversions.c */
int          isImage (SEXP);                                 /* tools.c */
double       distanceXY (const PointXY, const PointXY);       /* -"- */
PointXY      pointFromIndex (const int, const int);           /* -"- */
int          indexFromPoint (const PointXY, const int);       /* -"- */
int          indexFromXY (const int, const int, const int);   /* -"- */
void         assign_features (SEXP, SEXP);                    /* object_counting.c */

#ifdef USE_GTK
void         _doIter (void *);                    /* tools.c */
GdkPixbuf *  newPixbufFromImages (Image *, int);  /* conversions.c */
#ifdef WIN32
void         _doIterWin32 ();                     /* tools.c */
#endif
#endif

/* will be substituted by gettext internationalisation */
#ifndef GLIB_GETTEXT
    char *   _(char *);
    char *   N_(char *);
#endif

/*
    void R_init_EBImage(DllInfo *); and void R_unload_EBImage(DllInfo *);
    are exported in init.c
*/

#ifdef __cplusplus
};
#endif

/* _COMMON_H_LOCAL */
#endif
