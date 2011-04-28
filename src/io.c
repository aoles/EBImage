#include "io.h"

/* -------------------------------------------------------------------------
Image I/O
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"
#include "zlib.h"
#include "assert.h"
#include <R_ext/Error.h>
#include <magick/ImageMagick.h>

/* These are to use GTK */
#ifdef USE_GTK
#   include <gtk/gtk.h>
#   ifdef WIN32
        typedef unsigned long ulong;
#       include <sys/types.h>
#   else
#       include <gdk/gdkx.h>
#   endif
#endif

/*----------------------------------------------------------------------- */
// GP: mode = -1 will be automatically guessed from the file
SEXP
lib_readImages (SEXP files, SEXP mode) {
    SEXP res;
    int _mode, i, nappends;
    Image * image, * images;
    ImageInfo * image_info;
    ExceptionInfo exception;
    const char * file;
    ImageType it;

    if ( LENGTH(files) < 1 )
        error ( "please supply at least one file name or URL" );
    _mode = INTEGER (mode)[0];
    if ( _mode < -1 || _mode > MODE_MAX)
        error ( "requested mode is not supported" );

    // Special call for reading Cellomics image
    if (LENGTH(files)==1) {
      file = CHAR(STRING_ELT(files, 0));
      i = strlen(file);
      if (i>4 && (strncmp(&file[i-4], ".c01", 4)==0 || strncmp(&file[i-4], ".C01", 4)==0)) {
	return (readCellomics(file));
      }
    }

    image_info = (ImageInfo *) NULL;
    /* images loaded into image and moved into this list */
    images = NewImageList ();
    GetExceptionInfo (&exception);
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    nappends = 0;

    for ( i = 0; i < LENGTH (files); i++ ) {
        if ( LENGTH (files) > 1 )
            file = CHAR ( STRING_ELT(files, i) );
        else
            file = CHAR ( asChar(files) );
        strcpy (image_info->filename, file);

	// Prevent an ImageMagick bug when file is an empty string or NULL
	if (file==NULL) image=NULL;
	else if (strlen(file)==0) image=NULL;
	else {
	  image = ReadImage (image_info, &exception);
	  CatchException (&exception);
	}
        if ( image == (Image *)NULL ) {
            warning ("requested image not found or could not be loaded" );
            continue;
        }

	// Automatic color mode guess
	if (_mode==-1) {
	  it = GetImageType(image,&exception);
	  // Rprintf("it=%d G=%d P=%d PM=%d\n",it, GrayscaleType, PaletteType, PaletteMatteType);
	  if (it==BilevelType || it==GrayscaleType || it==GrayscaleMatteType) _mode=MODE_GRAYSCALE;
	  else _mode=MODE_COLOR;
	}
       
        /* do not destroy image here */
        AppendImageToList (&images, image);

        if ( nappends == 0 ) {
            /* set all attributes from the first image */
            strcpy (images->filename, image->filename);
            images->compression = image->compression;
            images->x_resolution = image->x_resolution;
            images->y_resolution = image->y_resolution;
        }
        nappends++;
    }
    /* do not update image properties here because if no image was added to
    the list it will cause segfault, or use GetImageListLength first to check size */
    image_info = DestroyImageInfo (image_info);

    /* convert image list into R object */
    res = PROTECT(magick2SEXP (images, _mode));
    images = DestroyImageList (images);

    DestroyExceptionInfo(&exception);

    UNPROTECT(1);

    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_writeImages (SEXP x, SEXP files, SEXP quality) {
    int nz, nfiles, i;
    Image * images, * image;
    ImageInfo *image_info;
    ExceptionInfo exception;

    /* basic checks */
    validImage(x,0);

    images = sexp2Magick (x);
    nz = GetImageListLength(images);
 
    nfiles = LENGTH (files);
    if ( nfiles != 1 && nfiles != nz)
        error ( "number of files must be 1, or equal to the size of the image stack" );
    
    if ( images == NULL || GetImageListLength (images) < 1 )
        error ( "cannot write an empty image" );
    GetExceptionInfo (&exception);
    image_info = CloneImageInfo ( (ImageInfo *)NULL );
    /* set attributes in image_info*/
    image_info->compression = images->compression;
    image_info->quality = (unsigned int) INTEGER (quality)[0];
    if ( nfiles == 1 ) {
    /* save into a single file, TIFF, GIF, or automatically add file suffixes */
        strcpy (image_info->filename, CHAR(STRING_ELT(files, 0)) );
        /* we want to overwrite the feature imported from SEXP image */
        strcpy (images->filename, image_info->filename);
	images->quality = image_info->quality;
        WriteImages(image_info, images, CHAR(STRING_ELT(files, 0)), &exception);
        CatchException (&exception);
    }
    else {
    /* save each frame into a separate file */
        for ( i = 0; i < nz; i++ ) {
            image = GetImageFromList (images, i);
            if ( image == NULL || GetImageListLength (image) < 1 ) {
                warning ( "cannot write an empty image, skipping" );
                continue;
            }
            strcpy (image_info->filename, CHAR(STRING_ELT(files, i)));
            /* we want to overwrite the feature imported from SEXP image */
            strcpy (image->filename, image_info->filename);
	    image->quality = image_info->quality;
            WriteImage (image_info, image);
            CatchException (&image->exception);
            // WriteImages(image_info, image, CHAR(STRING_ELT(files, i)), &exception);
            // CatchException (&exception);

        }
    }

    image_info = DestroyImageInfo (image_info);
    images = DestroyImageList (images);
    DestroyExceptionInfo(&exception);
    return R_NilValue;
}

#define CHUNK 65536
int inflateData(FILE *source, unsigned char **_dat, int *_datsize) {
  int ret;
  unsigned have;
  z_stream strm;
  unsigned char in[CHUNK];
  unsigned char out[CHUNK];
  int datmaxsize;
  unsigned char *dat;
  int datsize;

  // allocate dat with a fixed size ; will be reallocated if necessary
  datmaxsize = CHUNK*2;
  datsize = 0;
  dat = (unsigned char *)malloc(datmaxsize);

  // allocate inflate state
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  strm.avail_in = 0;
  strm.next_in = Z_NULL;
  ret = inflateInit(&strm);
  if (ret != Z_OK) return ret;

  // decompress until deflate stream ends or end of file
  do {
    strm.avail_in = fread(in, 1, CHUNK, source);
    if (ferror(source)) {
      (void)inflateEnd(&strm);
      return Z_ERRNO;
    }
    if (strm.avail_in == 0) break;
    strm.next_in = in;
    // run inflate() on input until output buffer not full
    do {
      strm.avail_out = CHUNK;
      strm.next_out = out;
      ret = inflate(&strm, Z_NO_FLUSH);
      assert(ret != Z_STREAM_ERROR);  // state not clobbered
      switch (ret) {
      case Z_NEED_DICT:
	ret = Z_DATA_ERROR;     // and fall through
      case Z_DATA_ERROR:
      case Z_MEM_ERROR:
	(void)inflateEnd(&strm);
	return ret;
      }
      have = CHUNK - strm.avail_out; 
      // reallocate dat if needed
      if (datsize + have >= datmaxsize) {
	datmaxsize = datmaxsize +  CHUNK*2;
	dat = realloc(dat, datmaxsize);
      }
      memcpy(&dat[datsize], out, have);
      datsize += have;
    } while (strm.avail_out == 0);
    
    // done when inflate() says it's done
  } while (ret != Z_STREAM_END);
  
  // clean up and return
  (void)inflateEnd(&strm);
  *_datsize = datsize;
  *_dat = dat;
  return ret == Z_STREAM_END ? Z_OK : Z_DATA_ERROR;
}

// File based on http://dev.loci.wisc.edu/trac/software/browser/trunk/components/bio-formats/src/loci/formats/in/CellomicsReader.java
SEXP readCellomics(const char *filename) {
  FILE *fin;
  unsigned char *dat, *pdat;
  int datsize;
  SEXP image, dim;
  int i, width, height, nplanes, nbits, compression;
  int nprotect;
  double *dimage;
  int ret;

  // init
  nprotect = 0;

  // open file
  fin = fopen(filename, "rb");
  if (!fin) error("readCellomics: cannot open file");
  
  // inflate zlib stream
  fseek(fin, 4, SEEK_SET);
  ret = inflateData(fin, &dat, &datsize);
  if (ret!=Z_OK) error("readCellomics: cannot decompress stream");
  fclose(fin);

  // read header
  width = *(int *)(&dat[4]); 
  height = *(int *)(&dat[8]); 
  nplanes = *(short *)(&dat[12]); 
  nbits = *(short *)(&dat[14]);
  compression = *(int *)(&dat[16]);
  if (width*height*nplanes*(nbits/8)+52 > datsize) {
    error("readCellomics: compressed mode is not yed supported");
  }

  // allocate new image
  image = PROTECT(allocVector(REALSXP, width * height * nplanes));
  nprotect++;
  if (nplanes==1) PROTECT(dim=allocVector(INTSXP, 2));
  else PROTECT(dim=allocVector(INTSXP, 3));
  nprotect++;
  INTEGER(dim)[0] = width;
  INTEGER(dim)[1] = height;
  if (nplanes>1) INTEGER(dim)[1] = nplanes;
  SET_DIM (image, dim);

  // copy planes
  dimage = REAL(image);
  pdat = &dat[52];
  if (nbits==8) {
    for (i=0; i<width*height*nplanes; i++) {
      *dimage++ = (*((unsigned char *)pdat))/256.0;
      pdat += sizeof(unsigned char);
    } 
  } else if (nbits==16) {
    for (i=0; i<width*height*nplanes; i++) {
      *dimage++ = (*((unsigned short *)pdat))/65536.0;
      pdat += sizeof(unsigned short);
    } 
  } else if (nbits==32) {
    for (i=0; i<width*height*nplanes; i++) {
      *dimage++ = (*((unsigned int *)pdat))/4294967296.0;
      pdat += sizeof(unsigned int);
    } 
  } else {
    free(dat);
    error("readCellomics: unsupported nbits/pixel mode");
  }
  
  // free dat
  free(dat);
  
  UNPROTECT(nprotect);
  return(image);
}
