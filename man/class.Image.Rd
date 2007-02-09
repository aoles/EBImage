\name{Image-class}

\docType{class}

\alias{Image-class}

\alias{colorMode}
\alias{colorMode<-}
\alias{compression}
\alias{compression<-}
\alias{fileName}
\alias{fileName<-}
\alias{imageData}
\alias{imageData<-}
\alias{resolution}
\alias{resolution<-}
\alias{sampleFilter}
\alias{sampleFilter<-}

\alias{Grayscale}
\alias{TrueColor}

\concept{image representation}
\concept{image analysis}
\concept{image processing}
\concept{image class}
\concept{image object}
\concept{color mode}

\title{ Image class defintion and accessor methods }

\description{
  \code{Image} is the base class to hold data and parameters of grayscale and
  RGB images in \code{R}. The class is derived from \code{\link{array}} and
  inherits all of its properties and methods, e.g. mathematical operations
  on arrays, subsetting, histograms etc. Image processing routines are
  defined additionally as S4 methods for objects of this class. In many cases
  they also return objects of class \code{Image}.
}

\section{ Constants }{

  The following constant are defined in the package and should be used everywhere
  where image color mode needs to be specified.

  \preformatted{
    Grayscale = as.integer(0)
    TrueColor = as.integer(1)
  }
}

\section{ Accessor methods }{

  \code{# S4 methods for class 'Image':}

  \describe{

    \item{\code{colorMode(x)}, \code{colorMode(x) <- value}}{ Gets and
      sets image color mode. \code{value} is one of the constants above.
      Default: \code{Grayscale}. }

    \item{\code{compression(x)}, \code{compression(x) <- value}}{ Gets
      and sets compression algorithm used when image saved in a format
      supporting compression. For exapmple, TIFF images support ZIP and
      LZW lossless compressions, whereas JPEG uses JPEG compression by default.
      Possible values of type \code{\link{character}} are \code{'NONE', 'LZW',
      'ZIP', 'JPEG', 'BZIP'} and \code{'GROUP4'}. Some of these may be
      unavailable on your system if \code{ImageMagick} was compiles without
      their support. Default: \code{'LZW'}. }

    \item{\code{features(x)}}{ Is a read only method that returns a
      \code{\link{list}} or matrices with descriptors of detected objects.
      If object detection was executed on the image, an empty list is
      returned. Otherwise, the number of elements matches the number of
      frames in the image. Default: \code{list()}. }

    \item{\code{fileName(x)}, \code{fileName(x) <- value}}{ Gets and
      sets the file name. When reading images this value is updated,
      when writing it can be used as default if no alternative is specified.
      When reading a stack, the first file name in the list of stack images
      will be assigned. Default: \code{'no-name'}. }

    \item{\code{imageData(x)}, \code{imageData(x) <- value}}{ Gets
      and sets image data. Image data is a 3D array with the last dimension
      matching the number of 2D images in a stack. The atomic type
      of the array is defined by \code{colorMode()}. }

    \item{\code{resolution(x)}, \code{resolution(x) <- value}}{ Gets
      and sets values of image resoultion: a numeric vector of two values
      of resolution in \code{x} and \code{y} dimensions. Generally resolution
      is understood in pixel-per-inch (ppi) or dots-per-inch (dpi), however
      some microscopes can save these values in images in different units.
      It is the responsibility of the user to identify the unit and keep the
      units comparable between different images in the same project if
      required: the package does not take care of this. The default values
      are equal for both directions, 2.5e+6 dpi. }

    \item{\code{sampleFilter(x)}, \code{sampleFilter(x) <- value}}{
      Gets and sets the filtering algorithm used when resizing images using
      \code{\link{resize}} and other image transformation routines. Possibly
      this value will be removed and supplied as argument to the
      corresponding function. At the moment, possible values include:
      \code{'point', 'box', 'triangle', 'hermite', 'hanning', 'hamming',
      'blackman', 'gaussian', 'quadratic', 'cubic', 'catrom', 'mitchell',
      'lanczos', 'bessel'} and \code{'sinc'}. Default: \code{'lanczos'}. }
  }

  R does not have a concept of private/public class members, therefore, the
  following cannot be enforced and are just recommendations: use only accessor
  methods to get/set slot data; and consequently -- everything that does not
  have an accessor method should not be accessed directly by the user. For this
  reason, we do not describe slots here: consider all slots as private. Package
  developers can have a look at the code and see the full object structure.
}

\section{ Creating objects }{
  Objects can be created using \code{\link{new}("Image")} supplying slot data
  if necessary. Additionally, wrapper functions are defined to simplify the
  construction of \code{Image} objects in different situations.

  See \code{\link{Image}} constructor and \code{\link{read.image}} function on
  details how to create images or read images from disk or network.
}

\section{ Details }{
  Image data are stored as 3D arrays,
  with first two dimensions specifying image/frame size and third -- number
  of frames. A single 2D image will have the number of frames 1. In subsetting,
  the value of \code{drop} parameter is set to \code{FALSE}, thus image
  dimensionality is preserved at 3.

  At the moment, images can store data either as grayscale values or true color values.
  Grayscale values are stored in \code{\link{numeric}} or \code{\link{double}}
  arrays, whereas true color data are stored as \code{\link{integer}} arrays.
  I.e. the type of image array with grayscale data can be tested as:
  \code{is.array(x) && is.numeric(x) && !is.integer(x)} whereas for true color
  data the test would look like: \code{is.array(x) && is.integer(x)}.
  Correspondingly, the data slots are created with
  \code{array(as.numeric(val),...)} for grayscales and
  \code{array(as.integer(val),...)} for true color images. Grayscale values are
  assumed to be in the range \code{[0,1]}, although this is not a requirement.
  However, many image processing functions will also assume data in this range
  or will generate wrong results if the data do not fit the range.
}

\seealso{
  \code{
    \link{Image}, \link{image.Image}, \link{display}, \link{channel}
  }
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}, LGPL >= 2.1
}

\examples{
  if ( interactive() ) {
    \dontrun{creates a new Grayscale image of shifted stripes}
    a <- Image( (0:100)*0.01, c(60,60) )
    display( a )
    print( a )

    \dontrun{converts image mode to TrueColor, compare print's}
    b <- a
    colorMode( b ) <- TrueColor
    print( b )

    \dontrun{fills values with brightness > 50\% with red}
    b[ a > 0.5 ] = as.integer( 255 )
    display( b )
  }
}

\keyword{methods}
\keyword{classes}
