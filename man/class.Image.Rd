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

\alias{Grayscale}
\alias{TrueColor}

\concept{image representation}
\concept{image analysis}
\concept{image processing}
\concept{image class}
\concept{image object}
\concept{color mode}

\title{ Defintion of class 'Image', its accessor methods }

\description{
  The class \code{Image} enables the storage and manipulation of image data for
  grayscale and true color (RGB) images in \code{R}. The class is based on 
  (derived from) \code{\link{array}} and inherits all its properties and methods,
  e.g. mathematical operations, subsetting, histograms etc. Image processing and
  analysis routines are defined as S4 methods for objects of the class \code{Image}.
}

\section{ Class Definition }{ 

The class is defined as follows:

\preformatted{
## S4 class definition

setClass( "Image",
  representation (
    colormode    = "integer", 
    filename     = "character",
    compression  = "character", 
    resolution   = "numeric",   
    features     = "list"
  ),
  contains = "array"
)
}

  Additionally, two constants are defined and exported that can be used to specify
  or test image data type:

\preformatted{
  Grayscale = as.integer(0)
  TrueColor = as.integer(1)
}
}

\section{ Creating objects }{
  Objects can be created using \code{\link{new}("Image")} supplying slot data
  as necessary. Additionally, wrapper functions are defined to simplify the
  construction of \code{Image} objects in different situations: the default 
  constructor, \code{\link{Image}}, the \code{\link{readImage}} constructor that
  creates an object from reading image files and the \code{\link{copy}} constructor.
}


\section{ Accessor methods }{

  Accessor methods are class methods that are defined to retrieve data values 
  from objects of a given class or to assigne those. These should be used instead
  of directly referring to the slots. Unfortunately, R does not have a concept 
  of private or protected class members: all slots have "public" access in terms
  of other object-oriented languages. Therefore, data encapsulation in sense of
  hiding the particular structure of the class is impossible. At the same time,
  it is still recommended to use accessor functions instead of directly accessing
  slots. One can also assume that any slot that does not have an accessor method
  should not be accessed directly by the user.

  The following accessor methods are defined as S4 methods for class \code{Image}:

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

  }
}

\section{ Details }{
  Image data are stored as 3D arrays with first two dimensions specifying 
  frame size and third the number of frames. A single 2D image has the number of 
  frames 1. In subsetting, the value of \code{drop} parameter is set to 
  \code{FALSE}, thus image dimensionality is preserved at 3.

  \code{Image} is derived from \code{\link{array}}, therefore, the data is stored
  in arrays. \code{Image} can store either grayscale or true color data at the
  moment. Grayscale data is stored in \code{\link{numeric}} arrays, whereas true
  color data is stored in \code{\link{integer}} arrays. Correspondingly, the data 
  is created with \code{array(as.numeric(val),dim(val))} for grayscale and
  \code{array(as.integer(val),dim(val))} for true color images. Grayscale values
  are assumed to be in the range \code{[0,1]}, although this is not a requirement
  in sense of data storage. Although, many image processing functions will 
  assume data in this range or will generate invalid results for the data out of 
  this range.
}

\seealso{\code{
  \link{Image}, \linkS4class{IndexedImage}, \link{display}, \link{channel}, 
  \link{image,Image-method}, \link{show,Image-method}, 
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{

  ## New Grayscale image of a black-to-white vertical gradient
  w <- 120
  a <- Image((0:(w^2))/w^2, c(w,w))
  if ( interactive() ) display(a)
  print(a)

  ## Converts image to TrueColor
  b <- a
  colorMode(b) <- TrueColor
  print(b)

  ## Fills values with intensities over 0.5 (50\%) with red
  b[ a > 0.5 ] = as.integer(255)
  if ( interactive() ) display(b)
}

\keyword{methods}
\keyword{classes}
