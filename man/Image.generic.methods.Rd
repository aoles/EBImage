\name{image,Image-method}

\docType{class}

\alias{hist.Image}
\alias{print.Image}
\alias{image.Image}
\alias{[,Image-method}

\concept{image processing}
\concept{image class}
\concept{image object}

\title{ Generic methods for objects of class Image }

\description{
  \code{\link{Image-class}} supports all (or almost all) operations that are 
  defined for \code{\link{array}}. However, the default behavior of some 
  generics was redefined in the package to better suit the class. Those are:
}

\section{ Generic methods }{
  
  \describe{

    \item{\code{print(x, ...)} and \code{show(object)} }{ Prints a summary of
      image and properties (as described in 'Accessor methods' section). Prints
      also an excerpt of data keeping it to the minimum just to have a visual 
      control over the atomic data type. Use\code{imageData(x)} to print or
      assign the full dataset. If image was put through the object detection
      algorithm a summary of detected objects will be printed instead of data. }

    \item{\code{as.array(x)}}{ Removed to avoid incompatibilities with R < 2.5. 
      Use \code{\link{imageData}} instead. }
  
    \item{\code{as.matrix(x, ...)}}{ Removed to avoid incompatibilities with 
      R < 2.5. Use \code{as.matrix(\link{imageData}(x))} instead. }

    \item{\code{hist(x, ...)}}{ Slightly modyfied to better suit the 
      assumed grayscale data range \code{[0,1]}.  }

    \item{\code{image(x, ...)}}{ Draws image data using \code{graphics::image} 
      keeping image aspect ratio. }
      
    \item{\code{[}}{ Redefined versions (with different argument classes) of 
      \code{\link{array}} subsetting operator. This returns objects of class 
      \code{Image} without dropping dimensions. }
  }
  
  Other important methods that are taken over from \code{\link{array}} class.
  These were not modified in the package and work in exactly the same manner as
  for array. We just want to stress that they can and should be used:
  
  \describe{

    \item{\code{\link{dim}(x)} and \code{dim(x) <- value} }{ Gets and sets image
      dimensions. }
    \item{\code{\link{fft}(z, inverse = FALSE)}}{ Performs the Fast Fourier
      Transformation on an Image. }
    \item{\code{\link{sqrt}(x)}}{ Miscellaneous mathematical functions. }
  }    
}

\seealso{ 
  \code{
    \link{Image-class}, \link{Image}
  }
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}
}

\keyword{methods}
\keyword{classes}
