\name{image,Image-method}

\docType{class}

\alias{hist,Image-method}
\alias{show,Image-method}
\alias{image,Image-method}
\alias{[,Image-method}
\alias{Arith}

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

    \item{\code{show(object)} }{ Prints a summary of
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
      
    \item{\code{[}}{ Redefined versions (multiple signatures) of the
      \code{\link{array}} subsetting operator. These return objects of class 
      \code{Image} without dropping dimensions. }

    \item{\code{Arith}}{ Redefined arithmetic operators acting on any 
      combination of \code{Image} with another \code{Image} or \code{array}
      will return an object of class \code{Image}. }
  }
  
  Other important methods that directly applicable due to inheritance from the 
  \code{\link{array}} class, just to stress that they can and should be used:
  
  \describe{

    \item{\code{\link{dim}(x)} and \code{dim(x) <- value} }{ Gets and sets image
      dimensions. }
    \item{\code{\link{fft}(z, inverse = FALSE)}}{ Performs the Fast Fourier
      Transformation on an Image. }
    \item{\code{\link{sqrt}(x)} etc.}{ Miscellaneous mathematical functions. }
  }    
}

\seealso{ 
  \code{
    \link{Image-class}, \link{IndexedImage-class}
  }
}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{methods}
\keyword{classes}
