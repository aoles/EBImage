\name{distmap}

\alias{distmap}

\concept{distance map}
\concept{distance map transform}

\title{ Distance map transform of binary images }

\description{
  Distance map transform of binary images. 
}

\section{ Usage }{

  \code{distmap(x, tolerance=0.05, minBG=0.05, strict=FALSE, ...)}

}

\section{ Arguments }{ \describe{


  \item{\code{x}}{An object of \code{\link{Image-class}} in the 
    \code{\link{Grayscale}} mode. }

  \item{\code{tolerance}}{ A numeric value in the range \code{[0,1)}. Image 
    pixels that differ from background (zero) by less than this value will be
    considered as background. This enables image-wide single value threshloding
    if necessary. } 

  \item{\code{minBG}}{ An estimated minimum fraction of background. If the 
    estimated number of background pixels is smaller than this value the 
    algorithm will terminate to avoid unnecessarily lengthy computations. This 
    is a protection mechanism in case a very noise image is supplied without 
    first  thresholding it. }
  
  \item{\code{sctrict}}{ If \code{TRUE}, single-pixel diagonal distances 
    are calculated exactly (i.e. \code{\link{sqrt}(2)}), otherwise these are
    set to be 1, same as neighbouring pixels on the vertical and horizontal
    axes. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  An object of \code{\link{Image-class}} in the \code{\link{Grayscale}} mode
  with pixels containing \code{\link{floor}} of distances to the nearest 
  background. If \code{strict} is set to \code{TRUE}, single-pixel diagonal 
  distances are calculated exactly (i.e. \code{\link{sqrt}(2)}), otherwise these 
  are set to be 1, same as neighbouring pixels on the vertical and horizontal
  axes.To preview the results visually, use 
  \code{\link{display}( \link{normalize}(result) )}.
    
}

\section{ Details }{

  Given a binary or grayscale image with zero background, a distance map 
  transformed image contains in each pixel a distance to the closest background 
  pixel (zero). This function is used extensively in object detection. Distance 
  map is a usual input for \code{\link{watershed}} and other segmentation 
  algorithms that are used to identify and separate closely positioned objects.
    
}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{thresh}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

