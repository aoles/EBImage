\name{distmap}

\alias{distmap}

\concept{distance map}
\concept{distance map transform}

\title{ Distance map transform of binary images }

\description{
  The function performs a distance map transform of binary images. In a distance
  map each pixel contains the distance from that pixel to the nearest background
  point.
}

\section{ Usage }{
  \preformatted{
    ## S4 method for signature x='Image'  
  
    distmap(x, t=0.05, exact=FALSE, bg=0.05)
}}

\section{ Arguments }{ \describe{


  \item{\code{x}}{A \code{\link{Grayscale}} object of \code{\link{Image-class}}.
    \code{x} is considered as a binary image, consisting of 0's for background
    and all other values for foreground. }

  \item{\code{t}}{ A numeric value for the background threshold in the range 
    \code{[0,1)}. Pixels with intensity smaller than \code{t} are considered 
    as background. } 

  \item{\code{bg}}{ A numeric value for the allowed minimum for the fraction of 
    background pixels in the image. The default and recommended value 
    corresponds to 5\% (0.05). See details for more information. }
  
  \item{\code{exact}}{ A logical value if the distance to the neighboring diagonal
    elements should be calculated exactly, as \code{\link{sqrt}(2)}, or it should
    be set to 1, same as that for the neighbouring pixels on the vertical and 
    horizontal axes. }
}}

\section{ Value }{

  A \code{\link{Grayscale}} object of \code{\link{Image-class}} with pixels
  containing the \code{\link{floor}} values of their distances to the nearest 
  background points. The range of the resulting image is \code{[0,n]}, where
  \code{n}, a positive integer, will be the distance from center of the largest 
  object.
  
  To preview the results visually, use \code{\link{display}(\link{normalize}(result))}.
}

\section{ Details }{

  Given a binary or gray scale image with background set to 0, a distance map 
  transform generates an image each pixel of which contains the distance from that
  pixel to the nearest background pixel (zero) or the edge of the image. 
      
  If foreground constitutes more than 95\% of the image, computation of a distance 
  map can be very lengthy as the function uses an iterative algorithm. This is a 
  very rare situation anyway and the value of \code{bg} provides a protection 
  mechanism, e.g. in case a very noise image is supplied performing a threshold 
  first.

  This function is used extensively in object detection. Distance map is a standard 
  input for the \code{\link{watershed}} algorithm used to identify and separate 
  closely positioned objects.
}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{thresh}, \link{propagate}
  }
}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2006
}

\examples{
  \dontrun{see ?watershed for an example}
}


\keyword{file}

