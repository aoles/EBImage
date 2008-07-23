\name{distmap}

\alias{distmap}
\alias{distmap,Image-method}

\title{ Distance map transform of binary images }

\description{
  The function computes a distance map transformation of a binary image,
  i.e. of an image whose pixels are labeled as \emph{foreground} and
  \emph{background}. 
  In the distance map, each pixel contains the distance from that pixel to the nearest
  background pixel, or to the border of the image.
  Note: this is a trivial brute force implementation. Please look
  elsewhere for efficient implementations.
}

\usage{
  \S4method{distmap}{Image}(x, t=0.05, exact=FALSE, bg=0.05, ...)
}

\arguments{

  \item{x}{A \code{\link{Grayscale}} object of \code{\linkS4class{Image}}.
    \code{x} is considered as a binary image, consisting of 0's for background
    and all other values for foreground. }

  \item{t}{A numeric vector of length 1, the background threshold in the range 
    \code{[0,1)}. Pixels with intensity smaller than \code{t} are considered 
    as background. } 

  \item{bg}{A numeric numeric vector of length 1, the allowed minimum of the fraction of 
    background pixels in the image. Deprecated.}
  
  \item{exact}{A logical vector of length 1, controlling the metric that
    is used (L1 vs L2). \code{TRUE} corresponds to L1.}
  
  \item{...}{Further arguments.}
}

\value{
  A \code{\link{Grayscale}} object of \code{\linkS4class{Image}} with pixels
  containing the \code{\link{floor}} values of their distances to the nearest 
  background points. 
}

\seealso{
  \code{
    \linkS4class{Image}, \link{watershed}, \link{thresh}, \link{propagate}
  }
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006
}

\examples{
  \dontrun{see ?watershed for an example}
}


