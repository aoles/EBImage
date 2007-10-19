\name{filter2}

\alias{filter2}
\alias{filter2,array,matrix-method}
\alias{filter2,Image,matrix-method}
\alias{filter2,array,missing-method}
\alias{filter2,Image,missing-method}
\alias{sharpen2}
\alias{sharpen2,Image-method}

\alias{mkball}
\alias{mkbox}

\concept{image color enhancement}

\title{ 2D Convolution Filter }

\description{
  2D convolution-based linear filter for images and matrix data.
}

\usage{

  ## S4 methods for signatures 'x=Image' or 'x=array' and
  ## 'filter=matrix' or 'filter=missing'
  filter2(x, filter, ...)
  
  \S4method{sharpen2}{Image}(x, ...)
  
  mkball(n=15, pow=1)
  mkbox(n=15)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} in Grayscale mode, a numeric 
    \code{array} or a \code{matrix}. }

  \item{filter}{ A square matrix with odd dimensions. }

  \item{n}{The size of the filter matrix (odd number). }

  \item{pow}{ The power of the function used to fill in the matrix. } 
  
  \item{...}{ Reserved. }
}

\value{

  \code{filter2} and \code{} return a transformed object of the same class as 
  \code{x}.
  
  \code{mkball} and \code{mkbox} return a square matrix that can be used for 
  \code{filter2}.
    
}

\details{

  The convolution filter is based on \code{fft} transforms.

  If \code{x} is an \code{array}, the filter is applied per frame (as to images).
  
  \code{sharpen2} uses \code{filter2} to sharpen grayscale images by subtracting
    a slightly blurred image with the center pixel excluded.
    
}

\seealso{
  \code{
    \linkS4class{Image}, \link{fft}
  }
}

\author{
  \code{filter2} author: Gregoire Pau, \email{gpau@ebi.ac.uk}, 2007
  
  \code{sharpen2} author: Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\keyword{manip}

