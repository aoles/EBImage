\name{filter2}

\alias{filter2}
\alias{filter2,array,matrix-method}
\alias{filter2,Image,matrix-method}
\alias{filter2,array,missing-method}
\alias{filter2,Image,missing-method}

\alias{mkball}
\alias{mkbox}

\title{2D Convolution Filter}

\description{
  2D convolution-based linear filter for images and matrix data.
}

\usage{
  \S4method{filter2}{Image, matrix}(x, filter, ...)

  mkball(n, shape="step")
  mkbox(n)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} in Grayscale mode, a numeric 
    \code{array} or a \code{matrix}. }

  \item{filter}{A square matrix with odd dimensions.}

  \item{n}{A positive integer of length 1, specifying the size (number
  of rows and columns) of the returned square matrix.}

  \item{shape}{A character vector of length 1, with one of two values:
  \code{step} for a step function, \code{ball} for a semisphere.} 
  
  \item{...}{Further arguments.}
}

\value{
  \code{filter2} returns a transformed object of the same class as 
  \code{x}.
  
  \code{mkball} and \code{mkbox} return a square matrix that can be used
  for the \code{filter} argument of \code{filter2}. Its dimensions
  (number of rows and columns) are given by \code{n}. For
  \code{shape="step"}, the entries of the matrix are the integers 0
  and 1, and the 1s correspond to the interior of a circle of radius 
  \code{n/2} around the center element of the matrix.
  For \code{shape="ball"}, the non-zero elements of
  the matrix are \code{z = sqrt(radius^2 - x^2 - y^2)}, a
  parameterization of a semisphere.
}

\details{

  The convolution filter is based on \code{fft} transforms.
  If \code{x} is an \code{array}, the filter is applied per frame (as to images).
  
}

\seealso{
  \code{
    \linkS4class{Image}, \link{fft}
  }
}

\author{Gregoire Pau, \email{gpau@ebi.ac.uk}}

