\name{filter2}

\alias{filter2}

\title{2D Convolution Filter}

\description{
  2D convolution-based linear filter for images and matrix data.
}

\usage{
  filter2(x, filter)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} in Grayscale mode, a numeric 
    \code{array} or a \code{matrix}. }

  \item{filter}{A square \code{matrix} with odd dimensions.}
}

\value{
  \code{filter2} returns a transformed object of the same class as 
  \code{x}.
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

