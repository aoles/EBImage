\name{gblur}

\alias{gblur}

\title{Low-pass Gaussian filter	}

\description{
  Filters an image with a low-pass Gaussian filter.	
}

\usage{
  gblur(x, sigma, radius = 2 * ceiling(3 * sigma) + 1)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{sigma}{A numeric denoting the standard deviation of the Gaussian filter
    used for blurring.}

  \item{radius}{The radius of the filter in pixels. Default is \code{2*ceiling(3*sigma)+1)}. }
}

\value{
  An \code{Image} object or an array, containing the filtered version
  of \code{x}.
}

\details{
The Gaussian filter is created with the function \code{makeBrush}.	
}

\seealso{
  \code{\link{filter2}},  \code{\link{makeBrush}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{
   x = readImage(system.file("images", "lena.png", package="EBImage"))
   if (interactive()) display(x)

   y = gblur(x, sigma=8)
   if (interactive()) display(y, title='gblur(x, sigma=8)')
}

