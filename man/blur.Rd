\name{denoise}

\alias{blur}
\alias{gblur}

\title{Blurring images}

\description{
  Blurs an image with ImageMagick functions.
}

\usage{
  gblur(x, r=0, s=0.5)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{r}{A numeric value for the radius of the pixel neighbourhood.
    The default value 0 enables automatic radius selection.}

  \item{s}{The standard deviation of the Gaussian filter used for
    blurring. For reasonable results, \code{r} must be larger
    than \code{s}. }
}

\value{
  An \code{Image} object or an array, containing the blurred version
  of \code{x}.
}

\details{
  \code{blur} uses an unspecified separable kernel. \code{gblur} uses
  a Gaussian kernel. The algorithms used by these ImageMagick functions
  are not well defined and hence, the usage of \code{filter2} is preferable
  to \code{blur} or \code{gblur}.
}

\seealso{
  \code{filter2}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{
   x = readImage(system.file("images", "lena.png", package="EBImage"))
   if (interactive()) display(x)

   y = gblur(x, r=3, s=2)
   if (interactive()) display(y, title='gblur(x, r=3, s=2)')
}

