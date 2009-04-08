\name{denoise}

\alias{blur}
\alias{gblur}

\title{Blurring images}

\description{
  Blur images with ImageMagick functions.
}

\usage{
  blur(x, r=0, s=0.5)
  gblur(x, r=0, s=0.5)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{r}{A numeric value for the radius of the pixel neighbourhood.
    The default value 0 enables automatic radius selection.}

  \item{s}{The standard deviation of the Gaussian filter used for
    blurring. For reasonable results, in most functions \code{r} must be
    larger than \code{s}. }
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  \code{blur} uses an unspecified separable kernel. \code{gblur} uses
  a Gaussian kernel. The algorithms used by these ImageMagick functions
  are not well defined.
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
   x = readImage(system.file("images", "lena.gif", package="EBImage"))
   if (interactive()) display(x)

   y = blur(x, r=3, s=2)
   if (interactive()) display(y, title='blur(x, r=3, s=2)')

   y = gblur(x, r=3, s=2)
   if (interactive()) display(y, title='gblur(x, r=3, s=2)')
}

