\name{equalize}

\alias{equalize}


\title{Histogram equalization}

\description{
  Equalize the histogram of an image.
}

\usage{
  equalize(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  The algorithm used by this ImageMagick function is not well defined.
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\examples{
  x = readImage(system.file("images", "lena.gif", package="EBImage"))
  if (interactive()) display(x)
  
  y = equalize(x)
  if (interactive()) display(y, title='equalize(x)')
}

