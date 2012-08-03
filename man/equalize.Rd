\name{equalize}
\alias{equalize}
\title{Histogram equalization}

\description{
  This function is deprecated. Use standard R functions such as \code{rank} or \code{cut} instead.
  This function calls ImageMagick's \code{equalize}, which 'equalizes
  the histogram of an image', whatever that means.
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


