\name{normalize}


\alias{normalize}

\concept{normalization}

\title{Intensity values linear scaling}

\description{
  Linearly scale the intensity values of an image to a specified range.
}

\usage{
  normalize(x, separate=TRUE, ft=c(0,1))
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{separate}{If \code{TRUE}, normalizes each frame separately.}

  \item{ft}{ A numeric vector of 2 values, target minimum and maximum 
    intensity values after normalization. }
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\examples{
if (FALSE) {
  x = readImage(system.file("images", "lena.gif", package="EBImage"))
  if (interactive()) display(x)
}
}



