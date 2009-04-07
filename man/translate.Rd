\name{translate}

\alias{translate}

\title{Image translation}

\description{
  Translates an image.
}

\usage{
  translate(x, v)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  \item{v}{The translation vector or a matrix of translation vectors
    if \code{x} contains several images.}
}

\value{
  An \code{Image} object or an array, containing the translated version
  of \code{x}.
}

\details{
  Borders are repeated during translation.
}

\seealso{ \code{resize}, \code{rotate}
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\examples{
  x = readImage(system.file("images", "lena-color.png", package="EBImage"))
  y = translate(x, c(20,20))
  if (interactive()) {
    display(x, title='Lena')
    display(y, title='Translated lena')
  }

  ## gradient
  y = translate(x, c(1,1))
  if (interactive()) display(0.5+4*(y-x), title='NE gradient')
}
