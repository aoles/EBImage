\name{fillHull}

\alias{fillHull}

\title{Fill holes in objects}

\description{
  Fill holes in objects.
}

\usage{
fillHull(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  \code{fillHull} fills holes in the objects defined in \code{x}, where
   objects are sets of pixels with the same unique integer value.
}

\seealso{
  \code{\link{bwlabel}}
}

\author{
  Gregoire Pau, Oleg Sklyar; 2007
}

\examples{
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  if (interactive()) display(x)

  y = thresh(x, 10, 10, 0.05)
  if (interactive()) display(y, title='Cell nuclei')

  y = fillHull(y)
  if (interactive()) display(y, title='Cell nuclei without holes')
}
