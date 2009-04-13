\name{rmObjects}

\alias{rmObjects}
\alias{reenumerate}

\concept{delete objects}

\title{Object removal and reindexation}

\description{
  The \code{rmObjects} functions deletes objects from an image
  by setting their pixel intensity values to 0.
  \code{reenumerate} re-enumerates all objects in an image from 0 (background)
  to the actual number of objects.
}

\usage{
  rmObjects(x, index)

  reenumerate(x)
}

\arguments{

 \item{x}{An \code{Image} object in \code{Grayscale} color mode or an
    array containing object masks. Object masks are sets of pixels
    with the same unique integer value.}

  \item{index}{A numeric vector (or a list of vectors if \code{x} contains multiple frames) 
    containing the indexes of objects to remove in the frame.}
}

\value{
 An \code{Image} object or an array, containing the new objects.

}

\seealso{ 
  \code{\link{bwlabel}}, \code{\link{watershed}}}

\examples{
  ## make objects
  x = readImage(system.file('images', 'shapes.png', package='EBImage'))
  x = x[110:512,1:130]
  y = bwlabel(x)
  if (interactive()) display(normalize(y), title='Objects')
  
  ## remove and reenumerate
  y = rmObjects(y, 5)
  if (interactive()) display(normalize(y), title='Removal')
  y = reenumerate(y)
  if (interactive()) display(normalize(y), title='Reenumerated')
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}



