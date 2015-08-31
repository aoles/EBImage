\name{rmObjects}

\alias{rmObjects}
\alias{reenumerate}

\concept{delete objects}

\title{Object removal and re-indexation}

\description{
  The \code{rmObjects} functions deletes objects from an image
  by setting their pixel intensity values to 0.
  \code{reenumerate} re-enumerates all objects in an image from 0 (background)
  to the actual number of objects.
}

\usage{
  rmObjects(x, index, reenumerate = TRUE)

  reenumerate(x)
}

\arguments{
 \item{x}{An \code{Image} object in \code{Grayscale} color mode or an
    array containing object masks. Object masks are sets of pixels
    with the same unique integer value.}

  \item{index}{A numeric vector (or a list of vectors if \code{x} contains multiple frames) 
    containing the indexes of objects to remove in the frame.}
    
  \item{reenumerate}{Logical, should all the objects in the image be re-indexed afterwards (default).}
}

\value{
 An \code{Image} object or an array, containing the new objects.
}

\seealso{ 
  \code{\link{bwlabel}}, \code{\link{watershed}}
}

\examples{
  ## make objects
  x = readImage(system.file('images', 'shapes.png', package='EBImage'))
  x = x[110:512,1:130]
  y = bwlabel(x)
  
  ## number of objects found
  max(y)
  
  display(normalize(y), title='Objects')
  
  ## remove every second letter
  objects = list(
    seq.int(from = 2, to = max(y), by = 2),
    seq.int(from = 1, to = max(y), by = 2)
    )
  z = rmObjects(combine(y, y), objects)
  
  display(normalize(z), title='Object removal')
  
  ## the number of objects left in each image
  apply(z, 3, max)
  
  ## perform object removal without re-enumerating
  z = rmObjects(y, objects, reenumerate = FALSE)
  
  ## labels of objects left
  unique(as.vector(z))[-1L]
  
  ## re-index objects
  z = reenumerate(z)
  unique(as.vector(z))[-1L]
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}
