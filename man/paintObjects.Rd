\name{paintObjects}

\alias{paintObjects}

\title{Marks objects in images}

\description{
  This function marks objects in images.
}

\usage{
  paintObjects(x, tgt, opac=c(1, 1), col=c('red', NA))
}

\arguments{
   \item{x}{An \code{Image} object in \code{Grayscale} color mode or an
    array containing object masks. Object masks are sets of pixels
    with the same unique integer value.}
    
  \item{tgt}{An \code{Image} object or an array, containing the
    intensity values of the objects.}

  \item{opac}{A numeric vector of two opacity values for drawing object
    boundaries and object bodies. Opacity ranges from 0 to 1, with 0
    being fully transparent and 1 fully opaque.}

  \item{col}{A character vector of two R colors for drawing object
    boundaries and object bodies. By default, object boundaries are
    painted in \code{red} while object bodies are not painted.}
}

\value{
  An \code{Image} object or an array, containing the painted version
  of \code{tgt}.
}

\seealso{
  \code{\link{bwlabel}}, \code{\link{watershed}}, \code{link{getFeatures}}
}

\examples{
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  x = x[,,1]
  if (interactive()) display(x)

  ## computes object mask
  y = thresh(x, 10, 10, 0.05)
  y = opening(y, makeBrush(5, shape='disc'))
  mask = bwlabel(y)

  ## moments
  mom = moments(mask, x)

  ## highlight nuclei with an eccentricity higher than 0.85
  maskb = mask
  id = which(mom[,'m.ecc']<0.85)
  maskb[!is.na(match(maskb, id))] = 0
  img = paintObjects(maskb, channel(x, 'rgb'), col='red')

  ## highlight nuclei with an intensity lower than 150
  maskb = mask
  id = which(mom[,'m.int']>150)
  maskb[!is.na(match(maskb, id))] = 0
  img = paintObjects(maskb, img, col=c(NA, 'blue'), opac=c(0,0.5))
  if (interactive()) display(img, title='Highlighted nuclei')
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}


