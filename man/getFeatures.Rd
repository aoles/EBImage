\name{getFeatures}

\alias{getFeatures}

\concept{features extraction}

\title{Extract feature extraction from image objects}

\description{
  Extracts numerical features from image objects.
}

\usage{
  getFeatures(x, ref, N=12, R=30, nc=32)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
  
  \item{ref}{An \code{Image} object or an array, containing the
    intensity values of the objects.}
  
  \item{N}{ Passed to \code{\link{zernikeMoments}}. Integer value 
    defining the degree of the Zernike polynomials, which in turn
    defines the number of features calculated. Defaults to 12.}

  \item{R}{ Passed to \code{\link{zernikeMoments}}. Defines the radius 
    of the circle around an object centre from which the features are calculated. 
    See details. Defaults to 30.}
   
  \item{nc}{ Passed to \code{\link{haralickFeatures}}. A numeric value. 
    Specifies the number of gray levels to bin \code{ref} into when 
    computing the co-occurrence matrix. Defaults to 32.}
}

\value{
  \code{getFeatures} returns feature matrices.
}
      
\details{
  Combines and returns the features returned by \code{hullFeatures},
  \code{moments}, \code{edgeFeatures}, \code{haralickFeatures} and
  \code{zernikeMoments}.
}
 

\seealso{
  \code{\link{hullFeatures}}, \code{\link{moments}}, \code{\link{edgeFeatures}}
  \code{\link{haralickFeatures}}, \code{\link{zernikeMoments}} 
  }

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  x = x[,,1]
  if (interactive()) display(x)

  ## computes object mask
  y = thresh(x, 10, 10, 0.05)
  y = opening(y, makeBrush(5, shape='disc'))
  mask = bwlabel(y)
  if (interactive()) display(mask, title='Cell nuclei')

  ## features
  ftrs = getFeatures(mask, x)[[1]]
  print(ftrs[1:5,])

  ## paint nuclei with an eccentricity higher than 0.85
  maskb = mask
  id = which(ftrs[,'m.ecc']<0.85)
  maskb[!is.na(match(maskb, id))] = 0

  img = paintObjects(maskb, channel(x, 'rgb'), col='red')
  if (interactive()) display(img, title='Nuclei with high eccentricity')
}

\keyword{manip}

