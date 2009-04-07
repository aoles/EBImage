\name{getFeatures}

\alias{getFeatures}
\alias{features}

\concept{object detection}

\title{Feature extraction for objects in indexed images}

\description{
  Extract numerical descriptors for objects in indexed images.
  The functions here call a set of individual feature extraction routines for
  every feature set like hull, texture (Haralick), edge, moment features etc.
}

\usage{
  getFeatures(x, ref, N=12, R=30, apply.Gaussian, nc=256, pseudo)

}

\arguments{

  \item{x}{ An object of \code{\linkS4class{Image}} (like those
    obtained from \code{\link{propagate}} or \code{\link{watershed}}. }

  \item{ref}{ A reference image containing images of the objects in
    Grayscale mode (intensity values)! }

  \item{N}{ Passed to \code{\link{zernikeMoments}}. Integer value 
    defining the degree of the Zernike polynomials, which in turn defines the 
    number of features calculated. Defaults to 12.}

  \item{R}{ Passed to \code{\link{zernikeMoments}}. Defines the radius 
    of the circle around an object centre from which the features are calculated. 
    It also defines the standard deviation for the 2D Gausian applied at the 
    centre of an object. See details. Defaults to 30. }
   
  \item{nc}{ Passed to \code{\link{haralickFeatures}}. A numeric value. 
    Specifies the number of gray levels to separate \code{ref} into when 
    calculating the co-occurrence matrix. Here defaults to 256.}

  \item{apply.Gaussian, pseudo}{Deprecated.}

}

\value{

  \code{getFeatures} returns feature matrices.
}

      
\details{
  \code{ref} can be missing in calls to \code{getFeatures}. In this case texture
  and other intensity-dependent features will be omitted.
}
 

\seealso{ \code{
  \linkS4class{Image}, \link{propagate}, \link{paintObjects}, \link{stackObjects},
  \link{hullFeatures}, \link{haralickFeatures}, \link{moments}, \link{edgeFeatures},
  \link{zernikeMoments}
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
if (FALSE) {
  ## load images
  f = system.file('images', 'Gene1_G.tif', package="EBImage")
  x = readImage(f)

  ## adaptative thresholding to find nuclei
  fnuc = makeBrush(25, shape='disc', step=FALSE)
  fnuc = fnuc/sum(fnuc)
  mask = x > (filter2(x, fnuc)+0.005)

  ## refine segmentation with morphology filters
  se = makeBrush(5, shape='diamond')

  mask = dilate(erode(closing(mask, mk5), mk3), mk5)
  ## index objects with 'watershed'
  io = watershed( distmap(mask), 1.5, 1)
  ft = hullFeatures(io)
  mf = moments(io, ii)   ## need these for intensity and size
  for ( i in seq_along(ft) ) ft[[i]] = cbind(ft[[i]], mf[[i]])
  io = rmObjects(io, lapply(ft, function(x) 
         which(x[,"g.s"] < 150 | x[,"m.int"] < 50 | 0.3 * x[,"g.p"] < x[,"g.edge"] )
       ))
  ft = getFeatures(io, ii)
  str( ft )
}
  
}

\keyword{manip}

