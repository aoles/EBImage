\name{haralick}

\alias{haralick.matrix}
\alias{haralick.features}

\concept{object detection}

\title{ Co-occurrence matrices (GLCM) and Haralick texture features }

\description{
  A set of functions to compute the co-occurrence matrix (GLCM - gray level 
  co-occurence matrix) and haralick texture features of objects in an indexed image.
}

\section{ Usage }{
\preformatted{

  ## S4 methods for signature x='IndexedImage', ref='Image'

  haralick.matrix(x, ref, nc = 32)
  haralick.features(x, ref, nc = 32)

}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{IndexedImage-class}} used as a mask.}

  \item{\code{ref}}{A \code{\link{Grayscale}} object of \code{\link{Image-class}}.
    The texture is calculated from the data in this image.}

  \item{\code{nc}}{ A numeric value. Specifies the number of gray levels to separate
  \code{ref} into when calculating the co-occurrence matrix. Defaults to 32}
  
}}

\section{ Value }{

  For a single frame in \code{x} the result of \code{haralick.matrix} is an array of 
  dimensions \code{nc} * \code{nc} * "the number of objects in the frame". 

  \code{haralick.features} returns a matrix of dimensions "number of objects in the frame" * 12. 
  
  For multiple frames a list of each of the above will be returned.

}

\section{ Details }{

  \code{haralick.matrix} computes a co-occurrence matrix of dimension \code{nc * nc} 
  for each object in an image.

  \code{haralick.features} computes for each object in an image 12 Haralick 
  texture features. The feature names carry a \code{t.} prefix (t for texture). 
  The features are:
  
  \describe{
  \item{\code{asm}}{Angular second moment}
  \item{\code{con}}{Contrast}
  \item{\code{cor}}{Correlation of GLCM}
  \item{\code{var}}{Variance}
  \item{\code{idm}}{Inverse difference moment}
  \item{\code{sav}}{Sum average}
  \item{\code{sva}}{Sum variance}
  \item{\code{sen}}{Sum entropy}
  \item{\code{ent}}{Entropy}
  \item{\code{dva}}{Difference variance}
  \item{\code{den}}{Difference entropy}
  \item{\code{f12}}{Measure of correlation}
  \item{\code{f13}}{Measure of correlation}
  }  
}

\section{ References }{
  R. M. Haralick, K Shanmugam and Its'Hak Deinstein (1979). "Textural Features for Image 
  Classification." IEEE Transactions on Systems, Man and Cybernetics.
  
}

\seealso{\code{
  \link{IndexedImage-class}, \link{watershed}, \link{propagate}
}}

\author{
    Mike Smith <\email{msmith@ebi.ac.uk}>, Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2007
}

\keyword{file}

