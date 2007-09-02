\name{haralickMatrix}

\alias{haralickMatrix}
\alias{haralickMatrix,IndexedImage,Image-method}
\alias{haralickFeatures}
\alias{haralickFeatures,IndexedImage,Image-method}

\concept{object detection}

\title{ Co-occurrence matrices (GLCM) and Haralick texture features }

\description{
  A set of functions to compute the co-occurrence matrix (GLCM - gray level 
  co-occurrence matrix) and haralick texture features of objects in an indexed image.
}

\usage{

  \S4method{haralickMatrix}{IndexedImage,Image}(x, ref, nc = 32, ...)
  \S4method{haralickFeatures}{IndexedImage,Image}(x, ref, nc = 32, ...)

}

\arguments{

  \item{x}{An object of \code{\linkS4class{IndexedImage}} used as a mask.}

  \item{ref}{A \code{\link{Grayscale}} object of \code{\link{Image-class}}.
    The texture is calculated from the data in this image.}

  \item{nc}{ A numeric value. Specifies the number of gray levels to separate
    \code{ref} into when calculating the co-occurrence matrix. Defaults to 32}
  
  \item{...}{ Reserved. }
  
}

\value{

  For a single frame in \code{x} the result of \code{haralickMatrix} is an array of 
  dimensions \code{nc} * \code{nc} * "the number of objects in the frame". 

  \code{haralickFeatures} returns a matrix of dimensions "number of objects in 
  the frame" * 12. 
  
  For multiple frames a list of each of the above will be returned.

}

\details{

  \code{haralickMatrix} computes a co-occurrence matrix of dimension \code{nc * nc} 
  for each object in an image.  The co-occurrence matrix is constructed
  by assigning to element \code{[i,j]} the number of times a pixel of value
  \code{i} is adjacent to a pixel of value \code{j}.
  
  In order to achieve rotational invariance the co-occurrence matrix is
  computed using four passes through each object.  Thus each pixel is
  compared to four neighbours: right, below, diagonally down left and
  diagonally down right.  In order to achieve symmetry in the
  co-occurrence matrix the reciprocal of each of these scores is also
  added.  Thus for each pixel we have have a score for comparison with each of the
  surrounding pixels.

  Finally the entire matrix is divided by the total
  number of comparisons, giving a probability for each of the possible combinations.

  \code{haralickFeatures} computes for each object in an image 12 Haralick 
  texture features, calculated from the co-occurrence matrix. The feature names 
  carry a \code{t.} prefix (t for texture). The features and their calculations are:
  
  \describe{
    \item{\code{asm}}{Angular second moment: \code{sum[_i=1^nc] sum[_j=1^nc]p(i,j)^2}.}
    \item{\code{con}}{Contrast: \code{sum[_i=2^(2*nc)] n^2 *
    sum[_i=1^nc] sum[_j=1^nc] p(i,j), for all i,j s.t ABS(i - j) = n}.}
  \item{\code{cor}}{Correlation of GLCM: \code{sum[_i=1^nc]
    sum[_j=1^nc]((i * j) * p(i,j) - mu_x * mu_y) / sigma_x * sigma_y}.}
  \item{\code{var}}{Variance: \code{sum[_i=1^nc] sum[_j=1^nc](i - mu)^2.
    * p(i,j)}.}
  \item{\code{idm}}{Inverse difference moment: \code{sum[_i=1^nc]
    sum[_j=1^nc] p(i,j) / (1 + (i - j)^2) }.}
  \item{\code{sav}}{Sum average: \code{sum[_i=2^(2*nc)] i * Px+y(i)}.}
  \item{\code{sva}}{Sum variance: \code{sum[_i=2^(2*nc)] (i - sen)^2 * Px+y(i)}.}
  \item{\code{sen}}{Sum entropy: \code{-sum[_i=2^(2*nc)] Px+y(i) * log(
    p(i,j) )}.}
  \item{\code{ent}}{Entropy: \code{-sum[_i=1^nc] sum[_j=1^nc] p(i,j) *
    log( p(i,j) )}.}
  \item{\code{dva}}{Difference variance: \code{sum[_i=0^(nc-1)] (i^2) * Px-y(i)}.}
  \item{\code{den}}{Difference entropy: \code{sum[_i=0^(nc-1)] Px-y(i)
      * log( Px-y(i,j) )}.}
  \item{\code{f12}}{Measure of correlation: \code{ABS(ent - HXY1) / HX}.}
  \item{\code{f13}}{Measure of correlation: \code{sqrt( 1 - exp( -2
    *(HXY2 - ent) ) )}.}
  }

  Where:
  \describe{
    \item{\code{p(i,j)}}{The value in row i, column j of the co-occurrence
      matrix.}
    \item{\code{Px(i)}}{Partial probability density function.  Defined
      by \code{sum[_j=1^nc] p(i,j) }.}
    \item{\code{Py(j)}}{Partial probability density function.  Defined
      by \code{sum[_i=1^nc] p(i,j) }.}
    \item{\code{mu_x, mu_y}}{Are the means of \code{Px} and \code{Px}, the partial
      probability density functions.}
    \item{\code{sigma_x, sigma_y}}{Are the standard deviations of
      \code{Px} and \code{Py}.}
    \item{\code{Px+y}}{Is the probability of the co-occurrence matrix
      co-ordinates summing to \code{x+y}.  It is defined as \code{Px+y(k) =
	sum[_i=1^nc] sum[_j=1^nc] p(i,j), i + j = k and k = 2,3,...,2*nc}.}
    \item{\code{Px-y}}{Is the probability of the absolute value of the
      difference between co-occurrence matrix
      co-ordinates being equal to \code{x-y}.  It is defined as \code{Px-y(k) =
	sum[_i=1^nc] sum[_j=1^nc] p(i,j), ABS(i - j) = k and k =
	2,3,...,2*nc}.}
    \item{\code{HXY1}}{\code{-sum[_i=1^nc] sum[_j=1^nc] p(i,j) * log(
	Px(i),Py(j) ) }.}
    \item{\code{HXY2}}{\code{-sum[_i=1^nc] sum[_j=1^nc] Px(i)*Py(j) * log(
	Px(i),Py(j) ) }.}
  }
  
}

\references{

  R. M. Haralick, K Shanmugam and Its'Hak Deinstein (1979). \emph{Textural Features for Image 
  Classification}. IEEE Transactions on Systems, Man and Cybernetics.
  
}

\seealso{\code{
  \linkS4class{IndexedImage}, \link{watershed}, \link{propagate}
}}

\author{
    Mike Smith, \email{msmith@ebi.ac.uk}; Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  ## see example(getFeatures)
}

\keyword{manip}

