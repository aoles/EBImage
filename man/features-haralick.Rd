\name{haralickMatrix}

\alias{haralickMatrix}
\alias{haralickFeatures}

\concept{object detection}

\title{ Co-occurrence matrices (GLCM) and Haralick texture features }

\description{
  Computes the gray level co-occurrence matrix (GLCM, frequency of
  pixel intensities given the mean intensity of their 4 neighbouring pixels)
  and corresponding Haralick features from image objects. 
}

\usage{
  haralickFeatures(x, ref, nc = 32)
  haralickMatrix(x, ref, nc = 32)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
  
  \item{ref}{An \code{Image} object or an array, containing the
    intensity values of the objects.}
  
  \item{nc}{ A numeric value. Specifies the number of gray levels
    used to compute the co-occurrence matrix. Default value is 32.}  
}

\value{
  \code{haralickFeatures} returns a matrix (or a list of matrices if
  \code{x} contains multiple frames) of features computed of the objects
  present in \code{x} and using the intensity values of \code{ref}.

  \code{haralickMatrix} returns an array (or a list of arrays if
  \code{x} contains multiple frames) of dimension \code{nc*nc*nobj},
  where \code{nobj} is the number of objects  in \code{x}, containing
  the GLCM values of image objects.
}

\details{
  \code{haralickFeatures} computes the following set of statistics on
  the GLCM matrix:
  
  \itemize{
    \item{\code{h.asm}}{Angular second moment: \code{sum[i=1^nc] sum[j=1^nc]p(i,j)^2}.}
    \item{\code{h.con}}{Contrast: \code{sum[i=2^(2*nc)] n^2 *
    sum[i=1^nc] sum[j=1^nc] p(i,j), for all i,j s.t ABS(i - j) = n}.}
  \item{\code{h.cor}}{Correlation: \code{sum[i=1^nc]
    sum[j=1^nc]((i * j) * p(i,j) - mu_x * mu_y) / sigma_x * sigma_y}.}
  \item{\code{h.var}}{Variance: \code{sum[i=1^nc] sum[j=1^nc](i - mu)^2.
    * p(i,j)}.}
  \item{\code{h.idm}}{Inverse difference moment: \code{sum[i=1^nc]
    sum[j=1^nc] p(i,j) / (1 + (i - j)^2) }.}
  \item{\code{h.sav}}{Sum average: \code{sum[i=2^(2*nc)] i * Px+y(i)}.}
  \item{\code{h.sva}}{Sum variance: \code{sum[i=2^(2*nc)] (i - sen)^2 * Px+y(i)}.}
  \item{\code{h.sen}}{Sum entropy: \code{-sum[i=2^(2*nc)] Px+y(i) * log(p(i,j))}.}
  \item{\code{h.ent}}{Entropy: \code{-sum[i=1^nc] sum[j=1^nc] p(i,j) *
    log( p(i,j) )}.}
  \item{\code{h.dva}}{Difference variance: \code{sum[i=0^(nc-1)] (i^2) * Px-y(i)}.}
  \item{\code{h.den}}{Difference entropy: \code{sum[i=0^(nc-1)] Px-y(i)
      * log( Px-y(i,j) )}.}
  \item{\code{h.f12}}{Measure of correlation: \code{abs(ent - HXY1) / HX}.}
  \item{\code{h.f13}}{Measure of correlation: \code{sqrt( 1 - exp(2*(ent - HXY2)))}.}
  }

  where:
  \itemize{
    \item{\code{p}}{is the GLCM matrix.}
    \item{\code{Px(i)}}{Marginal frequency. Defined by \code{Px(i) = sum[j=1^nc] p(i,j)}.}
    \item{\code{Py(j)}}{Marginal frequency. Defined by \code{Py(j) = sum[i=1^nc] p(i,j)}.}
    \item{\code{mu_x, mu_y}}{Means of \code{Px} and \code{Px}.}
    \item{\code{sigma_x, sigma_y}}{Standard deviations of \code{Px} and \code{Py}.}
    \item{\code{Px+y}}{Probability of the co-occurrence matrix
      coordinates sums to \code{x+y}. Defined by \code{Px+y(k) =
	sum[i=1^nc] sum[j=1^nc] p(i,j), i + j = k and k = 2,3,...,2*nc}.}
    \item{\code{Px-y}}{Probability of the absolute value of the
      difference between co-occurrence matrix cooordinates being equal
      to \code{x-y}. Defined by \code{Px-y(k) =	sum[i=1^nc] sum[j=1^nc] p(i,j), abs(i - j) = k and k =
	2,3,...,2*nc}.}
    \item{\code{HXY1}}{\code{-sum[i=1^nc] sum[j=1^nc] p(i,j) * log(
	Px(i),Py(j))}.}
    \item{\code{HXY2}}{\code{-sum[i=1^nc] sum[j=1^nc] Px(i)*Py(j) * log(
	Px(i),Py(j))}.}
  }

  Computed Haralick features are rotational invariant and good descriptors of
  object textures.
}

\references{
  R. M. Haralick, K Shanmugam and Its'Hak Deinstein (1979). \emph{Textural Features for Image 
  Classification}. IEEE Transactions on Systems, Man and Cybernetics.
}

\seealso{
  \code{\link{getFeatures}}, \code{\link{zernikeMoments}}
}

\author{
    Mike Smith, \email{msmith@ebi.ac.uk}; Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  example(getFeatures)
}


