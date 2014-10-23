\name{computeFeatures}

\alias{computeFeatures}
\alias{computeFeatures.basic}
\alias{computeFeatures.shape}
\alias{computeFeatures.moment}
\alias{computeFeatures.haralick}
\alias{standardExpandRef}
  
\title{Compute object features}

\description{
  Computes morphological and texture features from image objects.
}

\usage{
computeFeatures(x, ref, methods.noref=c("computeFeatures.moment", "computeFeatures.shape"),
  methods.ref=c("computeFeatures.basic", "computeFeatures.moment", "computeFeatures.haralick"),
  xname="x", refnames, properties=FALSE, expandRef=standardExpandRef, ...)
computeFeatures.basic(x, ref, properties=FALSE, basic.quantiles=c(0.01, 0.05, 0.5, 0.95, 0.99), xs, ...)
computeFeatures.shape(x, properties=FALSE, xs, ...)
computeFeatures.moment(x, ref, properties=FALSE, xs, ...)
computeFeatures.haralick(x, ref , properties=FALSE, haralick.nbins=32, haralick.scales=c(1, 2), xs, ...)
standardExpandRef(ref, refnames)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing labelled objects.
    Labelled objects are pixel sets with the same unique integer value.}
  \item{ref}{A matrix or a list of matrices, containing the
    intensity values of the reference objects.}
  \item{methods.noref}{A character vector containing the function names
    to be called to compute features without reference intensities. Default is
    \code{computeFeatures.moment} and \code{computeFeatures.shape}.}
 \item{methods.ref}{A character vector containing the function names
   to be called to compute features with reference intensities. Default is
   \code{computeFeatures.basic}, \code{computeFeatures.moment} and
   \code{computeFeatures.haralick}.}
   \item{xname}{A character string naming the object layer. Default is
     \code{x}.}
   \item{refnames}{A character vector naming the reference intensity
     layers. Default are the names of \code{ref}, if present. If not,
     reference intensity layers are named using lower-case letters.}
   \item{properties}{A logical. If \code{FALSE}, the default, the
    function returns the feature matrix. If \code{TRUE}, the function
    returns feature properties.}
  \item{expandRef}{A function used to expand the reference
    images. Default is \code{standardExpandRef}. See Details.}
  \item{basic.quantiles}{A numerical vector indicating the quantiles to
    compute.}
  \item{haralick.nbins}{An integer indicating the number of bins using
    to compute the Haralick matrix. See Details.}
  \item{haralick.scales}{A integer vector indicating the number of
    scales to use to compute the Haralick features.}
  \item{xs}{An optional temporary object created by
    \code{computeFeatures} used for performance considerations.}
  \item{...}{Optional arguments passed to the feature computation functions.}
}

\value{
  If \code{properties} if \code{FALSE} (by default), \code{computeFeatures}
  returns a matrix of n cells times p features, where p depends of
  the options given to the function. Returns \code{NULL} if no object is
  present.

  If \code{properties} if \code{TRUE}, \code{computeFeatures}
  returns a matrix of p features times 2 properties (translation and
  rotation invariance). Feature properties are useful to filter out
  features that may not be needed for specific tasks, e.g. cell
  position when doing cell classification.
}

\details{
  Features are named x.y.f, where x is the object layer, y the reference
  image layer and f the feature name. Examples include \code{cell.dna.mean},
  indicating mean DNA intensity computed in the cell or
  \code{nucleus.tubulin.cx}, indicating the x center of mass of tubulin
  computed in the nucleus region.

  The function \code{computeFeatures} computes sets of
  features. Features are organized in 4 sets, each computed by a
  different function. The function \code{computeFeatures.basic}
  computes spatial-independent statistics on pixel intensities:
  \itemize{
    \item b.mean: mean intensity
    \item b.sd: standard deviation intensity
    \item b.mad: mad intensity
    \item b.q*: quantile intensity
  }
  
  The function \code{computeFeatures.shape} computes features that
  quantify object shape:
  \itemize{
    \item s.area: area size (in pixels)
    \item s.perimeter: perimeter (in pixels)
    \item s.radius.mean: mean radius (in pixels)
    \item s.radius.sd: standard deviation of the mean radius (in pixels)
    \item s.radius.max: max radius (in pixels)
    \item s.radius.min: min radius (in pixels)
  }
  
  The function \code{computeFeatures.moment} computes features
  related to object image moments, which can be computed with or without
  reference intensities:
  \itemize{
    \item m.cx: center of mass x (in pixels)
    \item m.cy: center of mass y (in pixels)
    \item m.majoraxis: elliptical fit major axis (in pixels)
    \item m.eccentricity: elliptical eccentricity defined by
    sqrt(1-minoraxis^2/majoraxis^2). Circle eccentricity is 0 and straight
    line eccentricity is 1.
    \item m.theta: object angle (in radians)
  }

  The function \code{computeFeatures.haralick} computes features
  that quantify pixel texture. Features are named according to
  Haralick's original paper.
}


\references{
  R. M. Haralick, K Shanmugam and Its'Hak Deinstein (1979). \emph{Textural Features for Image 
  Classification}. IEEE Transactions on Systems, Man and Cybernetics.
}

\seealso{
  \code{\link{bwlabel}}, \code{\link{propagate}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2011
}

\examples{
  ## load and segment nucleus
  y = readImage(system.file("images", "nuclei.tif", package="EBImage"))[,,1]
  x = thresh(y, 10, 10, 0.05)
  x = opening(x, makeBrush(5, shape='disc'))
  x = bwlabel(x)
  display(y, title="Cell nuclei")
  display(x, title="Segmented nuclei")

  ## compute shape features
  fts = computeFeatures.shape(x)
  fts

  ## compute features
  ft = computeFeatures(x, y, xname="nucleus")
  cat("median features are:\n")
  apply(ft, 2, median)

  ## compute feature properties
  ftp = computeFeatures(x, y, properties=TRUE, xname="nucleus")
  ftp
}
