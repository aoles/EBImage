\name{segment}

\alias{edge}
\alias{edge,Image-method}
\alias{fill}
\alias{fill,Image-method}
\alias{segment}
\alias{segment,Image-method}

\concept{segmentation}
\concept{edge detection}

\title{ Segmentation and edge detection }

\description{
  Fucntions to segment images and detect edges.
}

\usage{
  \S4method{edge}{Image}(x, r=0, ...)
  \S4method{segment}{Image}(x, cl=10, s=1.5, ...)
  ## FIXME: 'fill' implemented incorrectly because of unclear API
  \S4method{fill}{Image}(x, col, xoff, yoff, fuzz=10, method="floodfill", ...) 
}

\arguments{

  \item{x}{ An object of \code{\linkS4class{Image}}. }

  \item{r}{ The radius of the pixel neighbourhood to take into account. 
    The 0 value enables automatic radius selection. }

  \item{cl}{ Minimum cluster size in pixels . } 

  \item{s}{ The smoothing threshold. }

  \item{col}{ The fill colour in any format (\code{character}, gray 
    value or integer byte-coded value).}

  \item{xoff, yoff}{ The point at which to start the fill. }

  \item{fuzz}{ Colour tolerance, within this interval the colours are
    considered the same. }

  \item{method}{ Fill method. }

  \item{...}{ Reserved. }
}

\value{

  A transformed image in an object of \code{\linkS4class{Image}}.
    
}

\details{

  \code{edge} returns an image of edges between different colours in the 
    original image. Most effective on binary images.
  
  \code{segment} segment an image by analyzing the histograms of the color 
    components and identifying units that are homogeneous with the fuzzy 
    C-means technique (source and implementation \code{ImageMagick}). The
    smoothing threshold eliminates noise in the second derivative of the 
    histogram. As the value is increased, you can expect a smoother second
    derivative.
    
  \code{fill} should allow colour fill, but ImageMagick API is not clear and the
    current implementation does not seem to produce any reasonable result.
    Therefore the function will currently produce an error message, should be
    corrected in future releases.
   
}

\seealso{ \code{
  \linkS4class{Image}
}}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2006
}

\keyword{manip}

