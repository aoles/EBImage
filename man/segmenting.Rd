\name{segment}

\alias{edge}
\alias{fill}
\alias{segment}

\concept{segmentation}
\concept{edge detection}

\title{ Segmentation and edge detection }

\description{
  Fucntions to segment images and detect edges.
}

\section{ Usage }{
  \preformatted{
    edge(x, r=0, ...)
    segment(x, cl=10, s=1.5, ...)
    fill(x, col, xoff, yoff, fuzz=10, method="floodfill", ...) ## FIXME: still not implemented, unclear API
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{Image-class}}. }

  \item{\code{r}}{ The radius of the pixel neighbourhood to take into account. 
    The 0 value enables automatic radius selection. }

  \item{\code{cl}}{ Minimum cluster size in pixels . } 

  \item{\code{s}}{ The smoothing threshold. }

  \item{\code{col}}{ The fill colour in any format (\code{character}, gray 
    value or integer byte-coded value).}

  \item{\code{xoff, yoff}}{ Point to start fill at. }

  \item{\code{fuzz}}{ Colour tolerance, within this interval the colours are
    considered the same. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
}

\section{ Details }{

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

\seealso{
  \code{
    \link{Image-class}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

