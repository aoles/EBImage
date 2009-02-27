\name{segment}

\alias{edge}
\alias{segment}

\concept{segmentation}
\concept{edge detection}

\title{ Segmentation and edge detection }

\description{
  Fucntions to segment images and detect edges.
}

\usage{
  edge(x, r=0)
  segment(x, cl=10, s=1.5)
}

\arguments{

  \item{x}{ An object of \code{\linkS4class{Image}}. }

  \item{r}{ The radius of the pixel neighbourhood to take into account. 
    The 0 value enables automatic radius selection. }

  \item{cl}{ Minimum cluster size in pixels . } 

  \item{s}{ The smoothing threshold. }

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

