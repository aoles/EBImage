\name{distmap}

\alias{distmap}
\alias{distmap,Image-method}

\title{ Distance map transform of binary images }

\description{
  The function computes a distance map transformation of a binary image,
  i.e. of an image whose pixels are labeled as \emph{foreground} and
  \emph{background}. 
  In the distance map, each pixel contains the distance from that pixel to the nearest
  background pixel, or to the border of the image.
  Note: this is a trivial brute force implementation. Please look
  elsewhere for efficient implementations; see also the references.
}

\usage{
  \S4method{distmap}{Image}(x, t=0.05, exact=FALSE, bg=0.05, ...)
}

\arguments{

  \item{x}{A \code{\link{Grayscale}} object of \code{\linkS4class{Image}}.
    \code{x} is considered as a binary image, consisting of 0's for background
    and all other values for foreground. }

  \item{t}{A numeric vector of length 1, the background threshold in the range 
    \code{[0,1)}. Pixels with intensity smaller than \code{t} are considered 
    as background. } 

  \item{bg}{A numeric numeric vector of length 1, the allowed minimum of the fraction of 
    background pixels in the image. Deprecated.}
  
  \item{exact}{???}
  
  \item{...}{Further arguments.}
}

\value{
  A \code{\link{Grayscale}} object of \code{\linkS4class{Image}} with pixels
  containing the \code{\link{floor}} values of their distances to the nearest 
  background points. 
}

\references{2D Euclidean Distance Transform Algorithms: A Comparative Survey.
R. Fabbri, L. da F. Costa, J.C. Torelli and O.M. Bruno. ACM Computing
Surveys, Vol. 40, No. 1, Article 2 (Feb. 2008).

The Image Processing Handbook. John Russ. 5th edition, 2006, CRC Press.

VIGRA by Ullrich Koethe: http://kogs-www.informatik.uni-hamburg.de/~koethe/vigra
}

\seealso{
  \code{
    \linkS4class{Image}, \link{watershed}, \link{thresh}, \link{propagate}
  }
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006
}

\examples{
  \dontrun{see ?watershed for an example}
}


