\name{distMap}

\alias{distMap}

\docType{function}

\title{Image distance maps}

\description{
    \code{distMap} operates on grayscale images that are interpreted as
    binary, with two classes of pixels: those with a value of 0 are
    considerd "background", all others are considered "foreground".
    For every pixel the function calculates the distance to the nearest
    background pixel. Distance maps are useful for segmentation if objects
    overlap, and for deriving object size and shape descriptors.
}

\usage{
    distMap(x, alg = "LotufoZampirolli")
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{alg}{Character scalar, the algorithm to use. The default value is \code{LotufoZampirolli},
        which returns squared distances to the nearest background points.}
}

\value{
    A new instance of class \code{\link{Image}} with the same size
    as \code{x} and of type grayscale that contains distances from every
    pixel to the background.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}, \code{\link{objectCount}}}
}

\source{
The C code for the Lotufo-Zampirolli algorithm was adapted from the \code{animal} imaging library by Ricardo Fabbri.
}

\references{
The algorithm \code{LotufoZampirolli} is from R. Lotufo, F. Zampirolli, SIBGRAPI 2001, 100-105, 2001.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    im <- read.image("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/segmented.tif")
    dm <- distMap(im)

%    x <- Image(numeric(100), dim=c(10,10))
%for(j in 2:9) x[2:9, j]=1
%
%d1 = distMap(x, "LotufoZampirolli")
%d2 = distMap(x)
%identical(sqrt(d1),  d2)
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

