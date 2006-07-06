\name{distMap}

\alias{distMap}

\docType{function}

\title{Image distance maps}

\description{
    \code{distMap} performs an image transformation, in which to each foreground
    pixel a value is associated which gives the distance from this pixel to a
    nearest background pixel (zero). \code{distMap} works on grayscale images 
    only, which are treated as binary such that every non-zero pixel is considered
    as foreground and all zero as background. It is advisable to use 
    \code{\link{thresh}} to obtain a true binary image before calling \code{distMap}.
}

\usage{
    distMap(x, alg = "LotufoZampirolli")
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{alg}{Character scalar, the algorithm to use; default (and so far 
        the only) value is \code{LotufoZampirolli}.}
}

\value{
    A new instance of class \code{\link{Image}} with the same size
    as \code{x} and of type grayscale that contains distances (in pixel) from every
    pixel to the background. The range of value is generally larger than the
    recommneded [0,1] for grayscale images.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}, \code{\link{ws}}}
}

\source{
    The C code for the Lotufo-Zampirolli algorithm was adapted from the 
    \code{ANIMAL} imaging library by Ricardo Fabbri.
}

\references{
    The algorithm \code{LotufoZampirolli} is from R. Lotufo, F. Zampirolli, 
    SIBGRAPI 2001, 100-105, 2001.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    # create an empty image of 300x200 size
    im1 <- Image(0, c(300,200,1))
    # fill image with data (waves)
    for (i in 1:dim(im1)[[1]])
        for (j in 1:dim(im1)[[2]]) {
            im1[i,j,1] <- abs(sin(i*0.0314)*sin(i*0.0314) + cos(j*0.0314)*cos(j*0.0314))
        }
    # normalize image
    im1 <- normalize(im1)
    \dontrun{display(im1)}
    # threshold
    seg <- thresh(im1, 20, 20, 0)
    \dontrun{display(seg)}
    # calculate distance map and normalize it
    dm <- distMap(seg)
    dmnorm <- normalize(dm)
    \dontrun{display(dmnorm)}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

