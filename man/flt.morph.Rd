\name{Morphology}

\alias{Morphology}
\alias{mErode}
\alias{mDilate}
\alias{mOpen}
\alias{mClose}

\docType{function}

\title{Mathematical morphology image filters}

\description{
    A set of functions to perform morphological transformations of binary images.
    The two basic operations of erosion and dilation are implemented with
    \code{mErode} and \code{mDilate}. \code{mOpen} and \code{mClose} implement
    morphological opening and morphological closing by consequent execution of
    \code{mErode} and \code{mDilate} (or in reverse order for \code{mClose}).
    Kernels are used as structuring elements, standaed kernels can be obtained
    with call to \code{\link{mKernel}}.
}

\usage{
    mErode(x, iter = 1, kernel = mKernel(5))
    mDilate(x, iter = 1, kernel = mKernel(5))
    mOpen(x, iter = 1, kernel = mKernel(5))
    mClose(x, iter = 1, kernel = mKernel(5))
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}}, grayscale, binary}
    \item{iter}{Number of iterations}
    \item{kernel}{A matrix of integers to match the image, 0 idicates that
        the value should not be used. Matrix must not have even size, i.e.
        good kernel sizes are 3x3, 5x7, 9x9 etc, not 4x6}
}

\value{
    A new instance of class \code{\link{Image}} with the same size
    as \code{x}.
}

\details{
    In erosion, for every foreground pixel a kernel match is performed (the source
    pixel in the middle of the kernel matrix). If the kernel does not match the
    image, the pixel is reset to the background. For dilation the same operation is
    performed by scanning the background and converting pixels to foreground.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}}, \code{\link{mKernel}}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
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
    seg1 <- mClose(mOpen(seg, 1, mKernel(7)))
    # compare seg and seg1
    \dontrun{display(seg1)}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

