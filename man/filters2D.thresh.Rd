\name{thresh}

\alias{thresh}

\docType{function}

\title{Adaptive threshold algorithm}
\description{
    \code{thresh} is the implementation of adaptive threshold. It uses a moving
    frame of \code{width} and \code{height} to estimate mean intensity values
    and calculate the threshold. Function operates on grayscale images only.
}

\usage{
    thresh(x, width = 20, height = 20, offset = 0.05, preprocess = FALSE)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{width}{x-pixel region to evaluate the mean.}
    \item{height}{y-pixel region to evaluate the mean.}
    \item{offset}{Threshold offset from the mean, a reasonable value should be within
        the \code{range(Image)}.}
    \item{preprocess}{If \code{TRUE} \code{gaussFilter} with arguments 4, 2 is applied to the
        normalized image first.}
}

\value{
    A new instance of class \code{\link{Image}} with the same size as \code{x},
    but with the filter applied.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}}, \code{\link{objectCount}}, \code{\link{distMap}}
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    im = read.image("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/example.tif")
    im1 = thresh(im)
    dm = distMap(im1)
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

