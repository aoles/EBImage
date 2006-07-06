\name{thresh}

\alias{thresh}

% deprecating
\alias{adaptThresh}

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
    \item{offset}{Threshold offset from the mean, a reasonable value should be
        within \code{range(Image)}.}
    \item{preprocess}{If \code{TRUE} \code{gaussFilter} with arguments 4, 2 is applied to the
        normalized image first.}
}
\details{
    \code{adaptThresh} is deprecated. Please use \code{thresh} instead.
    \code{thresh} has a different range of argument \code{offset}, which should be
    within \code{range(Image)}.
}

\value{
    A new instance of class \code{\link{Image}} with the same size as \code{x},
    but with the filter applied.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}}, \code{\link{distMap}}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    # see example for distMap
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

