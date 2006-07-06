\name{Filters 2D}
\alias{Filters 2D}

\docType{class}

\title{
    Summary of image filters
}

\description{
    Some filters are ported from ImageMagick library. Applied to image stacks
    filters are executed sequentially and independently for each image.
}

\details{
    The functions accept objects of class \code{\link{Image}} plus filter-specific arguments.
    \describe{
        \item{\code{\link{Enhancement}}:}{\code{noise}, \code{blur},
            \code{despeckle}, \code{enhance}, \code{gaussFilter},
            \code{medianFilter}, \code{reduceNoise}, \code{sharpen},
            \code{spread}, \code{unsharpMask}}
        \item{\code{\link{Segmentation}}:}{\code{\link{thresh}}, \code{edge},
            \code{segment}, \code{\link{distMap}}}
        \item{\code{\link{Morphology}}:}{\code{mErode}, \code{mDilate},
            \code{mOpen}, \code{mClose}}
        \item{\code{\link{Color filters}}:}{\code{\link{normalize}}, \code{contrast}, \code{equalize},
            \code{colorGamma}, \code{mod}, \code{shade}, \code{solarize}}
        \item{\code{\link{Transformation}}:}{\code{rotate}, \code{sample.image}}
    }
}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

