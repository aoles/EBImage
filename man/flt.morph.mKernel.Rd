\name{mKernel}

\alias{mKernel}

\docType{function}

\title{Kernel generator for mathematical morphology filters}

\description{
    Kernel generator for morphological filters.
}

\usage{
    mKernel(size = 5, shape = "round")
}
\arguments{
    \item{size}{Edge size of the kernel matrix, which will be a square one. 
        Valid values are odd numbers larger than 1, i.e. [3, 5, 7...}
    \item{shape}{Shape of the kernel (non-zero elements). Currently supported:
        \code{round} and \code{square}. The latter equals all elements 1}
}

\value{
    A square integer matrix (by row) of \code{size x size} with kernel elements set
    to either 0 or 1 depending on the shape.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}, \code{\link{Morphology}}}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    # see examples for mOpen and mClose
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

