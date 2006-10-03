\name{Tools}

\alias{Tools}
\alias{entropy}

\docType{class}

\title{Misc Tools and Utilities for EBImage}

\description{
    A collection of utility functions for different aspects of the package
    functionality.
}

\section{Usage}{
    \code{entropy(x, n=512, method="hist", ...)}
    
}
\section{Arguments}{
    \item{\code{x}}{Object of class \code{\link{Image}} or any other coersable
    	class, e.g. \code{numeric}, \code{array}}
    \item{\code{n}}{Corresponds to the number of divisions of the data range
        in evaluating \code{density} or \code{hist} -- correspondingly,
        argument \code{n} to the former and \code{breaks} to the latter}
    \item{\code{method}}{In call to \code{entropy} is the method used to generate
        the probability density used in the formula below. Possible values are
        \code{density} and \code{hist} -- by the names of corresponding functions 
        used}
    \item{\code{...}}{Further arguments that can be passed to functions used
        internaly, e.g. arguments for \code{\link{density}} or \code{\link{hist}}
        used in calls to \code{entropy}}
}

\details{
    \code{entropy} evaluates the Shannon's entropy, \verb|int(P(x)log_2(P(x))dx)|, for
    continuos floating-point data. The values of P(x) are evaluated using the
    function specified in \code{method} (can be either \code{hist} or \code{density}).
    \code{density} is prefered for dense data sets, whereas \code{hist} for sparce ones.
}

\section{Value}{
    A single numeric value of the dataset's entropy.
    
    If \code{x} is an image, the
    result is a vector of the length of \code{dim(x)[3]} -- number of frames -- 
    with separate entropy values for every frame. If \code{x} is RGB it is first
    converted to grayscale and entropy of a grayscale image is returned (please
    note that the reals information content of an RGB images should be larger
    or equal than that of a corresponding grayscale because one grayscale value
    can correspond to a full palette of RGB colours.
}

\seealso{
    Information Entropy at Wikipedia \url{http://en.wikipedia.org/wiki/Information_entropy}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{manip}

