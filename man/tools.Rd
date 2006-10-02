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
    \code{entropy(x, ...)}
    
}
\section{Arguments}{
    \item{\code{x}}{Object of class \code{\link{Image}} or any other coersable
    	class, e.g. \code{numeric}, \code{array}}
}

\details{
    \code{entropy} evaluates the Shannon's entropy, $\int P(x)log_{2}P(x)dx$, for
    continuos floating-point data. It uses \code{\link{density}} to evaluate the
    probability density. 
    
}

\section{Value}{
    A vector if the argument is a vector, \code{Image} if the argument is an image
    (with the exception of \code{toX11char} where the result is always a character
    vector).
}

\seealso{
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{manip}

