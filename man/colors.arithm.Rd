\name{Color arithmetics}

\alias{Color arithmetics}
\alias{add2RGB}
\alias{sub2RGB}
\alias{scale2RGB}

\docType{function}

\title{Color arithmetics, adding and subtracting images}

\description{
    Routines to add, subtract or scale colors given as separate vectors or 
    colors in images.
}

\section{Usage}{
    \code{add2RGB(x, y)}\\
    \code{sub2RGB(x, y)}\\
    \code{scale3RGB(x, mult)}
}
\section{Arguments}{
    \item{x}{An object of class \code{Image} for image conversions, 
        \code{numeric/double} to convert from grayscale values, \code{integer} to
        convert from RGB values, \code{character} to convert from RGB values
        written as X11 strings}
    \item{y}{idib}
    \item{mult}{Scaling factor, data will be multiplied by this value. RGB values
        multiplied by-channel}
}
\section{Value}{
    An integer vector of RGBs if \code{x} is a vector, an RGB \code{Image} if 
    \code{x} is \code{Image}. \code{y} can have any class supported by \code{\link{toRGB}}
    function. Results can be further converted to any type by using color conversion
    funcstions, see \code{\link{Colors}}.
    
}

\seealso{
   \code{\link{Image}}, \code{\link{Colors}}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{dplot}
\keyword{manip}

