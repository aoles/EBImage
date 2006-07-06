\name{Colors}

\alias{Colors}
\alias{toGray}
\alias{toRGB}
\alias{toX11char}
\alias{toRed}
\alias{toGreen}
\alias{toBlue}
\alias{getRed}
\alias{getGreen}
\alias{getBlue}

\docType{class}

\title{Color conversion routines}

\description{
    Functions to manipulate and convert colors. 
}

\section{Usage}{
    \code{toRGB(object)}\\
    \code{toGary(object)}\\
    \code{toX11char(object)}\\
    \code{toRed(object)}\\
    \code{toGreen(object)}\\
    \code{toBlue(object)}\\
    \code{getRed(object)}\\
    \code{getGreen(object)}\\
    \code{getBlue(object)}
}
\section{Arguments}{
    \item{object}{An object of class \code{Image} for image conversions, 
        \code{numeric/double} to convert from grayscale values, \code{integer} to
        convert from RGB values, \code{character} to convert from RGB values
        written as X11 strings}
}

\details{
    Separate from images, colors in EBImage can be specified as \code{integer} 
    values for RGB colors with byte-per-channel encoding; \code{double} values 
    (normally in the range [0,1]) or as X11 strings. The above functions accept 
    arguments of any of the above type and do the conversions automatically.
    
    For images, only two different modes are supported so far: \code{integer} and
    \code{double} for RGB and grayscale images correspondingly. Using 
    \code{toX11char} on an image will generate a \code{character} vector of the 
    same size as the image data.
}

\section{Value}{
    A vector if the argument is a vector, \code{Image} if the argument is an image
    (with the exception of \code{toX11char} where the result is always a character
    vector).
    
    \code{toGray}, \code{getRed}, \code{getGreen}, \code{getBlue} return data
    as double for grayscale values in the range [0,1].
    
    \code{toRGB}, \code{toRed}, \code{toGreen}, \code{toBlue} return data
    as integer with byte-per-channel color encoding.
}

\seealso{
   \code{\link{Image}}, \code{\link{add2RGB}}, \code{\link{scale2RGB}}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    fullred <- toRed(1.0)
    somegreen <- toGreen(0.5)
    abitblue <- toBlue(0.2)
    rgb <- fullred + somegreen + abitblue
    asX11 <- toX11char(rgb)
    asgray <- toGray(asX11)
    print(asX11)
    print(asgray)
}

\keyword{dplot}
\keyword{manip}

