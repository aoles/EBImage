\name{channel}

\alias{channel}

\concept{color conversion}
\concept{color mode}

\title{ Color mode conversions }

\description{
  The functions \code{channel} allows to convert images and other vector or
  array data representing colors from one mode into another. The supported
  modes are: true color (data type: \code{\link{integer}}), grayscale 
  (\code{\link{numeric}}) and X11 string (\code{\link{character}}). 
  
  The original mode of the argument is determined automatically from its type.
}


\usage{
    channel(x, mode, ...)
}

\arguments{

  \item{x}{A vector or an array of \code{integer}'s, \code{numeric}'s or
    \code{character}'s or an object of \code{\link{Image-class}}. }

  \item{mode}{ A character giving the target mode for conversion. Available 
    values are: \code{'rgb', 'gray', 'red', 'green', 'blue', 'asred', 
    'asgreen', 'asblue', 'x11'}; case insensitive; \code{'gray'='grey'}. }

  \item{...}{ Reserved for future use. }
}

\value{

  For objects of \code{\link{Image-class}} -- a new object of 
  \code{\link{Image-class}} in all modes except \code{'X11'}. In the latter
  case, a \code{character} vector of the same length as the image size.
    
  For vectors and arrays, the result will be a \code{Vector} of the same size.
}

\section{ Conversion modes }{

  \describe{

    \item{\code{rgb}}{ The resulting image is a true color representation of 
      input, its class is or includes \code{integer}, i.e. integer array,  
      integer-based image. }

    \item{\code{gray}}{ The resulting image is a grayscale representation of 
      input , its class is \code{numeric}. }
      
    \item{\code{red, green, blue }}{ The resulting image is a grayscale image
      being an extraction of a single channel of input, correspondingly of the
      red, green or blue channel. The class of the result is \code{numeric}. }
      
    \item{\code{asred, asgreen, asblue}}{ The resulting image is a true color
      image with input data converted into one single channel as the level of
      its intensity, correspondingly red, green or blue. The class of the
      result is \code{integer}. }

    \item{\code{x11}}{ The result is a \code{vector} of \code{character}'s,
      each giving the input color as X11 string. }

  }
  
  If input is a \code{character} vector, its values may contain color names
  instead of hex representation, like \code{red, purple, white} etc. However,
  the result of the \code{X11} mode will always contain hex codes.
  
}

\seealso{
  \code{
    \link{Image-class}, \link{Image}, \link{read.image}, \link{write.image}
  }
}

\examples{
  if ( interactive() ) {
    cl <- channel("pink","rgb")
    r <- channel( channel(cl, "red"), "asred")
    g <- channel( channel("pink", "green"), "asgreen")
    b <- channel( channel(cl, "blue"), "asblue")
    rgb <- r + g + b
    print( rgb == cl )
    clg <- channel(cl, "gray")
    print( channel(cl, "x11"))
  }

}


\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

