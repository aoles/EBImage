\name{channel}

\alias{channel}

\concept{color conversion}
\concept{color mode}

\title{ Color conversion }

\description{
  \code{channel} allows to convert images and other data in vectors,
  arrays or matrices representing colors from one mode to another.
  The supported modes are: TrueColor (data type: \code{\link{integer}}),
  Grayscale (\code{\link{numeric}}) and X11 strings (\code{\link{character}}).

  The mode of the argument is determined automatically from its atomic type.
}


\section{ Usage }{
  \preformatted{
    # S4 method for classes 'Image' and 'ANY':
    channel(x, mode)
  }
}

\section{Aarguments}{

  \item{x}{A vector, matrix or array of \code{integer}'s, \code{numeric}'s or
    \code{character}'s; or an object of \code{\link{Image-class}}. }

  \item{mode}{ A character giving the target mode for conversion. See section
    'Conversion Modes' below. }
}

\section{Value}{

  For an argument of \code{\link{Image-class}} -- a new object of
  \code{\link{Image-class}} in all modes except \code{'X11'}. In case of
  \code{'X11'} -- a \code{character} vector of the same length as that
  of the image.

  For vectors and arrays -- be a \code{vector} of the same size.
}

\section{ Conversion modes }{

  Conversion modes are case insensitive.

  \describe{

    \item{\code{rgb}}{ The result is a TrueColor representation of the
      input, its class is or includes \code{integer}, i.e. integer array,
      integer-based image. }

    \item{\code{gray}}{ The resul is a Grayscale representation of the
      input, its class is or includes \code{numeric}. Synonim: \code{grey} }

    \item{\code{red, green, blue }}{ The result image is a Grayscale image
      corresponding to the intensity of the channel requested, its
      class is \code{numeric}-based. }

    \item{\code{asred, asgreen, asblue}}{ The result image is a TrueColor
      image with the input converted to the intensity of the specified
      color channel, its class is \code{integer}-based. }

    \item{\code{x11}}{ The result is a \code{vector} of \code{character}'s
      each giving the input colors or intensities as X11 strings in
      the hex representation starting with a sharp sign. }

  }

  If input is a \code{character} vector, its values may contain color names
  instead of color hex codes, like \code{red, purple, white} etc. However,
  the result of the \code{X11} mode will always contain hex codes.

}

\seealso{
  \code{
    \link{Image-class}, \link{Image}, \link{read.image}, \link{write.image}
  }
}

\examples{
  if ( interactive() ) {
    cl <- channel("pink", "rgb")
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
    Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

