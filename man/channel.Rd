\name{channel}

\alias{channel}

\concept{color conversion}
\concept{color mode}

\title{ Color and image color mode conversions }

\description{
  \code{channel} allows to convert colors of images and color values given in 
  vectors, arrays or matrices from one mode to another. The supported source and
  target modes are TrueColor (data of type: \code{\link{integer}}) and
  Grayscale (\code{\link{numeric}}) and X11 strings (\code{\link{character}}).

  The mode of the argument is determined automatically from its type.
}


\section{ Usage }{ \preformatted{

## S4 methods for signatures x='Image', x='ANY' and mode="character":

channel(x, mode)

}}

\section{Arguments}{

  \item{x}{Either an object of \code{\link{Image-class}} or a vector of 
    \code{numeric}, \code{integer} or \code{character}. }

  \item{mode}{ A character value specifying the target mode for conversion. 
    See section 'Conversion Modes' below. }
}

\section{Value}{

  For an argument of \code{\link{Image-class}} the function returns a new object 
  of \code{\link{Image-class}} in all modes except \code{'X11'}. In case of
  \code{'X11'} it returns a \code{character} vector of the same length as the image.

  For vectors the function returns a \code{vector} of the same size in the target
  mode.
}

\section{ Conversion modes }{

  \describe{

    \item{\code{rgb}}{ The result is a true color representation of the input.
      Its class is or includes \code{integer}, i.e. an integer array or a 
      \code{\link{TrueColor}} integer-based image. }

    \item{\code{gray}}{ The result is a gray scale representation of the
      input. Its class is or includes \code{numeric}. Synonym: \code{grey} }

    \item{\code{red, green, blue }}{ The result is a gray scale representation 
      of the input from only the red, the green or the blue channel was extracted,
      correspondingly. The result's class is \code{numeric}-based. As synonyms
      single letters \code{r, g, b} can be used correspondingly.}

    \item{\code{asred, asgreen, asblue}}{ The result is a true color with all
      the data in one channel only, the red, green or blue, correspondingly. The
      result's class is \code{integer}-based. As synonyms \code{2r, 2g, 2b} can
      be used. }

    \item{\code{x11}}{ The result is a \code{character}, which elements represent
      the input colors as X11 strings, i.e. RGB hex codes starting with a sharp
      sign. }

  }

  Conversion modes are case insensitive!

  If the input is a \code{character} vector, its values may contain color names
  instead of color hex codes, like \code{red, purple, white} etc. However,
  the result of the \code{X11} mode will always contain hex codes.
}

\seealso{
  \code{
    \link{Image-class}, \link{Image}, \link{read.image}, \link{write.image}
  }
}

\examples{
cl <- channel("pink", "rgb")
r <- channel( channel(cl, "red"), "asred")
g <- channel( channel("pink", "green"), "asgreen")
b <- channel( channel(cl, "blue"), "asblue")
rgb <- r + g + b
print( rgb == cl )
clg <- channel(cl, "gray")
print( channel(cl, "x11"))
}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

