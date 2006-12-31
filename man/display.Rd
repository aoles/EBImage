\name{display}

\alias{display}

\concept{display}

\title{ Interactive image display }

\description{
  Display images on the screen of a local or remote display. 
}

\section{Usage}{
  \preformatted{
    display(x, no.GTK=FALSE, ...)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}}. }

  \item{\code{no.GTK}}{ Set to \code{FALSE} to avoid using \code{GTK+} display. } 

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  An invisible \code{NULL}.
}

\section{ Details}{

  If available the \code{GTK+} display is used by default. In this mode,
  users can display as many images simultaneously as they want. On contrary,
  if the \code{GTK+} mode is not used, the library uses internal 
  \code{ImageMagick} display functions, which due to the lack of control over
  their behavior are limited to displaying only one image at a time. Trying
  to display another image while there is one on display will result in an
  error message in the latter case.

  It is not necessary supply the \code{no.GTK} argument if the library was
  compiled without \code{GTK+} support as an alternative method will be 
  selected by default.
  
}

\seealso{
  \code{
    \link{Image-class}, \link{Image}, \link{read.image}, \link{write.image}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
  \dontrun{see ?watershed for examples}
}

\keyword{file}

