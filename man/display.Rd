\name{display}

\alias{display}
\alias{animate}

\concept{display}

\title{ Interactive image display }

\description{
  Display images on the screen of a local or remote display. 
}

\section{Usage}{
  \preformatted{
    ## S4 methods for signature x='Image'    

    display(x, no.GTK=FALSE)
    animate(x)                 ## not available on Windows
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}}. }

  \item{\code{no.GTK}}{ A logical value, if \code{TRUE} an \emph{ImageMagick}
    display will be used instead of the GTK display (read details below). 
    On Windows the \emph{ImageMagick} display is not available. } 

}}

\section{ Value }{

  An invisible \code{NULL}.
}

\section{ Details}{

  If available the \code{GTK} display will be used by default. This mode allows
  display of multiple images simultaneously. If the \code{GTK} mode is not used,
  an \code{ImageMagick}-internal display function, is used. This mode is
  for compatibility purposes only and should not be used if \code{GTK} is
  available. Because \code{ImageMagick} does not provide any programmatic ways to
  control and close display windows, the display in this mode is limited to one
  it time. The display window must be explicitly closed before a new image can
  be displayed. If \code{EBImage} was installed without \code{GTK} support,
  the function will fall back to the \code{ImageMagick} display automatically.
  In this case there is no need to supply \code{no.GTK} argument.
  
  So far \code{animate} uses the \code{AnimateImages} function from
  \code{ImageMagick}, which behavior is analogous to that of the \code{ImageMagick} 
  display. This function is not available on Windows due to the lack of support
  in the \code{ImageMagick} API.
  
  \code{GTK} display does not use \code{GTK} widgets available from other \code{R}
  packages, the functionality is coded in \code{EBImage}. Therefore, the package
  must be compiled with \code{GTK} support in order to enable it.
}

\seealso{ \code{
    \link{Image-class}, \link{Image}, \link{read.image}, \link{write.image}
}}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}; \emph{GTK}: 
  \url{http://www.gtk.org}; \emph{GTK on Windows}: \url{http://gladewin32.sf.net}
}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2005-2007
}

\keyword{file}

