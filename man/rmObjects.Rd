\name{rmObjects}

\alias{rmObjects}

\concept{object detection}
\concept{delete objects}

\title{ Delete objects }

\description{
  The \code{rmObjects} functions deletes objects indexed by a list of
  integer vectors (indexes) from an image with indexed objects.
}

\section{ Usage }{
  \preformatted{
    # S4 methods for class 'Image':

    rmObjects(x, index)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the
    \code{\link{Grayscale}} mode with integer-absed object index, as
    returned by \code{\link{watershed}}. }

  \item{\code{index}}{ A list of integer vectors (one vector per image)
    with indexes of existing objects that will be deleted. }

}}

\section{ Value }{

  An image of \code{\link{Image-class}}, same as in \code{\link{watershed}}.

}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{getObjects}, \link{paintObjects}
  }
}

\examples{
  if ( interactive() ) {
    ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
    a <- read.image( paste(ddir, "A04w1.jpg", sep="/") )
    w <- watershed( distmap( thresh(a, 10, 10) ) )
    display( normalize(w) )

    \dontrun{Often one need to delete small objects, here is the}
    \dontrun{straightfoward way to construct an index}
    index <- lapply( getObjects(w), function(x) which( x[, "size"] < 30) )

    \dontrun{now we delete these small objects}
    w <- rmObjects( w, index )
    display( normalize(w) )

  }
}

\author{
    Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

