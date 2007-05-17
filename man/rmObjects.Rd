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
## S4 methods for signature x='IndexedImage', index='numeric' and index='list'

rmObjects(x, index)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}. }

  \item{\code{index}}{ A numeric (integer) vector of indexes of objects to
    remove in the frame if \code{x} contains one frame only. For multiple
    frames, a list of such vectors.}

}}

\section{ Value }{

  An image of \code{\link{IndexedImage-class}}.

}

\seealso{
  \code{
    \link{IndexedImage-class}, \link{watershed}, \link{getObjects}, \link{paintObjects}
  }
}

\examples{
  \dontrun{ FIXME: redo the example as new signature is available for 2D}

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

