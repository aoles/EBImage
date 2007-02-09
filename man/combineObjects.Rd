\name{combineObjects}

\alias{combineObjects}
\alias{rmObjects}

\concept{object detection}
\concept{delete objects}

\title{ Combine connecting objects and delete objects }

\description{
  Given an image of indexed objects (obtained for example with
  \code{\link{watershed}} or \code{\link{propagate}}) this function
  identifies objects that have connecting neighbours and tries to
  combine them.
}

\section{ Usage }{
  \preformatted{
    # S4 methods for class 'Image':

    combineObjects(x, fraction=0.3, ext=1, seeds=NULL)
    rmObjects(x, index, ext=1)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the
    \code{\link{Grayscale}} mode with integer-absed object index, as
    returned by \code{\link{watershed}}. }

  \item{\code{ext}}{ Extension of neighboring pixels used in object
    detection: the width of the contact line. Should match the value
    in \code{\link{watershed}} if used for segmentation. }

  \item{\code{fraction}}{ A numeric in the range \code{(0,1]} giving
    the fraction of perimeter pixels of an object that need to have
    neighbors from other objects for this one to be considered for
    combining. }

  \item{\code{seeds}}{ A list of integer vectors (one vector per image)
    with indexes of existing objects that should serve as seeds --
    the program will try to combine objects to the given ones. The list
    can be obtained e.g. from \code{\link{matchObjects}}. }

  \item{\code{index}}{ A list of integer vectors (one vector per image)
    with indexes of existing objects that will be deleted. }

}}

\section{ Value }{

  An image of \code{\link{Image-class}}, same as in \code{\link{watershed}}.
  If image contained a feature matrix, it will be removed, use
  \code{\link{getObjects}} to recreate it.

}

\section{ Details }{

  An object will be combined with a neighbouring one if
  the fraction of its perimeter touching the other object is larger
  than a specified value. Execute this function several times if needed
  since multiple combinations of the same object are possible. This is
  not done automatically, because such behavior is not always necessary
  and nor desirable.

  Effectively, the function scans over values in \code{(0,1)}, i.e. between
  background 0, and the first object, 1, because edges are supposed to be
  encoded as 0.5. Objects are not combined if their mutual border is not
  marked by a value in this range.

  The function will reindex the color indexes in the image to
  remove missing objects, i.e. if out of 20 objects 3 were combined with
  other, then there will be 17 objects left indexed from 1 to 17.
  Mutual borders of combined objects will be set to the same index as the
  combined object.

  The same reindexing will be applied in \code{rmObjects}.

}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{getObjects}, \link{paintObjects}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\examples{
  if ( interactive() ) {
    \dontrun{Often one need to delete small objects, here is the}
    \dontrun{straightfoward way to construct an index}
    ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
    a <- read.image( paste(ddir, c("A04w0.jpg", "A04w1.jpg"), sep="/") )
    t <- thresh( a, 50, 50, 0.03 )
    t <- closing( t, morphKern(7) )
    dm <- distmap(t, 0.0)
    x <- watershed(dm, ext=1)
    res <- combine( a[,,1], normalize(x[,,1]), a[,,2], normalize(x[,,2]) )

  }
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}
}

\keyword{file}

