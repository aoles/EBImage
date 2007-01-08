\name{combineObjects}

\alias{combineObjects}

\concept{object detection}

\title{ Combine smaller objects detected in images with their neighbours }

\description{
  In an indexed image of objects, obtained for example with 
  \code{\link{watershed}} this function identifies objects that have neighbours
  and tries to combine them.
}

\section{ Usage }{
  \preformatted{
    combineObjects(x, ext=1, fraction=0.3, ...) 
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the 
    \code{\link{Grayscale}} mode with integer-absed object encoding, as
    returned by \code{\link{watershed}}. }

  \item{\code{ref}}{A reference grayscale image to calculate object intensity. 
    Can be \code{NULL}. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  An image of \code{\link{Image-class}}, same as in \code{\link{watershed}}.
  If image contained a feature matrix, it will be removed, use 
  \code{\link{getObjects}} to recreate it.
    
}

\section{ Details }{

  An object will be combined to a neighbouring one
  the fraction of its border touching the other object is larger than a 
  specified value. Execute this function several times if needed since multiple 
  combinations of the same object are possible. This is not done automatically
  until no changes in number of objects are detected, because such behaviour 
  is not always necessary and desirable.

  Effectively, the function scans over values in \code{(0,1)}, i.e. between 
  background 0, and the first object, 1, because edges are supposed to be 
  encoded as 0.5. Objects are not combined if their mutual border is not 
  marked by a value in this range.
  
  The function will reindex the colour in the image to remove missing objects,
  i.e. if of 20 objects 3 were combined to some other, then there will be
  17 objects left indexed from 1 to 17. Mutual borders of combined objects will
  be set to the same index as the combined object.
  
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
    \dontrun{See ?watershed for example}
  }
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

