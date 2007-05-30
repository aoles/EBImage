\name{normalize}

\alias{negate}
\alias{normalize}
\alias{normalize2}

\concept{normalization}

\title{ Functions to normalize images }

\description{
  Functions to normalize images.
}

\section{ Usage }{
  \preformatted{
    negate(x)
    normalize(x, separate=TRUE, ft=c(0,1))
    normalize2(x)
  }
  
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{Image-class}}. }

  \item{\code{separate}}{ If \code{TRUE} normalizes each frame separately. }

  \item{\code{ft}}{ A numeric vector of 2 values, target minimum and maximum 
    intensity values after normalization for \code{\link{Grayscale}} images. }
}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
}

\section{ Details }{

  \code{negate} negates the colors in the reference image. Operates on all image
    modes.
  
  \code{normalize} normalizes \code{\link{Grayscale}} images to the given range.
    
  \code{normalize2} uses \code{ImageMagick} normalization routine to normalize
    \code{\link{Grayscale}} or \code{\link{TrueColor}} images.
   
}

\seealso{
  \code{
    \link{Image-class}, \link{channel}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar: \email{osklyar@ebi.ac.uk}   
}

\examples{
  \dontrun{see ?watershed for examples}
}

\keyword{file}

