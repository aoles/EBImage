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

  \code{negate(x, ...)}
  
  \code{normalize(x, from=0, to=1, ...)}

  \code{normalize2(x, ...)}
  
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{Image-class}}. }

  \item{\code{from, to}}{ Target minimum and maximum intensity values after
    normalization for \code{\link{Grayscale}} images. }

  \item{\code{...}}{ Reserved for future use. }
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
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

