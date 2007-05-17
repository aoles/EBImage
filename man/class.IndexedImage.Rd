\name{IndexedImage-class}

\docType{class}

\alias{IndexedImage-class}
\alias{as.Image}

\concept{image representation}
\concept{image processing}
\concept{image class}
\concept{image object}

\title{ Defintion of class 'IndexedImage' }

\description{
  The class \code{IndexedImage} is used to store the results of functions that
  index image into separate objects using integer 1-based indexing. Such an
  image is essentially the same as \code{\link{Image-class}} in the gray scale
  mode. The class was defined to ensure correct types in calls to other object
  detection and processing routines.
}

\section{ Class Definition }{ 
\preformatted{
## S4 class definition

setClass( "IndexedImage", contains="Image" )

}
}

\section{ Creating objects }{
  Objects of this class should not be created directly, but rather as the result
  of calls to \code{\link{watershed}}, \code{\link{propagate}} etc.
}


\section{ Accessor methods }{

  None defined specifically for this class, but all of the parent 
  \code{\link{Image-class}} and \code{\link{array}} are accessible.

  There is one coersion routine defined in addition
\preformatted{
## S4 method for signature x='IndexedImage'

as.Image(x)
}

which simply sets the class attribute to \code{Image}.
}

\seealso{\code{
  \link{Image}, \link{watershed}, \link{propagate}
}}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\examples{
if ( interactive() ) {
  \dontrun{Load an image}
  ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
  a <- read.image( paste(ddir, "A04w0.jpg", sep="/") )

  \dontrun{Detect nuclei}
  t <- thresh( blur(a, 4, 2), 15, 15)
  t <- opening( closing(t, morphKern(9)) )
  objects <- watershed( distmap(t) )

  display(normalize(objects))
  class(objects)
}
}  
  


\keyword{methods}
\keyword{classes}
