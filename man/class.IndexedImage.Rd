\name{IndexedImage-class}

\docType{class}

\alias{IndexedImage-class}
\alias{as.Image,IndexedImage-method}

\concept{image representation}
\concept{image processing}
\concept{image class}
\concept{image object}

\title{ Defintion of class 'IndexedImage' }

\description{
  The class \code{IndexedImage} is used to store the results of functions that
  index image into separate objects using integer 1-based indexing. Such an
  image is essentially the same as \code{\linkS4class{Image}} in the gray scale
  mode. The class was defined to ensure correct types in calls to other object
  detection and processing routines.
}

\section{ Class Definition }{ 
  \preformatted{
    ## S4 class definition
    setClass( "IndexedImage", contains="Image" )
}}

\section{ Creating objects }{
  Objects of this class are not supposed to be created directly, rather as the 
  result of calls to \code{\link{watershed}} or \code{\link{propagate}}, or other
  functions that index objects in images.
}

\section{ Details }{
  No accessor methods are defined for this class, however all of the 
  parents' ones are available, i.e. those of \code{\linkS4class{Image}} 
  and \code{\link{array}}.

  One coersion routine is defined in addition, which simply sets the class 
  attribute to \code{Image}:

  \preformatted{
    ## S4 method for signature 'x=IndexedImage'
    as.Image(x, ...)
  }

  When used with \code{IndexedImage}'s \code{display} by default normalizes the 
  image.
}

\seealso{\code{
  \linkS4class{Image}, \link{watershed}, \link{getFeatures}
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  ## Load an image
  ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
  a <- readImage( paste(ddir, "A04w0.jpg", sep="/") )

  ## Detect nuclei
  t <- thresh( blur(a, 4, 2), 15, 15)
  t <- opening( closing(t, morphKern(9)) )
  objects <- watershed( distmap(t) )

  class(objects)
  if ( interactive() ) display(normalize(objects))
}  

\keyword{methods}
\keyword{classes}
