\name{abind}

\docType{methods}

\alias{abind}
\alias{abind-methods}
\alias{abind,ANY-method}
\alias{abind,Image-method}
\alias{abind,list-method}

\title{Combine Image Arrays}

\description{
  Methods for function \code{\link[abind]{abind}} from package \pkg{abind} useful for combining \code{Image} arrays.
}

\section{Usage}{
\code{abind(...)}
}

\section{Arguments}{
  \describe{
    \item{\code{\ldots}}{Arguments to \code{\link[abind]{abind}}}
  }
}

\section{Methods}{
\describe{
\item{\code{signature(... = "Image")}}{
 This method is defined primarily for the sake of preserving the class of the combined \code{Image} objects. Unlike the original \code{\link[abind]{abind}} function, if \code{dimnames} for all combined objects are \code{NULL} it does not introduce a list of empty \code{dimnames} for each dimension.
}
\item{\code{signature(... = "ANY")}}{
  Dispatches to the original \code{\link[abind]{abind}} function.
}
}}

\value{
  An \code{Image} object or an array, containing the combined data arrays of the input objects.
}

\seealso{
   \code{\link{combine}} provides a more convenient interface to merging images into an image sequence. Use \code{\link{tile}} to lay out images next to each other in a regular grid.
}

\author{
  Andrzej Oleś, \email{andrzej.oles@embl.de}, 2017
}

\examples{
  f = system.file("images", "sample-color.png", package="EBImage")
  x = readImage(f)
  
  ## combine images horizontally
  y = abind(x, x, along=1)
  display(y)
  
  ## stack images one on top of the other
  z = abind(x, x, along=2)
  display(z)
}

\keyword{methods}
