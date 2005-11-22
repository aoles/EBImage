\name{im.distMap}
\alias{im.distMap}
\docType{function}

\title{Distance Map Transformations}
\description{Distance Map Transformations}

\usage{
im.distMap(x, alg = "Lotufo_Zampirolli", ...) 
}
\arguments{
  \item{x}{An object of class \code{\link[class-rimage]{rimage}}. It is
    interpreted as a binary image, where the regions of the image that
    have the value 0 are considered the background, and the regions
    with values greater than 0 are considered foreground. The distance
    map is calculated for the foreground.}
  \item{alg}{Character. Currently, only Lotufo\_Zampirolli is
    implemented.}
  \item{...}{Further arguments that get passed on to
    \code{\link{.warnOnStack}}}
}
\value{An object of class \code{\link[class-rimage]{rimage}}}


\details{
  ...
}

\seealso{
  \code{\link{Filters 2D}}
}

\source{
  The C code for this function was adapted from the
  \code{animal} imaging library by Ricardo Fabbri.
}

\references{
  The algorithm \code{Lotufo\_Zampirolli} is from
  R. Lotufo, F. Zampirolli, SIBGRAPI 2001, 100-105, 2001.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

