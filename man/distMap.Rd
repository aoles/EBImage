\name{distMap}
\alias{distMap}
\docType{function}

\title{Distance Map Transformations}
\description{Distance Map Transformations}

\usage{
    distMap(x, alg = "Lotufo_Zampirolli")
}
\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects, filters
        will be applied to every single image composed of first two dimensions.}
  \item{alg}{Character. Currently, only Lotufo\_Zampirolli is
    implemented.}
}

\value{
    New instance of class \code{\link{Image2D}} or \code{\link{Image3D}} with the same characteristics
    as the original, but with filter applied (to every single 2D image if source is 3D).
}


\details{
  ...
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}}
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

