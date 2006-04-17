\name{distMap}

\alias{distMap}

\docType{function}

\title{Image Distance Maps}
\description{
\code{distMap} operates on grayscale images that are interpreted as
binary, with two classes of pixels: those with a value of 0 are
considerd "background", all others are considered "foreground".
For every pixel the function calculates the distance to the nearest
background pixel. Distance maps are useful for segmentation if objects
overlap, and for deriving object size and shape descriptors.
}

\usage{
    distMap(x, alg = "EBImage", modify = FALSE)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects,
        the algorithm will be applied to every single image composed of first two dimensions.}
    \item{alg}{Character scalar, the algorithm to use. The default value
      is exact, simple and slow. Currently, the only alternative is \code{Lotufo\_Zampirolli}
      which does not return real distances to the background though.}
% FIXME - can you please explain, what do you mean by "real" and if it
% does not work, we should not support it (but I suppose it does work,
% we just need to understand and document it properly)
    \item{modify}{Logical scalar, if TRUE, \code{x} will be
      modified in place (for performance and contrary to usual R pass-by-value semantics; see vignette)}
}

\value{
If \code{modify} is \code{FALSE}, a new instance of class
\code{\link{Image2D}} or \code{\link{Image3D}} with the same dimensions
as \code{x}. If \code{modify} is \code{TRUE}, the invisible modified
\code{x}.  It contains the distances of the foreground pixels of
\code{x} to the background.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}, \code{\link{objectCount}}}
}

\source{
The C code for the Lotufo-Zampirolli algorithm was adapted from the \code{animal} imaging library by Ricardo Fabbri.
}

\references{
The algorithm \code{Lotufo\_Zampirolli} is from R. Lotufo, F. Zampirolli, SIBGRAPI 2001, 100-105, 2001.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    im = read.image("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/segmented.tif")
    # get a copy of the image
    imCopy = copyImage(im)
    # convert original image to distance map
    distMap(im, modify = TRUE)
    # calculate distance map from the originally copied image
    dm = distMap(imCopy)
    # compare original image, im, with dm or run objectCount for im or dm


x=Image2D(numeric(100), dim=c(10,10))
for(j in 2:9) x[2:9, j]=1

d1 = distMap(x, "Lotufo_Zampirolli")
d2 = distMap(x)
identical(sqrt(d1),  d2)

}

\keyword{dplot}
\keyword{manip}
\keyword{array}

