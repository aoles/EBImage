\name{draw}

\alias{draw}

\docType{function}

\title{Draw primitives on images}

\description{
    \code{draw} allows for drawing of primitives on images. The primitives are defined
    as derived from the virtual class \code{\link{Drawable}}. The following primitives are supported
    at the moment: \code{\link{DrawableCircle}} - circles, \code{\link{DrawableRect}} - rectangles,
    \code{\link{DrawableEllipse}} - ellipses, \code{\link{DrawableLine}} - lines. To come shortly
    \code{\link{DrawableText}} to display texts.
}

\usage{
    draw(x, drawable)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{drawable}{An instance of any class derived from \code{\link{Drawable}}.}
}
\value{
    A new instance of class \code{\link{Image}} with the same size as \code{x},
    but with the objects drawn.
}

\seealso{
   \code{\link{Image}}, \code{\link{Drawable}}
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    # create an empty gray image of 300x200 size
    im1 <- Image(0.5, c(300,200,1))
    # create a drawable object with 3 circles with centres (40,30), (140,90) and (200,250)
    # and radii 30, 100 and 80
    circles <- DrawableCircle(c(40,140,200), c(30,90,250), c(30,100,80))
    # set not to fill objects
    circles@doFill <- FALSE
    # draw circles and get a new image
    im2 <- draw(im1, circles)
    \dontrun{display(im2)}
}

\keyword{dplot}
\keyword{manip}

