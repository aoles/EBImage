\name{Drawable}

\docType{class}

\alias{Drawable}
\alias{DrawableCircle}
\alias{DrawableRect}
\alias{DrawableEllipse}
\alias{DrawableLine}
%\alias{DrawableText}
\alias{Drawable-class}
\alias{DrawableCircle-class}
\alias{DrawableRect-class}
\alias{DrawableEllipse-class}
\alias{DrawableLine-class}
%\alias{DrawableText-class}

\concept{image analysis}
\concept{image processing}

\title{
    Classes to define primitives that can be drawn on images
}

\description{
    Classes that define primitives that can be drawn on images. All classes are derived from
    class \code{Drawable} extending its properties. These classes define essential coordinates and
    parameters of objects (radii etc) as well as filling method and colors.

    Class \code{Drawable} is a interface no objects of this class should be created, insted objects
    of derived classes with particular properties have to be created.
}

\section{Creating Objects}{
    \describe{
        \item{\code{DrawableCircle(x, y, z)}:}{Default constructor for circles.}
        \item{\code{DrawableRect(x1, y1, x2, y2)}:}{Default constructor for rectangles.}
        \item{\code{DrawableEllipse(x1, y1, x2, y2, sgrad = 0, egrad = 360)}:}{Default constructor for ellipses.}
        \item{\code{DrawableLine(x1, y1, x2, y2)}:}{Default constructor for lines.}
    }
}

\section{Slots}{
    \describe{
        \item{\code{x}:}{Matrix of coordinates and parameters: 3 columns of (x,y,r) for circles;
            4 columns of (x1,y1,x2,y2) - bounding box coordinates for ellipses, lines and rectangles}
        \item{\code{strokeColor}:}{Stroke color as character in X11 format}
        \item{\code{strokeWidth}:}{Stroke width in pixels}
        \item{\code{fillColor}:}{Fill color as character in X11 format}
        \item{\code{doFill}:}{A logical value if objects have to be filled.}
        \item{\code{fillOpacity}:}{A numeric value between 0 and 1 for filled object transparency.
            0 - transparent and 1 - opacue fill.}
    }
}

\section{Methods}{
    No methods defined. Objects are used as parameters in calls to \code{\link{draw}} function for
    class \code{\link{Image}}.
}

\details{
    Several objects of one type can be plotted at a time. For this constructor functions must be supplied
    with equal-length vectors of coordinates (see example below). \code{sgrad} and \code{egrad} indicate
    start and end angles of arcs to draw.
}

\seealso{
   \code{\link{Image}}, \code{\link{draw}}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
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

\keyword{methods}
\keyword{classes}
