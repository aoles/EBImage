\name{floodFill}

\alias{floodFill}
\alias{fillHull}

\title{Filling matrix data with flood fill algorithm}

\description{
  Functions to fill regions in images and matrices/arrays and fill holes in 
  indexed objects.
}

\usage{
  floodFill(x, pt, col, tolerance=1e-3)
  fillHull(x)
}

\arguments{

  \item{x}{ An \code{\link{array}}-like object (e.g. \code{\linkS4class{Image}}) 
    to be filled. For \code{fillHull} this must be an instance of \code{\linkS4class{Image}}.}

  \item{pt}{ An integer of 2 values for \code{x-y} coordinates of the point where
    fill starts. }

  \item{col}{ A value for the fill color, supposedly in the same storage mode as 
    \code{x}. If \code{x} is an image \code{col} will be converted to the 
    corresponding storage mode using the \code{\link{channel}} function, and thus
    can be a character value specifying the color in X11 mode.}

  \item{tolerance}{ Color tolerance used during the fill. }
}

\value{
  A copy of \code{x} with the fill applied.
}

\details{
  If no color is specified, the color under the coordinates of \code{pt} is used 
  (this only has sense if tolerance is not zero). The flood fill is implemented
  using the fast scan line algorithm. It is assumed that \code{floodFill} is
  run either on matrices of single-framed images. If the latter is not the case,
  only the first frame will be modified!
  
  \code{fillHull} files holes in objects of \code{\linkS4class{Image}}'s.
}

\seealso{\code{\linkS4class{Image}}}


\author{
  Gregoire Pau, Oleg Sklyar; 2007
}

\examples{
if (interactive()) {
  data(imageWithHoles)

  display(x, main="Original image")
  display(floodFill(x,c(5,5),tolerance=0.2))
  display(floodFill(x,c(40,40),tolerance=0.2))

  mask = x
  mask[mask>0.3]=1
  mask[mask<=0.3]=0

  y = floodFill(mask,c(40,40),2)
  display(y, main="Indexed mask with holes")
  
  y = fillHull(y)
  display(y, main="Indexed mask without holes")

  rgb = channel(normalize(y), "rgb")
  rgb = floodFill(rgb, c(1,1), "red")
  rgb = floodFill(rgb, c(50,50), "blue")
  rgb = floodFill(rgb, c(150,150), "green")
  display(rgb)
}}

\keyword{file}

