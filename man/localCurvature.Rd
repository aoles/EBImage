\name{localCurvature}

\alias{localCurvature}

\title{Local Curvature}

\description{
  Computes signed curvature along a line.
}

\usage{
localCurvature(x, h, maxk)
}

\arguments{
  \item{x}{A data frame or matrix of dimensions N x 2 containing the coordinates 
    of the line, where N is the number of points. The points should be ordered according 
    to their position on the line. The columns should contain the x and y coordinates. 
    The curvature calculation is unaffected by any permutation of the columns.
    Directly accepts a list element from \code{ocontour}.}
  \item{h}{Specifies the length of the smoothing window. See \code{locfit::lp} for more details.}
  \item{maxk}{See \code{locfit::locfit.raw} for details.}
}

\value{
  Returns a \code{list} containing the contour coordinates \code{x}, the signed curvature at each point \code{curvature} 
  and the arc length of the contour \code{length}.
}

\details{
 \code{localCurvature} fits a local non-parametric smoothing line (polynomial of degree 2) 
 at each point along the line segment, and computes the curvature locally using numerical derivatives.
}

\author{
  Joseph Barry, Wolfgang Huber, 2013
}

\seealso{
  \code{\link{ocontour}}
}

\examples{
  ## curvature goes as the inverse of the radius of a circle
  range=seq(3.5,33.5,by=2)
  plotRange=seq(0.5,33.5,length.out=100)
  circleRes=array(dim=length(range))
  names(circleRes)=range
  for (i in  seq_along(1:length(range))) {
   y=as.Image(makeBrush('disc', size=2*range[i]))
   y=ocontour(y)[[1]]
   circleRes[i]=abs(mean(localCurvature(x=y,h=range[i])$curvature, na.rm=TRUE))
  }
  plot(range, circleRes, ylim=c(0,max(circleRes, na.rm=TRUE)), xlab='Circle Radius', ylab='Curvature', type='p', xlim=range(plotRange))
  points(plotRange, 1/plotRange, type='l')

  ## Calculate curvature
  x = readImage(system.file("images", "shapes.png", package="EBImage"))[25:74, 60:109]
  x = resize(x, 200)
  y = gblur(x, 3) > .3
  display(y)
  
  contours = ocontour(bwlabel(y))
  c = localCurvature(x=contours[[1]], h=11)
  i = c$curvature >= 0
  pos = neg = array(0, dim(x))
  pos[c$contour[i,]+1]  = c$curvature[i]
  neg[c$contour[!i,]+1] = -c$curvature[!i]
  display(10*(rgbImage(pos, , neg)), title = "Image curvature")
}
