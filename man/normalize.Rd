\name{normalize}

\alias{normalize}
\alias{normalize,array-method}
\alias{normalize,matrix-method}
\alias{normalize,Image-method}

\concept{normalization}

\title{Intensity values linear scaling}

\description{
  Linearly scale the intensity values of an image to a specified range.
}

\usage{
\S4method{normalize}{Image}(object, separate=TRUE, ft=c(0,1), inputRange)%

\S4method{normalize}{array}(object, separate=TRUE, ft=c(0,1), inputRange)
}

\arguments{
  \item{object}{an \code{Image} object or an array}

  \item{separate}{if \code{TRUE}, normalizes each frame separately}

  \item{ft}{a numeric vector of 2 values, target minimum and maximum 
    intensity values after normalization}
    
  \item{inputRange}{a numeric vector of 2 values, sets the range of the input
    intensity values; values exceeding this range are clipped}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{object}.
}

\details{
  \code{normalize} performs linear interpolation of the intensity values of an image to the specified range \code{ft}. If \code{inputRange} is not set the whole dynamic range of the image is used as input. By specifying \code{inputRange} the input intensity range of the image can be limited to [min, max]. Values exceeding this range are clipped, i.e. intensities lower/higher than \code{min}/\code{max} are set to \code{min}/\code{max}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2013
}

\examples{
  x = readImage(system.file('images', 'shapes.png', package='EBImage'))
  x = x[110:512,1:130]
  y = bwlabel(x)
  if (interactive()) display(x, title='Original')

  print(range(y))
  y = normalize(y)
  print(range(y))
  if (interactive()) display(y, title='Segmented')
}
