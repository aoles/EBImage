\name{normalize}


\alias{normalize}

\concept{normalization}

\title{Intensity values linear scaling}

\description{
  Linearly scale the intensity values of an image to a specified range.
}

\usage{
  normalize(x, separate=TRUE, ft=c(0,1))
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{separate}{If \code{TRUE}, normalizes each frame separately.}

  \item{ft}{ A numeric vector of 2 values, target minimum and maximum 
    intensity values after normalization. }
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
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



