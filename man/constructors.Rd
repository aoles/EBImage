\name{Image}

\alias{Image}
\alias{combine}
\alias{combine,Image,Image-method}
\alias{combine,list,missing-method}
\alias{copy}
\alias{copy,Image-method}
\alias{header}
\alias{header,Image-method}
\alias{assert}
\alias{assert,Image,Image-method}
\alias{assert,Image,missing-method}
\alias{is.Image}
\alias{stopIfNotImage}

\title{Image creation, copying and assertion}

\description{
  Functions to create, copy and assert images.
}

\usage{
  Image(data=matrix(0,0,0), dim=base::dim(data), colormode=Grayscale, ...)

  is.Image(x)
  stopIfNotImage(x)

  ## S4 methods for signature 'x=Image'
  copy(x, ...)
  header(x, ...)

  ## S4 methods for signatures 'x=Image,y=Image'
  ## and 'x=list,y=missing'
  combine(x, y, ...)

  ## S4 methods for signatures 'x=Image,y=Image'
  ## and 'x=Image,y=missing'
  assert(x, y, strict=FALSE, ...)
}


\arguments{
  \item{x, y}{Objects of class \code{\linkS4class{Image}}. There is also
    a \code{combine} method for lists of equally sized \code{\linkS4class{Image}}
    objects, in that case \code{y} should be missing.}

  \item{data}{Data to fill the image, typically an array,
    but can be any object for which \code{as.numeric} or
    \code{as.integer} is defined.}

  \item{dim}{A numeric vector of image dimensions with length of 2 or 3.
    If its length is 2, the third dimension is set to 1.}

  \item{colormode}{An integer value for the image data color mode.
    It is recommended to use the predefined symbols \code{\link{TrueColor}} or
    \code{\link{Grayscale}}.}

  \item{strict}{A logical scalar. If \code{TRUE}, the size of all three dimensions of
    two images will be compared, if \code{FALSE}, the function will
    compare only the first two dimensions (i.e. stacks can have different
    size in z-direction, but x- and y-size should be the same).}

  \item{...}{With \code{Image}, further arguments to \code{\link{new}};
    with \code{combine}, further images to be combined.}
}

\value{
  The constructors \code{Image}, \code{copy}, \code{combine} and \code{header} 
  return a new object of \code{\linkS4class{Image}}.

  \code{assert} and \code{is.Image} return a \code{logical}.

  \code{stopIfNotImage} will return invisible \code{NULL} if its argument is of
  \code{\linkS4class{Image}} and an error message otherwise.
}

\details{

  \describe{

    \item{\code{Image}}{This is a wrapper around \code{\link{new}}, for 
      convenience.}

    \item{\code{copy}}{Makes an identical copy of an object of
      \code{\linkS4class{Image}} enforcing allocation of new memory for
      the image data. Note that in R, a simple assignment like
      \code{a<-b} does lead to copying of the data until either \code{a}
      or \code{b} are further modified.}

    \item{\code{combine}}{Acts similarly to \code{\link{rbind}} and
      \code{\link{cbind}}. It allows to combine images to stacks adding further
      images at the back of the first one. Properties of the first image in
      the argument \code{x} are transferred to the result. All images must be of
      the same size (in first two dimensions) and color mode. If applied to
      a list of images, it calls \code{\link{do.call}("combine", x)} }

    \item{\code{header}}{Acts similarly to copy, but does not copy the actual
      image data, only all the other slots. This function can be useful
      for creating new images from existing large ones preserving
      attributes.}

    \item{\code{is.Image}}{ Returns \code{TRUE} if argument is a valid
      \code{Image} and \code{FALSE} otherwise. }

    \item{\code{assert}}{ Compares dimensions and color modes of two images.
      If argument \code{strict} is \code{FALSE} images are allowed to have
      different number of frames.}

  }
}

\seealso{
  \code{
    \linkS4class{Image}, \linkS4class{IndexedImage}, \link{readImage}
  }
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{
  i1 = Image()
  sx = exp(24i*pi*seq(-1, 1, length=300)^2)
  i2 = Image(outer(Im(sx), Re(sx)))
  if (interactive()) display(normalize(i2))
  i3 <- copy(i2)
  is.Image(i2)
}

\keyword{file}

