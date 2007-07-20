\name{Image}

\alias{Image}
\alias{combine}
\alias{copy}
\alias{header}
\alias{assert}
\alias{is.Image}
\alias{stopIfNotImage}

\concept{image manipulation}

\title{ Image creation, copying and assertion }

\description{
  Functions to create, copy and assert images.
}


\section{ Usage }{

Image constructors:

\preformatted{
Image(data=0.5, dim=c(200,200), colormode=Grayscale, ...)

## S4 methods for signature x='Image':

copy(x)
header(x)

## S4 method for signatures x='Image', y='Image'; and
##                          x='list,   y='missing'

combine(x, y, ...)
}

Type assertion:

\preformatted{
is.Image(x)
stopIfNotImage(x)

# S4 methods for classes 'Image', 'Image':

assert(x, y, strict=FALSE)
}
}

\section{ Arguments }{ \describe{

  \item{\code{x, y}}{Objects of \code{\link{Image-class}} or any derived class.
    In case of \code{combine}, \code{x} can be a list of equally sized (in \code{xy}
    images) with \code{y} missing. }

  \item{\code{data}}{ Data to fill the image coercible to \code{numeric}. }

  \item{\code{dim}}{ A numeric vector of image dimensions with length of 2 or 3.
    If length is 2, the third dimension is set to 1 automatically. }

  \item{\code{colormode}}{ An integer value for the image data color mode.
    It is recommended to use constants \code{\link{TrueColor}} or
    \code{\link{Grayscale}} correspondingly. }

  \item{\code{strict}}{If \code{TRUE} the size of all three dimensions of
    two images will be compared, if \code{FALSE}, the function will
    compare only the first two dimensions (i.e. stacks can have different
    size, but image size should be the same). }

  \item{\code{...}}{ Further arguments to \code{\link{new}} when used with
    \code{Image} and further images to stack at the back when used with
    \code{combine}. }

}}

\section{ Value }{

  Constructors \code{Image}, \code{copy}, \code{combine} and \code{header} will
  return a new object of \code{\link{Image-class}}.

  \code{assert} and \code{is.Image} will return a \code{logical}.

  \code{stopIfNotImage} will return invisible \code{NULL} if argument is of
  \code{\link{Image-class}} and an error message otherwise.
}

\section{ Details }{

  \describe{

    \item{\code{Image}}{ This is the default constructor for images. This is just
      a convenience functions and it is a wrapper around \code{\link{new}}. }

    \item{\code{copy}}{ Makes an identical copy of an object of
      \code{\link{Image-class}} enforcing new allocation for image data. In
      contrast simple assignment does not copy the data until modified and
      simply assigns a pointer to the old object. }

    \item{\code{combine}}{ Acts similarly to \code{\link{rbind}} and
      \code{\link{cbind}}. It allows to combine images to stacks adding further
      images at the back of the first one. Properties of the first image in
      the argument \code{x} are transferred to the result. All images must be of
      the same size (in first two dimensions) and color mode. If applied to
      a list of images, it calls \code{\link{do.call}("combine", x)} }

    \item{\code{header}}{ Acts similarly to copy, but does not copy the actual
      image data. It copies all other slots however. This function is useful
      when creating new images from existing large ones preserving other
      attributes. }

    \item{\code{is.Image}}{ Returns \code{TRUE} if argument is a valid
      \code{Image} and \code{FALSE} otherwise. }

    \item{\code{assert}}{ Compares dimensions and color modes of two images.
      If argument \code{strict} is \code{FALSE} images are allowed to have
      different number of frames.}

  }
}

\seealso{
  \code{
    \link{Image-class}, \link{IndexedImage-class}, \link{read.image}
  }
}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\examples{
\dontrun{New Grayscale image of a black-to-white vertical gradient}
w <- 120
a <- Image((0:(w^2))/w^2, c(w,w))
if ( interactive() ) display(a)
b <- copy(a)
print( is.Image(b) )
}

\keyword{file}

