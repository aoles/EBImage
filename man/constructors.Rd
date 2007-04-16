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
  \preformatted{
    Image(data=0.5, dim=c(200,200), colormode=Grayscale, ...)
    is.Image(x)
    stopIfNotImage(x)

    # S4 methods for class 'Image':

    copy(x)
    combine(x, y, ...)
    header(x)
    assert(x, y, strict=FALSE)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x, y}}{Objects of \code{\link{Image-class}}. }

  \item{\code{data}}{ A vector giving data to fill the image. }

  \item{\code{dim}}{ Image dimensions, 2D or 3D. In case of 2D, the third
    dimension is set to 1 automatically. }

  \item{\code{colormode}}{ Image color mode, \code{\link{TrueColor}} or
    \code{\link{Grayscale}}. }

  \item{\code{strict}}{If \code{TRUE} the size of all three dimensions of
    two images will be compared, if \code{FALSE}, the function will
    compare only the first two dimensions (i.e. stacks can have different
    size, but image size should be the same). }

  \item{\code{...}}{ Further arguments to fill the slot values in constructor or
    further images in combine. }

}}

\section{ Value }{

  \code{Image}, \code{copy}, \code{combine} and \code{header} will return a new
  object of \code{\link{Image-class}}.

  \code{assert} and \code{is.Image} will return a \code{logical}.
}

\section{ Details }{

  \describe{

    \item{\code{Image}}{ This is a default constructor for images. It is
      a convenience functions being a wrapper around standard \code{R}
      function \code{\link{new}}. }

    \item{\code{copy}}{ Makes an identical copy of an object of
      \code{\link{Image-class}}. It is similar to assignment, but it forces
      \code{R} to allocate new memory for the object. }

    \item{\code{combine}}{ Acts similarly to \code{\link{rbind}} and
      \code{\link{cbind}} allowing to add images into the stack. Properties of
      the first image in argument \code{x} will be transfered to the result.
      Images must be of the same size (in first two dimensions) and in the
      same color mode. }

    \item{\code{header}}{ Acts similarly to copy, but leaves image data empty --
      only copies other properties. This function is useful when creating new
      images from existing large ones changing the dimensions. In this case,
      the result will have all the properties of the source and its data
      can be assigned later using \code{\link{imageData}}. }

    \item{\code{assert}}{ Compares image dimensions and other properties.
      This is a method defined for objects of \code{\link{Image-class}},
      therefore it will generate an error if \code{x} is not an Image. Use
      \code{is.Image} to test the class of a single object.}

    \item{\code{is.Image}}{ Returns \code{TRUE} if argument is a valid
      \code{Image} and \code{FALSE} otherwise. }

    \item{\code{stopIfNotImage}}{ Prints an error message if argument is not of
      \code{\link{Image-class}}. }

  }
}

\seealso{
  \code{
    \link{Image-class}, \link{read.image}
  }
}

\author{
    2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}
}

\keyword{file}

