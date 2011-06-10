\name{display}

\alias{display}
\alias{animate}

\concept{display}

\title{Interactive image display}

\description{
  Display images.
}

\usage{
display(x, title=paste(deparse(substitute(x))), useGTK=TRUE)
animate(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array.} 
  \item{useGTK}{A logical of length 1. See details.} 
  \item{title}{Window title.}
}

\value{
  The functions are called for their side effect.
  Return value is invisible \code{NULL}.
}

\details{
  By default (and if available), the \code{display} function uses GTK to
  open a window and display the image. Multiple windows can be opened in
  this way.
  
  If GTK is not available or if \code{useGTK} is \code{FALSE},
  ImageMagick is used; only one window at a time can be open,
  and it needs to be closed by the user interactively before
  the next window can be opened. The ImageMagick display is not
  available on MS-Windows.
  
  The \code{animate} function shows an animated sequence of images and uses \code{ImageMagick}. Similar limitations as for \code{display} apply
  (only one window, not on MS-Windows.)
}

\references{
  ImageMagick: \url{http://www.imagemagick.org}
  GTK: \url{http://www.gtk.org}, on MS-Windows \url{http://gladewin32.sf.net}
}

\examples{
   ## single image
   lena = readImage(system.file("images", "lena-color.png", package="EBImage"))
   if (interactive()) display(lena)

   ## animated threshold
   x = readImage(system.file("images", "lena-color.png", package="EBImage"))
   x = resize(x, 128, 128)
   xt = list()
   for (t in seq(0.1, 5, len=9)) xt=c(xt, list(blur(x, s=t)))
   xt = combine(xt)
   if (interactive()) display(xt, title='Blurred Lenas')
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

