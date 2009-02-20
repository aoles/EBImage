\name{display}

\alias{display}
\alias{animate}

\concept{display}

\title{Interactive image display}

\description{
  Display images.
}

\usage{
display(x,
        no.GTK, main, colorize,
        title = paste(deparse(substitute(x))),
        useGTK = TRUE, ...)
animate(x,
        ...)
}

\arguments{
  \item{x}{An object of \code{\linkS4class{Image}}.} 
  \item{useGTK}{A logical of length 1. See details.} 
  \item{title}{Window title.}
  \item{no.GTK, main, colorize}{Defunct.}
  \item{...}{Currently not used.}
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
  
  The \code{animate} function uses \code{ImageMagick}. Similar limitations as for \code{display} apply
  (only one window, not on MS-Windows.)
}

\references{
  ImageMagick: \url{http://www.imagemagick.org}
  GTK: \url{http://www.gtk.org}, on MS-Windows \url{http://gladewin32.sf.net}
}

\examples{
  if(interactive()) {
    Lena = readImage(system.file("images", "lena-color.png", package="EBImage"))
    display(Lena)
  }
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

