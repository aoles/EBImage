\name{drawtext}

\alias{drawtext}
\alias{drawfont}

\title{Draw text on images.}

\description{
  Draws text on images.
}

\usage{
  drawtext(img, xy, labels, font, col)
    
  drawfont(family=switch(.Platform$OS.type, windows="Arial", "helvetica"), 
           style="n", size=14, weight=200, antialias=TRUE)
}

\arguments{
  \item{img}{An \code{Image} object or an array.}

  \item{xy}{Matrix (or a list of matrices if \code{img} contains multiple frames) 
    of coordinates of labels.}

  \item{labels}{A character vector (or a list of vectors if \code{img} contains multiple frames) 
    containing the labels to be output.}

  \item{font}{A font object, returned by \code{drawfont}. If missing, a default OS-dependent
   font will be chosen.}

  \item{col}{A character vector of font colors.}

  \item{family}{A character value indicating the font family to use. Valid
    examples on Linux/UNIX systems include \code{helvetica}, \code{times},
    \code{courier} and \code{symbol}. Valid examples on Windows machines 
    include TrueType like \code{Arial} and \code{Verdana}.}
  
  \item{style}{A character value specifying the font style to use. 
    Supported styles are: \code{normal} (default), \code{italic}, 
    and \code{oblique}. }
  
  \item{size}{Font size in points.}

  \item{weight}{A numeric value indicating the font weight (bold font). Supported
    values range between 100 and 900.}

  \item{antialias}{A logical value indicating whether the font should be 
    anti-aliased.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{img}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  lena = readImage(system.file("images", "lena-color.png", package="EBImage"))
  font = drawfont(weight=600, size=28)
  lena = drawtext(lena, xy=c(250, 450), labels="Lena", font=font, col="white")
  if (interactive()) display(lena)
}


