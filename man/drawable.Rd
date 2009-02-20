\name{drawtext}

\alias{drawtext}
\alias{drawtext,Image,list,list-method}
\alias{drawfont}

\concept{drawing}
\concept{text}

\title{Draw text on images.}

\description{
  Draw text on images.
}

\usage{
  drawtext(img, xy, labels, ...) 
  \S4method{drawtext}{Image,list,list}(img, xy, labels, font, col, ...)
    
  drawfont(family=switch(.Platform$OS.type, windows="Arial", "helvetica"), 
           style="n", size=14, weight=200, antialias=TRUE)
}

\arguments{

  \item{img}{ An object of \code{\linkS4class{Image}}. With indexed images,
    please ensure that the color range is in \code{[0,1]}.}

  \item{xy}{ (x,y) coordinates of labels. For single frames a matrix with 
    the first column being \code{x} and second \code{y} or a corresponding numeric 
    vector as it would be used to construct such a matrix. For multiple frames,
    a list of corresponding matrices/vectors. }

  \item{labels}{ A character vector of labels to be output. For multiple frames
    a list of such characters. }

  \item{font}{ An S3 object of class \code{DrawFont} as returned by 
    \code{drawfont}. If omitted, the \code{drawfont} functions is called internally
    to obtain the default values. }

  \item{col}{A character vector of font colors. One per frame, recycled
    between frames if required. }
  
  \item{...}{ Reserved. }

  \item{family}{A character value for the font family to use. On Linux/UNIX
    machines one can try to use \code{helvetica}, \code{times}, \code{courier} 
    and \code{symbol}. On Windows machines, one can specify installed TrueType
    fonts, like \code{Arial}.}
  
  \item{style}{A character value for the font style to use. Can be
    specified by providing the first letter only. Supported are: \code{normal} 
    (default), \code{italic}, \code{oblique}. }
  
  \item{size}{A numeric value for the font size.}

  \item{weight}{A numeric value for the font weight (bold font). Supported
    values between 100 and 900.}

  \item{antialias}{A logical value for whether the font should be 
    anti-aliased. }
}

\value{
  An object of \code{\linkS4class{Image}}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
## if ( length(grep("apple", Sys.getenv("R_PLATFORM"))) == 0 ) {
  Lena = readImage(system.file("images", "lena-color.png", package="EBImage"))
  Lena = drawtext(Lena,
     xy = c(250, 450),
     labels = "Lena",
     font = drawfont(weight=600, size=28),
     col = "brown")
  if (interactive())
    display(Lena)
## }
}

\keyword{file}

