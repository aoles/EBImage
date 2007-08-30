\name{drawtext}

\alias{drawtext}
\alias{drawfont}

\concept{drawing}
\concept{text}

\title{ Drawing primitives on images }

\description{
  The family of functions to draw primitives on images. At the moment, there is
  only one functions out of the planned family to draw text.
}

\section{ Usage }{
  \preformatted{
    ## S4 methods for img='Image', xy='list', labels='list'
    ##                img='Image', xy='matrix'/'numeric', labels='character'
    drawtext(img, xy, labels, font, col, ...)
    
    drawfont(family=switch(.Platform$OS.type, windows="Arial", "helvetica"), 
             style="n", size=14, weight=200, antialias=TRUE)
}}

\section{ Arguments }{ \describe{

  \item{\code{img}}{ An object of \code{\link{Image-class}}. With indexed images,
    please ensure that the color range is in \code{[0,1]}.}

  \item{\code{xy}}{ (x,y) coordinates of labels. For single frames a matrix with 
    the first column being \code{x} and second \code{y} or a corresponding numeric 
    vector as it would be used to construct such a matrix. For multiple frames,
    a list of corresponding matrices/vectors. }

  \item{\code{labels}}{ A character vector of labels to be output. For multiple frames
    a list of such characters. }

  \item{\code{font}}{ An S3 object of class \code{DrawFont} as returned by 
    \code{drawfont}. If omitted, the \code{drawfont} functions is called internally
    to obtain the default values. }

  \item{\code{col}}{A character vector of font colors. One per frame, recycled
    between frames if required. }
  
  \item{\code{family}}{A character value for the font family to use. On Linux/UNIX
    machines one can try to use \code{helvetica}, \code{times}, \code{courier} 
    and \code{symbol}. On Windows machines, one can specify installed TrueType
    fonts, like \code{Arial}.}
  
  \item{\code{style}}{A character value for the font style to use. Can be
    specified by providing the first letter only. Supported are: \code{normal} 
    (default), \code{italic}, \code{oblique}. }
  
  \item{\code{size}}{A numeric value for the font size.}
  \item{\code{weight}}{A numeric value for the font weight (bold font). Supported
    values between 100 and 900.}
  \item{\code{antialias}}{A logical value for whether the font should be 
    anti-aliased. }
}}

\section{ Value }{
  An object of \code{\link{Image-class}} even if the supplied \code{img} was of 
  any derived class. If supplied was an \code{\link{IndexedImage-class}}, ensure 
  that it was normalized to the range $[0,1]$ before calling \code{drawtext}:
  the function is for annotation purposes only and using it with \code{IndexedImage}'s
  is likely to destroy indexing information!
}

\seealso{
  \code{
    \link{Image-class}
  }
}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2007
}

\examples{
## Dear Mac users. This example is currently excluded from MacOS builds as it
## fails in ImageMagick string assertion. Although I tried to find the reason for
## the problem, I do not have a test environment (MacOS). Please test it and
## report what goes wrong here and at which stage. Thank you. Oleg

if ( length(grep("apple", Sys.getenv("R_PLATFORM"))) == 0 ) {
  ## load images
  f <- paste( system.file(package="EBImage"), "images/Gene1_G.tif", sep="/" )
  ii = normalize(read.image(f), separate=TRUE)
  ## segment
  mask = thresh(ii, 25, 25, 0.02)
  mk3 = morphKern(3)
  mk5 = morphKern(5)
  mask = dilate(erode(closing(mask, mk5), mk3), mk5)
  ## index objects in images and remove bad ones
  io = watershed( distmap(mask), 1.5, 1)
  ft = hull.features(io)
  mf = moments(io, ii)   ## need these for intensity and size
  for ( i in seq_along(ft) ) ft[[i]] = cbind(ft[[i]], mf[[i]])
  io = rmObjects(io, lapply(ft, function(x) 
         which(x[,"h.s"] < 150 | x[,"int"] < 50 | 0.3 * x[,"h.p"] < x[,"h.edge"] )
       ))
  ## get centres of objects (list, for ii is a stack of 4)
  xy <- lapply(ft, function(x) x[,1:2])
  # create labels for objects (list, for ii is a stack of 4)
  labels <- lapply(xy, function(x) as.character(1:nrow(x)))
  ## set font properties: semi bold
  f <- drawfont()
  f$weight=600
  ## draw annotations, recycle 2 colours between 4 images
  annot <- drawtext(channel(ii,"rgb"), xy, labels, font=f, col=c("#F0B769","#ACEE3F"))
  if (interactive()) display(annot)
}

}

\keyword{file}

