\name{EBImage}

\alias{EBImage}

\docType{package}

\title{Package overview}

\description{
  \code{EBImage} is an image processing and analysis package for R. Its primary
  goal is to enable automated analysis of large sets of images such as those
  obtained in high throughput automated microscopy.

  The package uses the \code{ImageMagick} library for image I/O operations and
  some image processing methods. The \code{GTK} library is used for displaying images
  using \code{display}.

  \code{EBImage} relies on the \code{Image} object to store and process images
  but also works on multi-dimensional arrays.
}

\section{Package content}{

  Image methods
  \itemize{
    \item \code{Image}
    \item \code{as.Image}, \code{is.Image}
    \item \code{colorMode}, \code{imageData}
    \item \code{getFrame}, \code{getNumberOfFrames}
  }
  
  Image I/O, display
  \itemize{
    \item \code{readImage}, \code{writeImage}
    \item \code{display}, \code{animate}
    \item \code{image}
  }
  
  Spatial transform
  \itemize{
    \item \code{resize}, \code{flip}, \code{flop}
    \item \code{rotate}, \code{translate}
  }
  
  Image segmentation, objects manipulation
  \itemize{
    \item \code{thresh}, \code{bwlabel}
    \item \code{watershed}, \code{propagate}
    \item \code{ocontour}
    \item \code{paintObjects}, \code{rmObjects}, \code{reenumerate}
  }
  
  Image enhancement, filtering
  \itemize{
    \item \code{normalize}
    \item \code{filter2}, \code{blur}, \code{gblur}
    \item \code{equalize}
  }
  
  Morphological operations
  \itemize{
    \item \code{makeBrush}
    \item \code{erode}, \code{dilate}, \code{opening}, \code{closing}
    \item \code{distmap}
    \item \code{floodFill}, \code{fillHull}
  }
  
  Colorspace manipulation
  \itemize{  
    \item \code{rgbImage}, \code{channel}
  }
  
  Image stacking, combining, tiling
  \itemize{ 
    \item \code{stackObjects}
    \item \code{combine}
    \item \code{tile}, \code{untile}
  }
  
  Drawing on images
  \itemize{
    \item \code{drawfont}, \code{drawtext}, \code{drawCircle}
  }
    
  Features extraction
  \itemize{
    \item \code{getFeatures}
    \item \code{hullFeatures}
    \item \code{edgeProfile}, \code{edgeFeatures}
    \item \code{moments}, \code{cmoments}, \code{smoments}, \code{rmoments}
    \item \code{haralickFeatures}, \code{haralickMatrix}
    \item \code{zernikeMoments}
  }
}

\section{Authors}{

  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, Copyright 2005-2007

  Gregoire Pau, \email{gpau@ebi.ac.uk}
  
  Wolfgang Huber, \email{huber@ebi.ac.uk}
  
  Mike Smith, \email{msmith@ebi.ac.uk}
  
  \preformatted{
  European Bioinformatics Institute
  European Molecular Biology Laboratory
  Wellcome Trust Genome Campus
  Hinxton 
  Cambridge CB10 1SD
  UK
  }

  The code of \code{\link{propagate}} is based on the \code{CellProfiler} 
  with permission granted to distribute this particular part under LGPL, the 
  corresponding copyright (Jones, Carpenter) applies.

  The source code is released under \code{LGPL} (see the \code{LICENSE} 
  file in the package root for the complete license wording). \code{ImageMagick} 
  and \code{GTK} used from the package are distributed separately by the 
  respective copyright holders.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the
  License, or (at your option) any later version. This library is distributed
  in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 
  See the GNU Lesser General Public License for more details. For LGPL license 
  wording see \url{http://www.gnu.org/licenses/lgpl.html}
}

\examples{
  example(readImage)
  example(display)
  example(rotate)
  example(propagate)
}

\keyword{package}
