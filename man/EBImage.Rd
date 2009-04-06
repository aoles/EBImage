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
    \item \code{Image}, the class constructor
    \item \code{as.Image}, \code{is.Image}
    \item \code{colorMode}, \code{imageData}
    \item \code{getNumberOfFrames}
  }
  
  Image I/O, display
  \itemize{
    \item \code{readImage}, \code{writeImage}
    \item \code{display}, \code{animate}
    \item \code{show}, \code{print.Image}
    \item \code{image}
  }
  
  Spatial transform
  \itemize{
    \item \code{resize}, \code{resample}
    \item \code{rotate}, \code{translate}, \code{flip}, \code{flop}, \code{affinet}
  }
  
  Image analysis, segmentation
  \itemize{
    \item \code{edge}, \code{segment}
    \item \code{thresh}, \code{athresh}, \code{cthresh}
    \item \code{ocontour}
  }
  
  Image enhancement, restoration
  \itemize{
    \item \code{cgamma}, \code{contrast}, \code{enhance},
  \code{equalize},  \code{modulate}
    \item \code{noise}, \code{denoise}, \code{despeckle}
    \item \code{mediansmooth}
    \item \code{blur}, \code{gblur}, \code{sharpen}
    \item \code{normalize}, \code{normalize2}
    \item \code{umask}
  }
  
  Linear filtering
  \itemize{
    \item \code{filter2}
  }
  
  Morphological operations
  \itemize{
    \item \code{makeBrush}
    \item \code{erode}, \code{dilate}, \code{opening}, \code{closing}
    \item \code{distmap}
    \item \code{watershed}
    \item \code{propagate}
    \item \code{floodFill}, \code{fillHull}
  }
  
  Colorspace manipulation
  \itemize{  
    \item \code{rgbImage}, \code{channel}
    \item \code{channelMix}
  }
  
  Image stacking, combining, tiling
  \itemize{ 
    \item \code{stackObjects}
    \item \code{combine}
    \item \code{tile}, \code{untile}
  }
  
  Drawing on images
  \itemize{
    \item \code{drawfont}, \code{drawtext}
  }
  
  Objects manipulation
  \itemize{
    \item \code{paintObjects}
    \item \code{rmObjects}, \code{reenumerate}
  }
    
  Features extraction
  \itemize{
    \item \code{getFeatures}
    \item \code{hullFeatures}
    \item \code{edgeProfile}, \code{edgeFeatures}
    \item \code{moments}, \code{cmoments}, \code{smoments},  \code{rmoments}
    \item \code{haralickFeatures}, \code{haralickMatrix}
    \item \code{zernikeMoments}
  }

  Deprecated
  \itemize{
    \item \code{read.image}, \code{write.image}
    \item \code{choose.image}
    \item \code{hull.features}, \code{edge.features},
  \code{edge.profile},  \code{haralick.matrix},
  \code{haralick.features},  \code{zernike.moments}
    \item \code{stopIfNotImage}
    \item \code{frameDist}
   \item \code{header},  \code{assert}
    \item \code{matchObjects}
    \item \code{morphKern}, \code{mkbox}, \code{mkball}
    \item \code{chooseImage}
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

  \preformatted{
  This library is free software; you can redistribute it and/or modify it 
  under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the 
  License, or (at your option) any later version. This library is distributed
  in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even 
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  }

  See the GNU Lesser General Public License for more details. For LGPL license 
  wording see \url{http://www.gnu.org/licenses/lgpl.html}
}

\keyword{package}
