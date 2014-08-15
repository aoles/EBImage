\name{EBImage}

\alias{EBImage}

\docType{package}

\title{Package overview}

\description{
  \code{EBImage} is an image processing and analysis package for R. Its primary
  goal is to enable automated analysis of large sets of images such as those
  obtained in high throughput automated microscopy.

  \code{EBImage} relies on the \code{Image} object to store and process images
  but also works on multi-dimensional arrays.
}

\section{Package content}{

  Image methods
  \itemize{
    \item \code{Image}
    \item \code{as.Image}, \code{is.Image}, \code{as.raster}
    \item \code{colorMode}, \code{imageData}
    \item \code{getFrame}, \code{getNumberOfFrames}
  }

  Image I/O, display
  \itemize{
    \item \code{readImage}, \code{writeImage}
    \item \code{display}
    \item \code{image}
  }

  Spatial transforms
  \itemize{
    \item \code{resize}, \code{flip}, \code{flop}, \code{transpose}
    \item \code{rotate}, \code{translate}, \code{affine}
  }

  Image segmentation, objects manipulation
  \itemize{
    \item \code{thresh}, \code{bwlabel}, \code{otsu}
    \item \code{watershed}, \code{propagate}
    \item \code{ocontour}
    \item \code{paintObjects}, \code{rmObjects}, \code{reenumerate}
  }

  Image enhancement, filtering
  \itemize{
    \item \code{normalize}
    \item \code{filter2}, \code{gblur}, \code{medianFilter}
  }

  Morphological operations
  \itemize{
    \item \code{makeBrush}
    \item \code{erode}, \code{dilate}, \code{opening}, \code{closing}
    \item \code{erodeGreyScale}, \code{dilateGreyScale}, \code{openingGreyScale}, \code{closingGreyScale}
    \item \code{whiteTopHatGreyScale}, \code{blackTopHatGreyScale}, \code{selfcomplementaryTopHatGreyScale}
    \item \code{distmap}
    \item \code{floodFill}, \code{fillHull}
  }

  Color space manipulation
  \itemize{  
    \item \code{rgbImage}, \code{channel}, \code{toRGB}
  }

  Image stacking, combining, tiling
  \itemize{ 
    \item \code{stackObjects}
    \item \code{combine}
    \item \code{tile}, \code{untile}
  }

  Drawing on images
  \itemize{
    \item \code{drawCircle}
  }

  Features extraction
  \itemize{
    \item \code{computeFeatures}
    \item \code{computeFeatures.basic}, \code{computeFeatures.moment}, \code{computeFeatures.shape}, \code{computeFeatures.haralick}
    \item \code{standardExpandRef}
  }

  Defunct
  \itemize{
    \item \code{blur}, \code{equalize}
    \item \code{drawtext}, \code{drawfont}
    \item \code{getFeatures}, \code{hullFeatures}, \code{zernikeMoments}
    \item \code{edgeProfile}, \code{edgeFeatures}, 
    \item \code{haralickFeatures}, \code{haralickMatrix}
    \item \code{moments}, \code{cmoments}, \code{smoments}, \code{rmoments}
  }
}

\section{Authors}{

  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, Copyright 2005-2007

  Gregoire Pau, \email{gpau@ebi.ac.uk}

  Wolfgang Huber, \email{huber@ebi.ac.uk}

  Andrzej Oles, \email{andrzej.oles@embl.de}

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
  file in the package root for the complete license wording).

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
