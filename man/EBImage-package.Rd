\name{EBImage-package}

\alias{EBImage-package}
\alias{EBImage}

\docType{package}

\title{Package overview}

\description{
  \code{EBImage} is the image processing and analysis package for R. Its primary 
  goal is to enable automated analysis of large sets of images such as those
  obtained in high throughput automated microscopy.

  The package uses \code{ImageMagick} library for image I/O operations and
  for many image processing routines. Algorithms for image analysis are
  implemented natively including algorithms for feature extraction.

  Image data are stored in objects of the class \code{\linkS4class{Image}}, which
  is derived as an S4 class from \code{\link{array}} and inherits all of its 
  properties and methods.

  The project page is \url{http://www.ebi.ac.uk/~osklyar/EBImage}
}

\section{Package content}{

  Classes
  
  \itemize{
    \item \link[=Image-class]{Class 'Image', its accessor method}
    \item \link[=IndexedImage-class]{Class 'IndexedImage'}
    \item \link[=Arith]{Common generic methods for class 'Image'}
    \item \link[=Image]{'Image' object creation, copying and assertion}
    \item \link[=readImage]{Image read/write operations}
  }
  
  Image processing
  
  \itemize{
    \item \link[=cgamma]{Enhancing images and colors}
    \item \link[=negate]{Image color manipulation}
    \item \link[=tile]{Generate a tiled image from a stack}
    \item \link[=cthresh]{Image and color channel thresholding}
    \item \link[=rotate]{Image transformation, rotation, resize etc.}
    \item \link[=denoise]{Noise removal, blurring and smoothing of images}
  }
  
  Image analysis
  
  \itemize{
    \item \link[=distmap]{Distance map transform of binary images}
    \item \link[=dilate]{Morphological trasnsformations of binary images}
    \item \link[=edge]{Segmentation and edge detection}
    \item \link[=propagate]{Voronoi-based segmentation on image manifolds}
    \item \link[=watershed]{Watershed transformation and watershed-based object
      detection}
  }

  Feature extraction
  
  \itemize{
    \item \link[=haralickMatrix]{Extraction of Haralick texture features
      and co-occurance matrices (GLCM)}
    \item \link[=edgeProfile]{Extraction of edge profiles and edge features}
    \item \link[=hullFeatures]{Extraction of hull features}
    \item \link[=zernikeMoments]{Extraction of Zernike moments}
    \item \link[=moments]{Extraction of image moments and moment invariants}
    \item \link[=getFeatures]{Combined feature extraction for objects in
      indexed images}
    \item \link[=rmObjects]{Object removal in indexed images}
    \item \link[=paintObjects]{Marking detected objects in indexed images}
    \item \link[=stackObjects]{Generate a stack of images for detected objects}
    \item \link[=matchObjects]{Matching objects in two indexed images}
  }  

  Tools
  
  \itemize{
    \item \link[=channel]{Color and image color mode conversions}
    \item \link[=drawtext]{Drawing primitives on images, annotation}
    \item \link[=display]{Interactive image display}
  }
  
}

\section{Authors}{

  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, Copyright 2005-2007
  
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
  under the terms of the

  GNU Lesser General Public License

  as published by the Free Software Foundation; either version 2.1 of the 
  License, or (at your option) any later version. This library is distributed
  in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even 
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  }

  See the GNU Lesser General Public License for more details. For LGPL license 
  wording see \url{http://www.gnu.org/licenses/lgpl.html}
}

\keyword{package}
