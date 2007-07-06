\name{EBImage-package}

\alias{EBImage-package}
\alias{EBImage}

\docType{package}

\title{Package overview and changelog}

\description{
  \code{EBImage} is the image processing and analysis package for R. It is being
  created primarily to enable automated analysis of large sets of
  cytomicroscopy images, e.g. results of automated microscopy screens, however
  other image can be analysed in a similar manner.

  The package uses \code{ImageMagick} library for all of its I/O operations and
  for many image processing routines; however, it does not inteface all
  \code{ImageMagick} API functions. Algorithms for image analysis are
  implemented natively. These include distance map and watershed transforms, 
  adaptive thresholding, morphological transforms and image moments etc.

  Image data are stored in objects of \code{\link{Image-class}}. This S4 class
  is derived from \code{\link{array}} and inherits all of its properties and
  methods like mathematical operations on arrays, subsetting, histograms
  etc. Image processing and analysis routines work with objects of
  \code{\link{Image-class}}.

  The package provides many image processing filters that can
  be subdivided into the following categories: colour and image enhancement
  (e.g. \code{\link{enhance}, \link{contrast}}), sharpening and noise reduction
  (e.g. \code{\link{denoise}, \link{despeckle}}), image transformation (e.g.
  \code{\link{rotate}, \link{resize}}), image segmentation (e.g. \code{\link{edge}, 
  \link{segment}, \link{thresh}}), image morphology (e.g. \code{\link{dilate}, 
  \link{opening}, \link{distmap}}), object detection (e.g. \code{\link{watershed},
  \link{propagate}}) abd feature extractuion (e.g. \code{\link{haralick.features},
  \link{edge.features}} etc).

  The project page is: \url{http://www.ebi.ac.uk/~osklyar/EBImage}
}

\section{Changelog}{
  \preformatted{
* 2.1.7, 2007-07-06: multiple feature extraction algorithms (hull, Haralick, 
    edge features); new optimized 'stackObjects' (with rotation) and fast 'tile'

* 2.1.2, 2007-05-15: new development version with image\link{moments}
    implemented.

* 2.0.0, 2007-04-26: Bioconductor 2.0 release with EBImage 2.0 release!

* 1.9.26, 2007-02-08: Voronoi-based image segmentation
  - added propagate(): Voronoi-based segmentation on image manifolds
    with modified metric! Great thing for cell image segmentation

* 1.9.25, 2007-02-02: (Critical) bug fixes
  - corrected memory leak in functions using ImageMagick filters:
    copied images were not deleted! Everyone should update!
  - disabled setting GTK locale. This solves problem with GTK reseting
    R locale and turning "." unusable on locales that use for example
    "," instead of a "." (like German).
  - added DestroyExceptionInfo() everywhere to ensure everything
    is cleaned up in the ImageMagick related code
  - welcome message is now printed only if getOption("verbose") is TRUE

* 1.9.23, 2007-01-27: [,Image,numeric,missing now works as it should
  - long going problem with nargs() in this method is now solved

* 1.9.20, 2007-01-07: animate
  - added (restored) animate()

* 1.9.18, 2007-01-16: accircularity calculations in getObjects()
  - added acircularity and other related object descriptors

* 1.9.01, 2006-12-29: New EBImage development release
  - new development version of EBImage, fully recoded. Now runs on
    Windows as well. The new code is much cleaner and less buggy,

* Earlier modifications:
  - Jul 2006: morphological operators erode, dilate, opening, closing
  - Jun 2006: watershed-based object detection
  - Jun 2006: first full vignette
  - Apr 2006: first major restructuring, removing Image2D and Image3D
  - Apr 2006: EBImage first time in the Bioconductor release!
  - Mar 2006: ./configure script with autoconf
  }
}

\author{
  Copyright (c) 2005-2007 Oleg Sklyar <\email{osklyar@ebi.ac.uk}>

  Parts of the code by Wolfgang Huber <\email{huber@ebi.ac.uk}> and
  Mike Smith <\email{msmith@ebi.ac.uk}>. The code of \code{\link{propagate}}
  is based on \code{CellProfiler} with permission granted to distribute this
  particular part under LGPL, the corresponding copyright applies.

  \preformatted{
  European Bioinformatics Institute
  European Molecular Biology Laboratory
  Wellcome Trust Genome Campus
  Hinxton, Cambridgeshire
  CB10 1SD
  England
  }

  The full source code is released under \code{LGPL} (see the \code{LICENSE} 
  file in the package root for the full license wording). \code{ImageMagick} and
  \code{GTK} used from the package are distributed separately by the respective
  copyright holders.

  \preformatted{
  This library is free software; you can redistribute
  it and/or modify it under the terms of the

  GNU Lesser General Public License

  as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any
  later version. This library is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE.}

  See the GNU Lesser General Public License for more details.

  For LGPL license wording see \url{http://www.gnu.org/licenses/lgpl.html}
}
\keyword{package}
