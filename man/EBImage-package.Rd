\name{EBImage-package}

\alias{EBImage-package}
\alias{EBImage}

%\alias{Image-class}
%\alias{Grayscale}
%\alias{TrueColor}

%\alias{features}
%\alias{fileName}
%\alias{fileName<-}
%\alias{imageData}
%\alias{imageData<-}
%\alias{resolution}
%\alias{resolution<-}
%\alias{sampleFilter}
%\alias{sampleFilter<-}
%\alias{colorMode}
%\alias{colorMode<-}
%\alias{compression}
%\alias{compression<-}

%\alias{hist}
%\alias{image}

%\alias{display}

%\alias{Image}
%\alias{combine}
%\alias{copy}
%\alias{header}
%\alias{assert}
%\alias{is.Image}
%\alias{stopIfNotImage}


%\alias{choose.image}
%\alias{read.image}
%\alias{write.image}

%\alias{channel}

\alias{cgamma}
\alias{contrast}
\alias{enhance}
\alias{equalize}
\alias{modulate}

\alias{flip}
\alias{flop}
\alias{resample}
\alias{resize}
\alias{rotate}
\alias{affinet}

\alias{dilate}
\alias{closing}
\alias{erode}
\alias{morphKern}
\alias{opening}

\alias{negate}
\alias{normalize}
\alias{normalize2}

\alias{mediansmooth}
\alias{blur}
\alias{denoise}
\alias{despeckle}
\alias{gblur}
\alias{noise}
\alias{sharpen}
\alias{umask}

\alias{edge}
\alias{fill}
\alias{segment}

%\alias{thresh}
%\alias{athresh}
%\alias{cthresh}

%\alias{distmap}

%\alias{watershed}


\docType{package}

\title{EBImage package overview and changelog}

\description{
  \code{EBImage} is image processing and analysis package for R. It is being
  created primarily to enable automated analysis of large sets of 
  cytomicroscopy images, e.g. results of automated microscopy screens.

  The package uses \code{ImageMagick} library for all of its I/O operations and
  for many image processing routines. It does not inteface all 
  \code{ImageMagick API} functions though. Algorithms for image analysis are
  implemented natively. These include distance maps, adaptive thresholding, 
  object counting, watershed transformation etc.

  Image data are stored in objects of \code{\link{Image-class}}. The class 
  is derived from \code{\link{array}} and inherits all of its properties and
  S3 methods, like mathematical operations on arrays, subsetting, histograms 
  etc. Image processing and analysis routines require objects of 
  \code{\link{Image-class}} as input and often return objects of this class.
}

\details{

}

\section{Changelog}{
  \describe{
    \item{\code{New Year 2007:} }{New major RC (to be 2.0.0): the package 
    rewritten from scratch in pure C. It supports all OS's including Windows 
    and integrates with \code{GTK+} for display and dialogs.}
    \item{\code{July 2006:} }{Added morphological operations (\code{\link{erode}, 
      \link{dilate}, \link{opening}, \link{closing}}). }
    \item{\code{June 2006:} }{Added watershed-based object detection 
      (\code{\link{watershed}}). }
    \item{\code{June 2006:} }{Added first full vignette. }
    \item{\code{April 2006:} }{First major restructuring deprecating 
      \code{Image2D} and \code{Image3D} in favor of \code{\link{Image-class}} }
    \item{\code{April 2006:} }{EBImage first time in the Bioconductor release! }
    \item{\code{March 2006:} }{Uses autoconf to generate configure script. }
    \item{\code{June 2005:} }{Project started in its current form. }
  }
}

\author{
  Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}. Parts of 
  code by Wolfgang Huber : \email{huber@ebi.ac.uk}
    
  European Bioinformatics Institute, Wellcome Trust Genome Campus, 
  Hinxton, Cambs, CB10 1SD, England

  Released under \code{LGPL}, see the \code{LICENSE} file in the package root
  for the full license wording:

  \code{This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License 
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.}

  \code{This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.}

  \code{See the GNU Lesser General Public License for more details.
  LGPL license wording: http://www.gnu.org/licenses/lgpl.html}
}
\keyword{package}
