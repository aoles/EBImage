# Declarations of all generic methods used in the package

# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## Image and ObjectImage property accessor methods
setGeneric ("colorMode",      function (x, ...)        standardGeneric("colorMode") )
setGeneric ("colorMode<-",    function (x, ..., value) standardGeneric("colorMode<-") )
setGeneric ("fileName",       function (x, ...)        standardGeneric("fileName") )
setGeneric ("fileName<-",     function (x, ..., value) standardGeneric("fileName<-") )
setGeneric ("compression",    function (x, ...)        standardGeneric("compression") )
setGeneric ("compression<-",  function (x, ..., value) standardGeneric("compression<-") )
setGeneric ("resolution",     function (x, ...)        standardGeneric("resolution") )
setGeneric ("resolution<-",   function (x, ..., value) standardGeneric("resolution<-") )
setGeneric ("sampleFilter",   function (x, ...)        standardGeneric("sampleFilter") )
setGeneric ("sampleFilter<-", function (x, ..., value) standardGeneric("sampleFilter<-") )
setGeneric ("imageData",      function (x, ...)        standardGeneric("imageData") )
setGeneric ("imageData<-",    function (x, ..., value) standardGeneric("imageData<-") )
## ObjectImage property accessor methods
setGeneric ("features",       function (x, ...)        standardGeneric("features") )

## in this package defined for Image and ObjectImage only (class.Image.R)
setGeneric ("copy",           function (x, ...)        standardGeneric("copy") )
setGeneric (".isCorrectType", function (x)             standardGeneric(".isCorrectType") )
setGeneric (".correctType",   function (x)             standardGeneric(".correctType") )
setGeneric ("display",        function (x, ...)        standardGeneric("display") )
setGeneric ("header",         function (x, ...)        standardGeneric("header") )
setGeneric ("write.image",    function (x, files, quality, ...) standardGeneric("write.image") )
setGeneric ("assert",         function (x, y, ...)     standardGeneric("assert") )
setGeneric ("as.array",       function (x)             standardGeneric("as.array") )
setGeneric ("as.matrix",      function (x, ...)        standardGeneric("as.matrix") )
setGeneric ("combine",        function (x, y, ...)     standardGeneric("combine") )

## image filters (imageFilters.R)
setGeneric ("blur",           function (x, ...)        standardGeneric("blur") )
setGeneric ("gblur",          function (x, ...)        standardGeneric("gblur") )
setGeneric ("contrast",       function (x, ...)        standardGeneric("contrast") )
setGeneric ("denoise",        function (x, ...)        standardGeneric("denoise") )
setGeneric ("despeckle",      function (x, ...)        standardGeneric("despeckle") )
setGeneric ("edge",           function (x, ...)        standardGeneric("edge") )
setGeneric ("enhance",        function (x, ...)        standardGeneric("enhance") )
setGeneric ("equalize",       function (x, ...)        standardGeneric("equalize") )
setGeneric ("cgamma",         function (x, ...)        standardGeneric("cgamma") )
setGeneric ("mediansmooth",   function (x, ...)        standardGeneric("mediansmooth") )
setGeneric ("noise",          function (x, ...)        standardGeneric("noise") )
setGeneric ("resize",         function (x, ...)        standardGeneric("resize") )
setGeneric ("rotate",         function (x, ...)        standardGeneric("rotate") )
setGeneric ("resample",       function (x, ...)        standardGeneric("resample") )
setGeneric ("segment",        function (x, ...)        standardGeneric("segment") )
setGeneric ("sharpen",        function (x, ...)        standardGeneric("sharpen") )
setGeneric ("umask",          function (x, ...)        standardGeneric("umask") )
## adaptive threshold using running square frame
setGeneric ("thresh",         function (x, ...)        standardGeneric("thresh") )
## adaptive threshold using magick
setGeneric ("athresh",        function (x, ...)        standardGeneric("athresh") )
## channel threshold
setGeneric ("cthresh",        function (x, ...)        standardGeneric("cthresh") )
## affine transform image
setGeneric ("affinet",        function (x, ...)        standardGeneric("affinet") )
## bright, sat and hue in percent
setGeneric ("modulate",       function (x, ...)        standardGeneric("modulate") )
setGeneric ("negate",         function (x, ...)        standardGeneric("negate") )
setGeneric ("normalize",      function (x, ...)        standardGeneric("normalize") )
## works for RGB as well, uses magick
setGeneric ("normalize2",     function (x, ...)        standardGeneric("normalize2") )
setGeneric ("fill",           function (x, ...)        standardGeneric("fill") )
setGeneric ("flip",           function (x, ...)        standardGeneric("flip") )
setGeneric ("flop",           function (x, ...)        standardGeneric("flop") )
setGeneric ("erode",          function (x, ...)        standardGeneric("erode") )
setGeneric ("dilate",         function (x, ...)        standardGeneric("dilate") )
setGeneric ("opening",        function (x, ...)        standardGeneric("opening") )
setGeneric ("closing",        function (x, ...)        standardGeneric("closing") )

## object detection and related
setGeneric ("distmap",        function (x, ...)        standardGeneric("distmap") )
setGeneric ("watershed",      function (x, ...)        standardGeneric("watershed") )
setGeneric ("getObjects",     function (x, ref, ...)   standardGeneric("getObjects") )
setGeneric ("paintObjects",   function (x, tgt, ...)   standardGeneric("paintObjects") )

## in this package defined for Image (class.Image.R), ANY (in tools.R)
setGeneric ("channel",        function (x, mode, ...)  standardGeneric("channel") )

