# Declarations of all generic methods used in the package

# Copyright (c) 2005-2007 Oleg Sklyar

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

## image class, copy, assertion, accessors
setGeneric ("header",         function (x)             standardGeneric("header") )
setGeneric ("assert",         function (x, ...)        standardGeneric("assert") )
setGeneric ("as.Image",       function (x)             standardGeneric("as.Image") )
setGeneric ("colorMode",      function (x)             standardGeneric("colorMode") )
setGeneric ("colorMode<-",    function (x, value)      standardGeneric("colorMode<-") )
setGeneric ("imageData",      function (x)             standardGeneric("imageData") )
setGeneric ("imageData<-",    function (x, value)      standardGeneric("imageData<-") )
setGeneric ("getNumberOfFrames",function (x, ...)      standardGeneric("getNumberOfFrames") )

## image IO, display
setGeneric ("display",        function (x, ...)        standardGeneric("display") )
setGeneric ("animate",        function (x, ...)        standardGeneric("animate") )
setGeneric ("writeImage",     function (x, ...)        standardGeneric("writeImage") )
setGeneric ("image",          function (x, ...)        standardGeneric("image") )

## spatial transform
setGeneric ("resize",         function (x, ...)        standardGeneric("resize") )
setGeneric ("resample",       function (x, ...)        standardGeneric("resample") )
setGeneric ("rotate",         function (x, ...)        standardGeneric("rotate") )
setGeneric ("affinet",        function (x, ...)        standardGeneric("affinet") )
setGeneric ("flip",           function (x, ...)        standardGeneric("flip") )
setGeneric ("flop",           function (x, ...)        standardGeneric("flop") )
setGeneric ("translate",      function (x, ...)        standardGeneric("translate") )

## image analysis, segmentation
setGeneric ("edge",           function (x, ...)        standardGeneric("edge") )
setGeneric ("segment",        function (x, ...)        standardGeneric("segment") )
setGeneric ("thresh",         function (x, ...)        standardGeneric("thresh") )
setGeneric ("athresh",        function (x, ...)        standardGeneric("athresh") )
setGeneric ("cthresh",        function (x, ...)        standardGeneric("cthresh") )

## statistics
setGeneric ("hist",           function (x, ...)        standardGeneric("hist") )

## image enhancement, restoration
setGeneric ("cgamma",         function (x, ...)        standardGeneric("cgamma") )
setGeneric ("contrast",       function (x, ...)        standardGeneric("contrast") )
setGeneric ("enhance",        function (x, ...)        standardGeneric("enhance") )
setGeneric ("equalize",       function (x, ...)        standardGeneric("equalize") )
setGeneric ("modulate",       function (x, ...)        standardGeneric("modulate") )
setGeneric ("denoise",        function (x, ...)        standardGeneric("denoise") )
setGeneric ("mediansmooth",   function (x, ...)        standardGeneric("mediansmooth") )
setGeneric ("despeckle",      function (x, ...)        standardGeneric("despeckle") )
setGeneric ("sharpen",        function (x, ...)        standardGeneric("sharpen") )
setGeneric ("umask",          function (x, ...)        standardGeneric("umask") )
setGeneric ("blur",           function (x, ...)        standardGeneric("blur") )
setGeneric ("gblur",          function (x, ...)        standardGeneric("gblur") )
setGeneric ("noise",          function (x, ...)        standardGeneric("noise") )
setGeneric ("normalize",      function (x, ...)        standardGeneric("normalize") )
setGeneric ("normalize2",     function (x, ...)        standardGeneric("normalize2") )
setGeneric ("negate",         function (x, ...)        standardGeneric("negate") )

## linear filtering
setGeneric ("filter2",        function (x, filter, ...) standardGeneric("filter2"))

## morphological operations
setGeneric ("erode",          function (x, ...)        standardGeneric("erode") )
setGeneric ("dilate",         function (x, ...)        standardGeneric("dilate") )
setGeneric ("opening",        function (x, ...)        standardGeneric("opening") )
setGeneric ("closing",        function (x, ...)        standardGeneric("closing") )
setGeneric ("distmap",        function (x, ...)        standardGeneric("distmap") )
setGeneric ("watershed",      function (x, ...)        standardGeneric("watershed") )
setGeneric ("propagate",      function (x, seeds, ...) standardGeneric("propagate") )
setGeneric ("floodFill",      function (x, pt, ...)    standardGeneric("floodFill") )
setGeneric ("fillHull",       function (x, ...)        standardGeneric("fillHull") )

## colorspace
setGeneric ("channel",        function (x, mode, ...)  standardGeneric("channel") )

## image stacking, tiling
setGeneric ("combine",        function (x, ...)     standardGeneric("combine") )
setGeneric ("tile",           function (x, ...)        standardGeneric("tile") )
setGeneric ("untile",         function (x, nim, ...)   standardGeneric("untile") )

## drawables
setGeneric ("drawtext",       function (img, xy, labels, ...) standardGeneric("drawtext"))

## object manipulation
setGeneric ("paintObjects",   function (x, tgt, ...)   standardGeneric("paintObjects") )
setGeneric ("matchObjects",   function (x, ref, ...)   standardGeneric("matchObjects") )
setGeneric ("stackObjects",   function (x, ref, index, ...) standardGeneric("stackObjects") )
setGeneric ("rmObjects",      function (x, index)      standardGeneric("rmObjects") )
setGeneric ("reenumerate",    function (x)             standardGeneric("reenumerate"))

## features extraction
setGeneric ("getFeatures",    function (x, ...)        standardGeneric("getFeatures") )
setGeneric ("hullFeatures",   function (x, ...)        standardGeneric("hullFeatures") )
setGeneric ("edgeProfile",    function (x, ...)        standardGeneric("edgeProfile") )
setGeneric ("edgeFeatures",   function (x, ...)        standardGeneric("edgeFeatures") )
setGeneric ("cmoments",       function (x, ref, ...)   standardGeneric("cmoments") )
setGeneric ("smoments",       function (x, ref, ...)   standardGeneric("smoments") )
setGeneric ("rmoments",       function (x, ref, ...)   standardGeneric("rmoments") )
setGeneric ("moments",        function (x, ref, ...)   standardGeneric("moments") )
setGeneric ("zernikeMoments", function (x, ref, ...)   standardGeneric("zernikeMoments") )
setGeneric ("haralickMatrix", function (x, ref, ...)   standardGeneric("haralickMatrix") )
setGeneric ("haralickFeatures",function (x, ref,...)   standardGeneric("haralickFeatures") )

# deprecated
setGeneric ("write.image",    function (x, ...)        standardGeneric("write.image") )
setGeneric ("hull.features",  function (x, ...)        standardGeneric("hull.features") )
setGeneric ("edge.profile",   function (x, ...)        standardGeneric("edge.profile") )
setGeneric ("edge.features",  function (x, ...)        standardGeneric("edge.features") )
setGeneric ("haralick.matrix",function (x, ref, ...)   standardGeneric("haralick.matrix") )
setGeneric ("haralick.features",function (x, ref,...)  standardGeneric("haralick.features") )
setGeneric ("zernike.moments",function (x, ref, ...)   standardGeneric("zernike.moments") )
setGeneric ("frameDist",      function(x, y, ...)      standardGeneric("frameDist"))
