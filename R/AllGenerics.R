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

## image accessor methods
setGeneric ("colorMode",      function (x, ...)        standardGeneric("colorMode") )
setGeneric ("colorMode<-",    function (x, ..., value) standardGeneric("colorMode<-") )
setGeneric ("fileName",       function (x, ...)        standardGeneric("fileName") )
setGeneric ("fileName<-",     function (x, ..., value) standardGeneric("fileName<-") )
setGeneric ("compression",    function (x, ...)        standardGeneric("compression") )
setGeneric ("compression<-",  function (x, ..., value) standardGeneric("compression<-") )
setGeneric ("resolution",     function (x, ...)        standardGeneric("resolution") )
setGeneric ("resolution<-",   function (x, ..., value) standardGeneric("resolution<-") )
setGeneric ("imageData",      function (x, ...)        standardGeneric("imageData") )
setGeneric ("imageData<-",    function (x, ..., value) standardGeneric("imageData<-") )
setGeneric ("features",       function (x, ...)        standardGeneric("features") )

setGeneric ("channel",        function (x, mode, ...)  standardGeneric("channel") )

## image IO, assertion, display
setGeneric ("copy",           function (x, ...)        standardGeneric("copy") )
setGeneric (".isCorrectType", function (x)             standardGeneric(".isCorrectType") )
setGeneric (".correctType",   function (x)             standardGeneric(".correctType") )
setGeneric ("display",        function (x, ...)        standardGeneric("display") )
setGeneric ("animate",        function (x, ...)        standardGeneric("animate") )
setGeneric ("header",         function (x, ...)        standardGeneric("header") )
setGeneric ("write.image",    function (x, files, ...) standardGeneric("write.image") )
setGeneric ("assert",         function (x, y, ...)     standardGeneric("assert") )
setGeneric ("combine",        function (x, y, ...)     standardGeneric("combine") )
setGeneric ("tile",           function (x, ...)        standardGeneric("tile") )
setGeneric ("image",          function (x, ...)        standardGeneric("image") )
setGeneric ("hist",           function (x, ...)        standardGeneric("hist") )
setGeneric ("as.Image",       function (x, ...)        standardGeneric("as.Image") )

## image filters
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
setGeneric ("thresh",         function (x, ...)        standardGeneric("thresh") )
setGeneric ("athresh",        function (x, ...)        standardGeneric("athresh") )
setGeneric ("cthresh",        function (x, ...)        standardGeneric("cthresh") )
setGeneric ("affinet",        function (x, ...)        standardGeneric("affinet") )
setGeneric ("modulate",       function (x, ...)        standardGeneric("modulate") )
setGeneric ("negate",         function (x, ...)        standardGeneric("negate") )
setGeneric ("normalize",      function (x, ...)        standardGeneric("normalize") )
setGeneric ("normalize2",     function (x, ...)        standardGeneric("normalize2") )
setGeneric ("fill",           function (x, ...)        standardGeneric("fill") )
setGeneric ("flip",           function (x, ...)        standardGeneric("flip") )
setGeneric ("flop",           function (x, ...)        standardGeneric("flop") )

## morphology
setGeneric ("distmap",        function (x, ...)        standardGeneric("distmap") )
setGeneric ("watershed",      function (x, ...)        standardGeneric("watershed") )
setGeneric ("propagate",      function (x, seeds, ...) standardGeneric("propagate") )
setGeneric ("erode",          function (x, ...)        standardGeneric("erode") )
setGeneric ("dilate",         function (x, ...)        standardGeneric("dilate") )
setGeneric ("opening",        function (x, ...)        standardGeneric("opening") )
setGeneric ("closing",        function (x, ...)        standardGeneric("closing") )

## object manipulation
setGeneric ("paintObjects",   function (x, tgt, ...)   standardGeneric("paintObjects") )
setGeneric ("matchObjects",   function (x, ref, ...)   standardGeneric("matchObjects") )
setGeneric ("stackObjects",   function (x, ref, index, ...) standardGeneric("stackObjects") )
setGeneric ("rmObjects",      function (x, index, ...) standardGeneric("rmObjects") )

## feature extraction
setGeneric ("getFeatures",    function (x, ...)        standardGeneric("getFeatures") )
setGeneric ("hull.features",  function (x, ...)        standardGeneric("hull.features") )
setGeneric ("edge.profile",   function (x, ref, ...)   standardGeneric("edge.profile") )
setGeneric ("edge.features",  function (x, ...)        standardGeneric("edge.features") )
setGeneric ("cmoments",       function (x, ref, ...)   standardGeneric("cmoments") )
setGeneric ("smoments",       function (x, ref, ...)   standardGeneric("smoments") )
setGeneric ("rmoments",       function (x, ref, ...)   standardGeneric("rmoments") )
setGeneric ("moments",        function (x, ref, ...)   standardGeneric("moments") )
setGeneric ("zernike.moments",function (x, ref, ...)   standardGeneric("zernike.moments") )
setGeneric ("haralick.matrix",function (x, ref, ...)   standardGeneric("haralick.matrix") )
setGeneric ("haralick.features",function (x, ref,...)  standardGeneric("haralick.features") )

