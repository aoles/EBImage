# -------------------------------------------------------------------------
# Generic declaratios for EBImage
 
# Copyright (c) 2006 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.          

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

# See the GNU General Public License for more details.
# GPL license wording: http://www.gnu.org/licenses/gpl.html

# -------------------------------------------------------------------------

setGeneric("write.image", function(object, files) standardGeneric("write.image"))
setGeneric("assert",      function(object, ...)   standardGeneric("assert"))
setGeneric("copy",        function(x)             standardGeneric("copy"))
setGeneric("display",     function(object, ...)   standardGeneric("display"))
setGeneric("channels",    function(object)        standardGeneric("channels"))
setGeneric("toGray",      function(object)        standardGeneric("toGray"))
setGeneric("toRGB",       function(object)        standardGeneric("toRGB"))
setGeneric("toX11char",   function(object)        standardGeneric("toX11char"))
setGeneric("add2RGB",     function(x, y)          standardGeneric("add2RGB"))
setGeneric("sub2RGB",     function(x, y)          standardGeneric("sub2RGB"))
setGeneric("scale2RGB",   function(x, mult)       standardGeneric("scale2RGB"))
setGeneric("toRed",       function(object)        standardGeneric("toRed"))
setGeneric("toGreen",     function(object)        standardGeneric("toGreen"))
setGeneric("toBlue",      function(object)        standardGeneric("toBlue"))
setGeneric("getRed",      function(object)        standardGeneric("getRed"))
setGeneric("getGreen",    function(object)        standardGeneric("getGreen"))
setGeneric("getBlue",     function(object)        standardGeneric("getBlue"))
setGeneric("normalize",   function(object, ...)   standardGeneric("normalize"))
setGeneric("as.array",    function(x)             standardGeneric("as.array"))
setGeneric("summary",     function(object, ...)   standardGeneric("summary"))
setGeneric("plot.image",  function(x, ...)        standardGeneric("plot.image"))
setGeneric("print",       function(x, ...)        standardGeneric("print"))
