# -------------------------------------------------------------------------
# Not image-specific tools for EBImage
 
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

# -------------------------------------------------------------------------

setMethod("entropy", signature(x = "ANY"),
	function(x, n=512, method="hist", ...) {
		N <- length(x)
		if (N <= 1) {
		    warning("too few observations (< 2), entropy is zero")
		    return(0)
		}
		if (sd(x) == 0) {
		    warning("no variation in data, entropy is zero")
    		return(0)
		}
        if (method[1] == "hist") {
			Px <- hist(x, breaks=n, plot=FALSE, ...)$counts / N
		}
		else {
			dens <- density(x, n=n, ...)
			Px <- dens$y[1:(n-1)] * diff(dens$x)
		}
		Px <- Px[Px > 0]
		if (length(Px) == 0) return(0)
    	return(-sum(Px * log2(Px)))
	}
)
