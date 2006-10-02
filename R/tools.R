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
	function(x, ...) {
		N <- length(x)
		if (N <= 1) return(0)
#		if (N < 1000 || sd(x) == 0) {
#			Px <- hist(x, 512, plot=FALSE)$counts / N
#			warning("'hist' is used to evaluate densities instead of 'density'")
#		}
#		else {
			dens <- density(x)#, adjust=0.01)
			npts <- length(dens$y) - 1
			Px <- dens$y[1:npts] * diff(dens$x)
#		}
		Px <- Px[Px > 0]
		if (length(Px) > 0)
			return(-sum(Px * log2(Px)))
		else
			return(0)
	}
)
