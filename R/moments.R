# Image moments

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

## centered moments
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cmoments = function (x, ref) {
  checkCompatibleImages(x,ref)
  return( .Call("lib_cmoments", castImage(x), castImage(ref), PACKAGE='EBImage') )
}

## full moment matrix
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
smoments = function (x, ref, pw=3, what="s") {
  checkCompatibleImages(x,ref)
  alg <- as.integer(switch(tolower(substr(what, 1, 1)), n=0, c=1, s=2, r=3, 2) )
  pw <- as.integer (pw)
  if ( pw < 1 || pw > 9 ) stop("'pw' must be in the range [1,9]" )
  if ( alg == 3 && pw < 3 ) stop( "'pw' must be at least 3 to calculate rotation invariant moments" )
  return( .Call("lib_moments", castImage(x), castImage(ref), pw, alg, PACKAGE='EBImage') )
}

## Hu's set of invariant moments
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rmoments = function (x, ref) {
  checkCompatibleImages(x,ref)
  return( .Call("lib_moments", castImage(x), castImage(ref), as.integer(3), as.integer(3), PACKAGE='EBImage') )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
moments <- function(x, ref) {
  checkCompatibleImages(x,ref)
  
  ctr <- cmoments (x=x, ref=ref)
  rmo <- rmoments (x=x, ref=ref)
  mom <- smoments (x=x, ref=ref, pw=2, what="c")
  
  summ <- function(u) {
    if ( length(u) < 6 ) return( numeric() )
    res <- matrix(0, nrow=dim(u)[3], ncol=6)
    res[,1] <- u[3,1,] / u[1,1,] # m20 = u20/u00
    res[,2] <- u[2,2,] / u[1,1,] # m11 = u11/u00
    res[,3] <- u[1,3,] / u[1,1,] # m02 = u02/u00
    res[,4] = 0.5 * atan2( 2 * res[,2], res[,1] - res[,3])  # theta = 1/2 * tan[-1] (2u11/(u20-u02))
    x1  = 0.5 * (res[,1] + res[,3])
    x2  = 0.5 * sqrt( 4 * res[,2]^2 + (res[,1] - res[,3])^2 )
    res[,5]  = x1 + x2 # eigenvalues of the cov matrix
    res[,6]  = x1 - x2 # eigenvalues of the cov matrix
    ## second division to transfer to scale invariants
    res[,1] <- res[,1] / u[1,1,]
    res[,2] <- res[,2] / u[1,1,]
    res[,3] <- res[,3] / u[1,1,]
    res[ which(is.na(res)) ] = 0.0
    colnames(res) <- c("m.n20", "m.n11", "m.n02", "m.theta", "m.l1", "m.l2")
    res
  }
  if ( getNumberOfFrames(x,'total') == 1 ) {
    mom <- summ(mom)
    return( cbind(ctr, mom, rmo) )
  }
  mom <- lapply(mom, summ)
  res <- vector( "list", getNumberOfFrames(x,'total'))
  for ( i in 1:length(res) ) {
    if ( length(ctr[[i]]) == 0 || length(mom[[i]]) == 0 || length(rmo[[i]]) == 0 ) res[[i]] <- numeric()
    else res[[i]] <- cbind(ctr[[i]], mom[[i]], rmo[[i]])
  }
  return( res )  
}



