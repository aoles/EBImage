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
smoments = function (x, ref, pw=3, what="scale") {
  checkCompatibleImages(x, ref)
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
  checkCompatibleImages(x, ref)
  
  cmo = cmoments (x=x, ref=ref)
  smo = smoments (x=x, ref=ref, pw=2, what="c")
  rmo = rmoments (x=x, ref=ref)
  
  summ = function(u) {
    res = matrix(0, nrow=dim(u)[3], ncol=7)
    colnames(res) = c('m.n20', 'm.n11', 'm.n02', 'm.theta', 'm.l1', 'm.l2', 'm.ecc')

    u00 = u[1,1,]
    res[,'m.n20'] = u[3,1,] / u00 ## m20 = u20/u00
    res[,'m.n11'] = u[2,2,] / u00 ## m11 = u11/u00
    res[,'m.n02'] = u[1,3,] / u00 ## m02 = u02/u00
    
    ## theta = 1/2 * tan[-1] (2*u11/(u20-u02))
    res[,'m.theta'] = 0.5 * atan2( 2 * res[,'m.n11'], res[,'m.n20'] - res[,'m.n02'])

    ## eigenvalues of the covariance
    x1  = 0.5 * (res[,'m.n20'] + res[,'m.n02'])
    x2  = 0.5 * sqrt( 4 * res[,'m.n11']^2 + (res[,'m.n20'] - res[,'m.n02'])^2 )
    res[,'m.l1'] = x1 + x2 
    res[,'m.l2'] = x1 - x2
    res[,'m.ecc'] = sqrt(1-res[,'m.l2']/res[,'m.l1'])
    
    ## scale invariants moments nu
    res[,'m.n20'] = res[,'m.n20'] / u00
    res[,'m.n11'] = res[,'m.n11'] / u00
    res[,'m.n02'] = res[,'m.n02'] / u00
   
    res
  }
  
  if (getNumberOfFrames(x,'total')==1) res = cbind(cmo, summ(smo), rmo)
  else {
    smo = lapply(smo, summ)
    res = vector( "list", getNumberOfFrames(x,'total'))
    for (i in 1:length(res)) res[[i]] = cbind(cmo[[i]], smo[[i]], rmo[[i]])
  }
  
  return(res)  
}



