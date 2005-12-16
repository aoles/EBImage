library(EBImage)
im = read.image("cells.jpg")
s = despeckle(tresh(im,30,30,200))
dm = distMap(s)

preprocess <- function(x) {
    if (!is(x, "Image2D"))
        stop("x must be a 2D image")
    return(normalize(distMap(despeckle(tresh(im,30,30,200)))))
}

dzdxPlusdzdy <- function(x) {
    d = dim(x)
    dat = as.array(x)
    dzdx = diff(dat)[,1:(d[[2]]-1)]
    dzdy = t(diff(t(dat)))[1:(d[[1]]-1),]

    res = Image2D.CopyHeader(x, dzdx^2 + dzdy^2)
    return(res)
}

d2zdx2Plusd2zdy2 <- function(x) {
    d = dim(x)
    dat = as.array(x)
    res = Image2D.CopyHeader(x, diff(diff(dat))[,2:(d[[2]]-1)] + t(diff(diff(t(dat))))[2:(d[[1]]-1),])
    return(res)

}

indexCells <- function(x, ...) {
    .warnOnStack(x, ...)
    x = im.gaussian(im.distMap(x, ...), 4, 2, ...)

    indexmax <- function(x) {
        if (!is(x, "Image2D") || is(x, "Image3D"))
            stop("argument to indexmax must be Image2D")
        nc = ncol(x)
        nr = nrow(x)
        ind = rep(2:(nr - 1), nc - 2) + rep(1:(nc - 2), each = nr - 2) * nr
        ## compare against maximum of the 8 neighbours
        x = as.vector(x)
        whMax = ind[  x[ind] > pmax(x[ind-1], x[ind+1], x[ind-nr], x[ind-nr-1], x[ind-nr+1], x[ind+nr], x[ind+nr-1], x[ind+nr+1]) ]

        ## convert to x,y - but the scalar indexing is more efficient
        ## cbind( 1+ (whMax-1)%%nr, 1+ (whMax-1)%/%nr )
        return(whMax)
    }

    if (is(x, "Image3D")) {
        res = list(dim(x)[[3]])
        for (i in 1:dim(x)[[3]])
            res[[i]] = indexmax(x[ , , i])
    }
    else
        res = indexmax(x)
    return(res)
}

getCenters <- function(x) {
    if (class(x) != "Image2D")
        stop("not supported for provided argument class")
    dims = dim(x)
    x = im.sample(x, dims[[1]]*2, dims[[2]]*2)
    x = im.tresh(x, 40, 40, 1000)
    x = im.segment(normalize(im.distMap(im.edge(normalize(im.distMap(x)), 1))))
    x = im.sample(x, dims[[1]], dims[[2]])

}

im.log <- function(x, m = exp(1)) {
    return( normalize( log(x / 65535 * (m - 1) + 1) ) )
}

im.sqrt <- function(x) {
    return( normalize( sqrt(x / 65535)    )   )
}
