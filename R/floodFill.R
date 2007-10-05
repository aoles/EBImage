setMethod("floodFill", signature(x="array", pt="ANY"),
  function(x, pt, col, tolerance=1e-3, ...) {
    pt = as.integer(pt)
    if (missing(col)) stop("'col' must be specified")
#    if (missing(col)) col=x[pt[1]+pt[2]*dim(x)[1]]
    if (is.integer(x)) col=as.integer(col) else
    if (is.double(x)) col=as.double(col)
    return( .Call("lib_floodFill", x, pt, col, as.numeric(tolerance)))
  }
)
