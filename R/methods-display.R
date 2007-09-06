setMethod ("display", signature(x="Image"),
  function (x, no.GTK=FALSE, ...) {
    if ( !.isCorrectType(x) ) x <- .correctType (x)
    invisible ( .DoCall("lib_display", x, as.logical(no.GTK) ) )
  }
)

setMethod ("display", signature(x="array"),
  function (x, no.GTK=FALSE, ...)
    display(Image(x), no.GTK=no.GTK, ...))
