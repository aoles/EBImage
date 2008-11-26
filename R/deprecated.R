setMethod ("write.image", signature(x="Image"),
  function (x, ...) {
    .Deprecated("writeImage", "EBImage")
    writeImage(x, ...)
  }
)

read.image <- function(files, colormode=Grayscale, ...) {
  .Deprecated("readImage", "EBImage")
  readImage(files, colormode, ...)
}

choose.image <- function(colormode=Grayscale) {
  .Deprecated("chooseImage", "EBImage")
  chooseImage(colormode)
}

setMethod ("hull.features", signature(x="IndexedImage"),
  function(x, ...) {
    .Deprecated("hullFeatures", "EBImage")
    hullFeatures(x, ...)
  }
)

setMethod ("edge.profile", signature(x="IndexedImage"),
  function (x, ...) {
    .Deprecated("edgeProfile", "EBImage")
    edgeProfile(x, ...)
  }
)

setMethod ("edge.features", signature(x="IndexedImage"),
  function (x, ...) {
    .Deprecated("edgeFeatures", "EBImage")
    edgeFeatures(x, ...)
  }
)

setMethod ("haralick.matrix", signature(x="IndexedImage", ref="Image"),
  function(x, ref, ...) {
    .Deprecated("haralickMatrix", "EBImage")
    haralickMatrix(x, ref, ...)
  }
)

setMethod ("haralick.features", signature(x="IndexedImage", ref="Image"),
  function(x, ref, ...) {
    .Deprecated("haralickFeatures", "EBImage")
    haralickFeatures(x, ref, ...)
  }
)

setMethod ("zernike.moments", signature(x="IndexedImage", ref="Image"),
  function(x, ref, ...) {
    .Deprecated("zernikeMoments", "EBImage")
    zernikeMoments(x, ref, ...)
  }
)
