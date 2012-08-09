library(EBImage)
options(error=recover, warn=2)

# could use an idiom like the following to generate a federated repository of example images for testing:

exampleImages = c("http://skuld.bmsc.washington.edu/raster3d/examples" = "density.tiff",
                  "http://another.url.ie" = "name.png", ...)
for(i in seq(along=exampleImages)){
  if(!file.exists(exampleImages[i]))
    system(paste("wget ", names(exampleImages)[i], "/", exampleImages[i], sep=""))
    readImage2(exampleImages[i])
    ...
}



f <- system.file("images", rep("lena-color.png", 3), package="EBImage")
x1 <- readImage2(f, colormode="Color")
display(x1)

f <- system.file("images", c("cells.tif", "nuclei.tif"), package="EBImage")
x2 <- readImage2(f)
display(x2)

# R logo:
f <- system.file("img", "Rlogo.tiff", package="tiff")
x3 <- readImage2(f, colormode="Color")
display(x3)

f <- system.file("images", c("cells.tif", "nuclei.tif"), package="EBImage")[1]
x4 <- readImage2(f)
display(x4)

