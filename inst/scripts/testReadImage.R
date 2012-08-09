library(EBImage)
options(error=recover, warn=2)

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

