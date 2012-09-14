## cat newAgainstOld.R | R --vanilla &>newAgainstOld.Rout.save

library("EBImage")

## Fetch test images frome the on-line respository
repositoryURL  = "http://www-huber.embl.de/EBImage/ExampleImages"

testImages = c(	
  "lena.jpg",		# JPEG Greyscale
  "lena-color.jpg",	# JPEG Color
  "dices-grey.png",	# PNG GA
  "dices.png",		# PNG RGBA
  "photo_16b.png",	# PNG RGB 16bps
  "dices-grey.tif",	# TIFF GA 8bps
  "photo_8b.tif",	# TIFF RGB 8bps
  "lena-original.tif",	# TIFF RGB 8bps
  "dices.tif",		# TIFF RGBA 8bps
  "nuc.tif",		# TIFF G 16bps
  "photo_16b.tif"	# TIFF RGB 8bps multiple pages
)

names(testImages) = rep(repositoryURL, length(testImages))

for (i in seq_along(testImages))
  if (!file.exists(testImages[i]))
    system(paste("wget ", names(testImages)[i], "/", testImages[i], sep=""))

## Append package test images

packageTestImages = list.files(system.file("images", package="EBImage"), full.names=TRUE)
names(packageTestImages) =  list.files(system.file("images", package="EBImage"))
names(testImages) = testImages
testImages = c(testImages, packageTestImages)

## Actual tests

similar = function(x, y) {
  if ( identical(dim(x), dim(y)) )
    max(abs(x-y)) < 1/(2^32-1)
  else
    FALSE
}

for (i in seq_along(testImages)) {
  old = readImage(testImages[i])
  new = readImage2(testImages[i])
  filename = names(testImages)[i]
  cat(filename, rep(" ", 40-nchar(filename)), if(identical(old, new)) "PASS" else "FAIL", "  ", if(similar(old, new)) "PASS" else "FAIL", "\n", sep="")	
}