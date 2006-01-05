library(EBImage)

# get file list to load
files = paste("HT01-C02/", dir("HT01-C02"), sep = '')
nfiles = length(files)
files = files[(1:(nfiles / 3)) * 3 - 2]
nfiles = length(files)

# reset nfiles: MEMORY
filestart = 1
nfiles = 100

# load nfiles images
im = read.image(files[filestart:(filestart + nfiles - 1)])

# preprocess images

# normalize each image separately
normalizeTime = system.time(
  normalize(im, modify = TRUE, independent = TRUE)
)
print(normalizeTime)

rgb = toRGB(im)

# segment
treshTime = system.time(
  tresh(im, 30, 30, 200, TRUE, modify = TRUE)
)
print(treshTime)

# distance map
distMapTime = system.time(
  distMap(im, modify = TRUE)
)
print(distMapTime)

# objectCount
objectCountTime = system.time(
  res <- objectCount(im)
)
print(objectCountTime)

# get total time
totalTime = normalizeTime + treshTime + distMapTime + objectCountTime
cat("Total time required:\n")
print(totalTime)[[3]]

dims = dim(im)[1:2]

# res contains 0-based indexes for every image, which must be shifted by the total number
# of pixels multiplied by the image index to be applied to Image3D class
offset <- function(index) {
  return((index - 1) * dims[[1]] * dims[[2]])
}

# marking cell centres in original images
index = integer()
for(i in 1:nfiles) index = c(index, res[[i]][1,] + offset(i))
rgb[index] = 255
#display(im)

# are there some problems
counts = integer()
for (i in 1:nfiles) counts = c(counts, length(res[[i]][1,]))
hist(counts)

print("small count indexes")
print(which(counts < 100))
print("large count indexes")
print(which(counts > 600))
print("display(rgb[,,which(counts < 100)])")
print("display(rgb[,,which(counts < 100)])")




