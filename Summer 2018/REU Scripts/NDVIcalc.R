# load packages 
setwd("/Users/mnukala/Documents/REU Project/NDVI and EVI/")
library(raster)
library(rgdal)
library(sp)
library(ggplot2)

# load 16-bit bands 4 - 6
b6 <- raster("/Users/mnukala/Documents/REU Project/Humacao Landsat Imagery/LC080040472017101201T1-SC20180605155703/LC08_L1TP_004047_20171012_20171024_01_T1_b6.tif")
b5 <- raster("/Users/mnukala/Documents/REU Project/Humacao Landsat Imagery/LC080040472017101201T1-SC20180605155703/LC08_L1TP_004047_20171012_20171024_01_T1_b5.tif")
b4 <- raster("/Users/mnukala/Documents/REU Project/Humacao Landsat Imagery/LC080040472017101201T1-SC20180605155703/LC08_L1TP_004047_20171012_20171024_01_T1_b4.tif")
b2 <- raster("/Users/mnukala/Documents/REU Project/Humacao Landsat Imagery/LC080040472017101201T1-SC20180605155703/LC08_L1TP_004047_20171012_20171024_01_T1_b2.tif")

# I want to use this loop to load the files as rasters
# it's loading files as characters
bands = list.files(path,
                   full.names = FALSE,
                   pattern = "T1_b\\d.tif$")

# and then name each with the substring from the file name (b1, b2, b3, etc.)
for(i in length(bands)){
  name <- substring(bands[[i]], 42, 43) #For each
}

# function for adjusting extent of values to 0:255
val_adjust <- function(raster){
  ((raster - minValue(raster)) * 255) / (maxValue(raster) - minValue(raster)) + 0
}

# adjust each band (loop wouldn't work)
b6 <- val_adjust(b6_16)
b5 <- val_adjust(b5_16)
b4 <- val_adjust(b4_16)
b2 <- val_adjust(b2_16)
bands = list(b6, b5, b4, b2)

# stack raster layers
satImage <- stack(bands)
plotRGB(satImage)

# overlay ndvi
ndviFun <- function(nir, red){
  (nir - red) / (nir + red)
}
ndvi2 <- overlay(b5, b4, fun = ndviFun)
plot(ndvi2, col = cm.colors(256))

# overlay evi (this keeps on crashing for some reason)
#         eviFun <- function(nir, red, blue){
#           2.5 * ((b5 -b4) / (b5 + 6 * b4 - 7.5 * b2 + 1))
#         }
#         evi2 <- overlay(b5, b4, b2, fun = eviFun)

# export as raster objects
writeRaster(ndvi2, "oct_10_n_ndvi", 
            format = "GTiff", 
            bylayer = FALSE, 
            overwrite = TRUE)

# list of NDVI file paths
aug09 = raster("/Users/mnukala/Documents/REU Project/NDVI and EVI/aug_09_n_ndvi.tif")
aug25 = raster("/Users/mnukala/Documents/REU Project/NDVI and EVI/aug_25_n_ndvi.tif")
oct10 = raster("/Users/mnukala/Documents/REU Project/NDVI and EVI/oct_10_n_ndvi.tif")
all_NDVI = list(aug09, aug25, oct10)

# set low NDVI values to NA (this loop isn't working, the values below 0.2 are not NA)
for(NDVI in all_NDVI){
  values(NDVI)[values(NDVI) < 0.2] = NA
  print(minValue(NDVI))
}
#this works
values(aug09)[values(aug09) < 0.2] = NA
values(aug25)[values(aug25) < 0.2] = NA
values(oct10)[values(oct10) < 0.2] = NA

# adjust extent (is there a shorter way to do this?)
oct10 = crop(oct10, aug09)
aug09 = crop(aug09, oct10)
aug25 = crop(aug25, oct10)

# plot NDVI 
stack_NDVI <- stack(aug09, aug25, oct10)
plot(stack_NDVI)
hist(stack_NDVI)

# export NDVI geotiffs
writeRaster(stack_NDVI, "timeseries/n_timeseries", 
            format = "GTiff", 
            bylayer = TRUE, 
            overwrite = TRUE)

# calc & plot mean NDVI
mean_NDVI <- cellStats(stack_NDVI, median)
mean_NDVI <- as.data.frame(mean_NDVI)
names(mean_NDVI) <- "avgNDVI"
mean_NDVI$day <- 221
mean_NDVI[2,2] <- 237
mean_NDVI[3,2] <- 283
ggplot(mean_NDVI, aes(day, avgNDVI)) +
  geom_point(na.rm = TRUE) +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 0.450))



