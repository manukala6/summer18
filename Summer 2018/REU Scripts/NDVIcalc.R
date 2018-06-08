# load packages 
setwd("/Users/mnukala/Documents/REU Project/Humacao Landsat Imagery/LC080040472017101201T1-SC20180605155703/")
library(raster)
library(rgdal)
library(sp)
library(ggplot2)

temp <- list.files(pattern = "T1_b\\d.tif$")
bands <- lapply(temp, raster)
band.names <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")
for(i in 1:7){
  names(bands[[i]]) <- band.names[[i]]
}
val_adjust <- function(raster){
  ((raster - minValue(raster)) * 255) / (maxValue(raster) - minValue(raster)) + 0
}
for(i in 1:7){
  bands[[i]] <- val_adjust(bands[[i]])
  message("band ", i, " of 7 complete")
  print(maxValue(bands[[i]]))
}
for(i in 1:7){
  print(maxValue(bands[[i]]))
}
satImg <- stack(bands[1:6], quick=TRUE)
plotRGB(satImg, 6, 5, 4)

# overlay ndvi
ndviFun <- function(nir, red){
  (nir - red) / (nir + red)
}
ndvi2 <- overlay(bands[[5]], bands[[4]], fun = ndviFun)
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
