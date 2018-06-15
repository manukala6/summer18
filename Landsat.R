# load required libraries
library(raster)
library(ggplot2)
library(sp)
library(rgdal)
library(grDevices)
library(RStoolbox)

# load Landsat SR imagery
setwd("/Users/mnukala/Documents/REU Project/Landsat/Landsat_0809") # change date here
tempA <- list.files(pattern = ".tif$")
tempB <- lapply(tempA, raster)
ld0809 <- stack(tempB) # change date here 

# load Landsat NDVI imagery
ndvi_ld0809 = raster("/Users/mnukala/Documents/REU Project/Landsat/NDVI_EVI/LC08_L1TP_004047_20170809_20170824_01_T1_sr_ndvi.tif")
ndvi_ld0825 = raster("/Users/mnukala/Documents/REU Project/Landsat/NDVI_EVI/LC08_L1TP_004047_20170825_20170913_01_T1_sr_ndvi.tif")

# crop imagery to Humacao Natural Reserve
new.extent <- extent(206000, 211000, 2010000, 2014000)
cr_ld0809 <- crop(x = ld0809, y = new.extent)
ndvi_cr_ld0825 <- crop(x = ndvi_ld0825, y = new.extent)

# plot imagery
plotRGB(cr_ld1012, 5, 4, 3, stretch = "hist")
plotRGB(cr_ld0809, 5, 4, 3, stretch = "hist")
plotRGB(cr_ld0825, 5, 4, 3, stretch = "hist")
plot(ndvi_cr_ld0809)
plot(ndvi_cr_ld0825)

# cloud mask and shadow mask

cloud_and_shadow <- function(landsat){
  cm_landsat <- cloudMask(landsat, threshold = 0.8, blue = 2, tir = 8, 
                              buffer = NULL,plot = FALSE, verbose = TRUE)
  cm_landsat[["CMASK"]][cm_landsat[["CMASK"]] == 1] <- 2
  cm_landsat[["CMASK"]][is.na(cm_landsat[["CMASK"]])] <- 1
  cm_landsat[["CMASK"]][cm_landsat[["CMASK"]] == 2] <- NA
  masked_landsat <- mask(landsat, mask = cm_landsat)
  return(masked_landsat)
}

cm_cr_ld0825 <- cloud_and_shadow(cr_ld0825) # change date here
plotRGB(cm_cr_ld0825, 5, 4, 3, stretch = 'hist')

# NDVI difference
calc_diff <- function(pre, post){
  ndvi_diff <- post - pre
  return(ndvi_diff)
}
ndvi_diff <- overlay(ndvi_cr_ld0809, ndvi_cr_ld0825, fun = calc_diff)
plot(ndvi_diff, axes = TRUE, main = "Landsat - NDVI Difference")

