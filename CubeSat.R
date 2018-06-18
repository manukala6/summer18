# load required libraries
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(RStoolbox)

# load Planet imagery
pl0912 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_0912.tif")
pl0928 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_0928.tif")
pl1013 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_1013.tif")
pl1021 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_1021.tif")
pl1104 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_1104.tif")
pl1122 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_1122.tif")
pl1202 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_1202.tif")
pl1219 = stack("/Users/mnukala/Documents/REU Project/Planet/SR/CubeSat_1219.tif")
pl = list(pl0912, pl0928, pl1013, pl1021, pl1104, pl1122, pl1202, pl1219)
dates = list('0912', '0928', '1013', '1021', '1104', '1122', '1202', '1219')

# crop imagery to Humacao Natural Reserve
new_ext <- extent(206000, 211000, 2010000, 2014000)
humacao_crop <- function(planet){
  cropped <- crop(x = planet, y = new_ext)
  return(cropped)
}
cr_pl = list()
for(i in 1:length(pl)){
  cr_pl[[i]] <- pl[[i]]
  cr_pl[[i]] <- humacao_crop(cr_pl[[i]])
  assign(paste('cr_pl', dates[i], sep = ""), cr_pl[[i]])
}

# plot imagery
# for planet - 1 (blue), 2 (green), 3 (red), 4 (NIR)
plotRGB(cr_pl[[2]], 4, 3, 2, stretch = "hist")
plotRGB(cr_pl0928, 4, 3, 2, stretch = "hist")

# cloud masking -- NEEDS WORK
setwd("/Users/mnukala/Documents/REU Project/Planet/UDM") # change date here
udm_pl_temp <- list.files(pattern = ".tif$")
udm_pl <- lapply(udm_pl_temp, raster)
for(i in 1:length(udm_pl)){
  assign(paste('udm_pl', dates[i], sep = ""), udm_pl[[i]])
}
udm_pl = list(udm_pl0912, udm_pl0928, udm_pl1013, udm_pl1021, udm_pl1104, udm_pl1122, 
              udm_pl1202, udm_pl1219)
cr_udm_pl = list()
for(i in 1:length(udm_pl)){
  cr_udm_pl[[i]] <- udm_pl[[i]]
  cr_udm_pl[[i]] <- humacao_crop(cr_udm_pl[[i]])
  assign(paste('cr_udm_pl', dates[i], sep = ""), cr_udm_pl[[i]])
}
planet_mask <- function(planet, planet_udm){
  values(planet_udm)[is.na(values(planet_udm))] = 0.5
  values(planet_udm)[values(planet_udm) > 0.5] = NA
  masked_image <- mask(planet, mask = planet_udm)
}
cm_cr_pl = list()
for(i in 1:length(udm_pl)){
  cm_cr_pl[[i]] <- cr_udm_pl[[i]]
  cm_cr_pl[[i]] <- humacao_crop(cm_cr_pl[[i]])
  assign(paste('cm_cr_pl', dates[i], sep = ""), cm_cr_pl[[i]])
}

# plot cloud-masked imagery - NEEDS WORK
cm_cr_pl0912 <- planet_mask(cr_pl0912, cr_udm_pl0912)
plot(cm_cr_pl0912[[4]])
plotRGB(cm_cr_pl0912, 4, 3, 2, stretch = "hist")
plotRGB(cr_pl0912, 4, 3, 2, stretch = 'hist')
cm_cr_pl0928 <- planet_mask(cr_pl0928, cr_udm_pl0928)

# NDVI calculation 
ndviFun <- function(nir, red){
  (nir - red) / (nir + red)
}
ndvi_pl = list()
for(i in 1:length(pl)){
  ndvi_pl[[i]] <- cr_pl[[i]]
  ndvi_pl[[i]] <- overlay(cr_pl[[i]][[4]], cr_pl[[i]][[3]], fun = ndviFun)
  values(ndvi_pl[[i]])[values(ndvi_pl[[i]]) < 0.15] = NA # threshold cloud masking, temporary
  assign(paste('ndvi_pl', dates[i], sep = ""), ndvi_pl[[i]])
  message(i, " of ", length(pl), " completed.")
}
ndvi_stack <- stack(ndvi_pl)
plot(ndvi_stack)
ggR(ndvi_stack, geom_raster = TRUE) + 
  scale_fill_gradient2(low = 'red', high = 'green', mid = 'yellow', midpoint = 0) +
  ggtitle("sf") + theme_minimal()

# plot time series of mean NDVI
NDVI_time <- cellStats(ndvi_stack, mean)
NDVI_time <- as.data.frame(NDVI_time)
names(NDVI_time) <- 'meanNDVI'
jul_dates <- list('255', '271', '286', '294', '308', '326', '336', '353')
NDVI_time$date <- jul_dates
ggplot(NDVI_time, aes(unlist(date), unlist(meanNDVI))) +
  geom_point(size = 2, na.rm = TRUE) + coord_cartesian(ylim = c(0, 0.75)) +
  labs(title = "Post-hurricane NDVI recovery", x = "Julian Date", y = "mean NDVI")

# NDVI comparison
calc_diff <- function(pre, post){
  ndvi_diff <- post - pre
  return(ndvi_diff)
}
ndvi_diff_1 <- overlay(ndvi_pl0912, ndvi_pl0928, fun = calc_diff)
ndvi_diff_2 <- overlay(ndvi_pl0928, ndvi_pl1013, fun = calc_diff)
ndvi_diff_3 <- overlay(ndvi_pl1013, ndvi_pl1021, fun = calc_diff)
ndvi_diff_4 <- overlay(ndvi_pl1021, ndvi_pl1102, fun = calc_diff)
ndvi_diff_5 <- overlay(ndvi_pl1102, ndvi_pl1122, fun = calc_diff)
ndvi_diff_6 <- overlay(ndvi_pl1122, ndvi_pl1202, fun = calc_diff)
ndvi_diff_7 <- overlay(ndvi_pl1202, ndvi_pl1219, fun = calc_diff)
ggR(ndvi_diff_2, geom_raster = TRUE) + 
  scale_fill_gradient2(low = 'red', high = 'green', mid = 'yellow', midpoint = 0) +
  ggtitle("Change in NDVI from Sep 28 to Oct 13") + theme_minimal() +
  coord_cartesian()


# export raster
setwd("/Users/mnukala/Documents/REU Project/Planet/R") # change workspace here
writeRaster(cr_pl0912, "Planet_Sep12", format = "GTiff", overwrite = TRUE)
writeRaster(cr_pl0928, "Planet_Sep28", format = "GTiff", overwrite = TRUE)
writeRaster(ndvi_cr_pl0912, "Planet_Sep12_NDVI", format = "GTiff", overwrite = TRUE)
writeRaster(ndvi_cr_pl0928, "Planet_Sep28_NDVI", format = "GTiff", overwrite = TRUE)
writeRaster(ndvi_diff, "Planet_Sep12_Sep28_NDVI", format = "GTiff", overwrite = TRUE)

