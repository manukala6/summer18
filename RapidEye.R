# import libraries
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(RStoolbox)

# load rasters
re0917 = stack('/Users/mnukala/Documents/REU Project/Planet/SR/RapidEye_0917.tif')
re1004 = stack('/Users/mnukala/Documents/REU Project/Planet/SR/RapidEye_1004.tif')
re1202 = stack('/Users/mnukala/Documents/REU Project/Planet/SR/RapidEye_1201.tif')
re = list(re0917, re1004, re1202)
dates = list('0917', '1004', '1202')

# crop rasters
new_ext <- extent(206000, 211000, 2010000, 2014000)
humacao_crop <- function(planet){
  cropped <- crop(x = planet, y = new_ext)
  return(cropped)
}
cr_re <- list()
for(i in 1:length(re)){
  cr_re[[i]] <- re[[i]]
  cr_re[[i]] <- humacao_crop(cr_re[[i]])
  assign(paste('cr_re', dates[i], sep = ""), cr_re[[i]])
}

plotRGB(cr_re0917, 5, 2, 1)
plotRGB(cr_re1004, 5, 2, 1)
plotRGB(cr_re1202, 5, 2, 1)

# red NDVI
rndviFun <- function(re, red){
  rndvi <- ((red - re)/(red + re))
  return(rndvi)
}
rndvi_re0917 <- overlay(cr_re0917[[5]], cr_re0917[[4]], fun = rndviFun)
plot(rndvi_re0917)
rndvi_re1004 <- overlay(cr_re1004[[5]], cr_re1004[[4]], fun = rndviFun)
plot(rndvi_re1004)
rndvi_re1202 <- overlay(cr_re1202[[5]], cr_re1202[[4]], fun = rndviFun)
plot(rndvi_re1202)

# modified-red NDVI
rmndviFun <- function(re, red, blue){
  re_red <- red - r
  re_blu <- red + r - (2 * blue)
  rmndvi <- re_red / re_blue
}
rmdvi_re0917 <- overlay(cr_re0917[[5]], cr_re0917[[4]],
                        cr_re0917[[1]], fun = rndviFun)
plot(rmndvi_re0917)
rmdvi_re1004 <- overlay(cr_re1004[[5]], cr_re1004[[4]], 
                        cr_re1004[[1]], fun = rndviFun)
plot(rmndvi_re1004)
rmdvi_re1202 <- overlay(cr_re1202[[5]], cr_re1202[[4]], 
                        cr_re1202[[1]], fun = rndviFun)
plot(rmndvi_re1202)

