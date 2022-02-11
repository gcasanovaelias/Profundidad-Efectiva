# Packages----
install.packages(c("glcm", "rgdal", "raster"))
library(glcm)
library(raster)
library(tidyverse)
library(sf)

# WD
setwd("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva")

# Datos
ae.shp <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/Cau_18s.shp")

composite2 <- raster::stack("./L8_composite.tif") 

plotRGB(composite2,4,3,2)
plotRGB(composite, 4, 3, 2, axes=TRUE, main=321)

plot(composite$SR_B3)

composite_glcm <- glcm(raster(composite2, layer = 5),
                       n_grey = 32,
                       window = c(3,3),
                       shift = c(1,1),
                       statistics = c("mean", 
                                      "variance", 
                                      "homogeneity", 
                                      "contrast",
                                      "dissimilarity", 
                                      "entropy", 
                                      "second_moment",
                                      "correlation"))

writeRaster(composite_glcm,"glcm.tif", format = "GTiff", overwrite = T)

stack("glcm.tif")
plot(stack("glcm.tif"))

writeRaster(composite2$SR_B5,"nir_composite.tif", format = "GTiff")




