# Packages ----
library(glcm)
library(raster)
library(tidyverse)
library(sf)

# Datos ----
setwd("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva")

composite <- stack("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva/L8_composite.tif")

composite <- composite[[1:7]]

ae.shp <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/Cau_18s.shp")

# Raster de referencia
dir_predic <- "~/Casanova/Estudio Personal/Practicos PR/p11 - Modelos predictivos/predictores"
bioclim <- list.files(path = paste0(dir_predic, "/bioclim"),
                      full.names = T) %>% stack()

# Operaciones de registro ----
compareRaster(composite, bioclim)

# Extent
composite <- composite %>% setExtent(ext = extent(bioclim))

extent(texture);extent(bioclim)

# Remuestreo (Resampling)
composite <- composite %>% 
  resample(bioclim$max_temp_warmestmonth, method = "ngb")

compareRaster(composite, bioclim)

# Exportar composite
dir.create("./composite/")

writeRaster(x = composite,
            filename = "composite/composite_",
            format = "GTiff",
            overwrite = T,
            bylayer = T,
            suffix =names(composite))

# GLCM ----
plotRGB(composite,4,3,2, stretch = "lin")

nir = composite$SR_B5*10000

glcm_IR <- glcm(nir,
                window = c(9,9),
                shift = c(1,1),
                statistics = c("mean", 
                               "variance", 
                               "homogeneity", 
                               "contrast",
                               "dissimilarity", 
                               "entropy", 
                               "second_moment",
                               "correlation"))

texture <- glcm_IR %>% crop(ae.shp) %>% 
  mask(ae.shp)

# Exportar
dir.create("./textura/")

writeRaster(x = texture,
            filename = "textura/texture_.tif",
            format = "GTiff",
            overwrite = T,
            bylayer = T,
            suffix = names(texture))
