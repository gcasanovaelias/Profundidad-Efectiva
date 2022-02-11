# WD
setwd("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva")

# Datos
ae.shp <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/Cau_18s.shp")

composite2 <- raster::stack("./L8_composite.tif") 

composite <- composite %>% 
  mask(ae.shp) %>% #No funcionaba con ae en formato raster
  setExtent(ext = extent(ae)) #El extent no calzaba incluso despues del mask() por lo que se aplica esta función

extent(composite);extent(ae)
            
composite$SR_B1

plot(composite)
