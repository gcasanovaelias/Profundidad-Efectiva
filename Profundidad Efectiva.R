#Paquetes----
library(sf)
library(tidyverse)

# Datos----
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3/Observaciones")
list.files(pattern = '.shp')

# 46 observaciones
prof_efect <- read_sf("soilData.shp") %>% 
  filter(field_1 %in% c(17109,17114,17131,17135,17140,17145,17156,17159,
                        17164,17170,17174,17178,17184,17189,17194,17200,
                        17203,17207,17212,17217,17221,17225,17230,17237,
                        17242,17247,17252,17257,17261,17266,17272,17276,
                        17280,17286,17291,17296,17301,17307,17310,17317,
                        17320,17663,17999,18002,18005,18009)) %>% 
  mutate(bottom = if_else(field_1 == 17221,140,bottom)) %>% 
  mutate(bottom = if_else(field_1 == 17272,200,bottom)) %>%
  {prof_efect[prof_efect$field_1 == 17247,17] <- 120;prof_efect} %>% 
  {prof_efect[prof_efect$field_1 == 17663,17] <- 150;prof_efect} %>% 
  {prof_efect[prof_efect$field_1 == 18002,17] <- 100;prof_efect} %>% 
  {prof_efect[prof_efect$field_1 == 18009,17] <- 120;prof_efect} %>% 
  select(field_1,ID,Nombre,bottom)

# 69 observaciones (c/ 13 no descritas)
prof_efect <- read_sf("soilData.shp") %>% 
  filter(field_1 %in% c(17109,17114,17119,17125,17131,17135,17140,17145,
                        17151,17156,17159,17164,17170,17174,17178,17184,
                        17189,17194,17200,17203,17207,17212,17217,17221,
                        17225,17230,17237,17242,17247,17252,17257,17261,
                        17266,17272,17276,17280,17286,17291,17296,17301,
                        17307,17310,17317,17320,17663,17999,18002,18005,
                        18009,18692,18698,18704,18710,18935,18939,18944,
                        18949,18954,18958)) %>% 
  mutate(bottom = if_else(field_1 == 17221,140,bottom)) %>% 
  mutate(bottom = if_else(field_1 == 17272,200,bottom)) %>%
  {prof_efect[prof_efect$field_1 == 17247,17] <- 120;prof_efect} %>% 
  {prof_efect[prof_efect$field_1 == 17663,17] <- 150;prof_efect} %>% 
  {prof_efect[prof_efect$field_1 == 18002,17] <- 100;prof_efect} %>% 
  {prof_efect[prof_efect$field_1 == 18009,17] <- 120;prof_efect} %>% 
  select(field_1,ID,Nombre,bottom)

# Verificación
prof_efect %>% filter(field_1 == 18009) %>% select(bottom)

# RDS y sf
setwd("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva/Profundidad-Efectiva")
saveRDS(prof_efect,"prof_efect.rds")
write_sf(prof_efect,'prof_efect.shp')
