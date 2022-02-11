# Packages----
library(tidyverse)
library(raster)
library(rgdal)
library(sf)
library(VSURF)
library(randomForest)
library(hydroGOF)
library(tidymodels)

# Datos----
setwd("~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva/Profundidad-Efectiva")

prof_efect <- read_sf("prof_efect.shp")

ae <- read_sf("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/Cau_18s.shp")

db <- st_intersection(prof_efect, ae)

# Raster de referencia
raster <- raster(res = 30,
                 crs = st_crs(ae),
                 ext = extent(ae))

# Rasterizar AE
ae <- rasterize(x = ae,
                y = raster, 
                res = 30)
#writeRaster(ae, filename = "ae.tif", overwrite = T)
ae <- raster("ae.tif")

# Rasterizar BD
db.rsat <- rasterize(x = db,
                     y = ae,
                     field = "bottom",
                     fun = mean)

# Vectorizar BD
db.ae <- rasterToPoints(x = db.rsat,
                        spatial = T)

# Nombrar columna
names(db.ae) <- "Prof_efect"

# BD covariables----
dir_predic <- "~/Casanova/Estudio Personal/Practicos PR/p11 - Modelos predictivos/predictores"
bioclim <- list.files(path = paste0(dir_predic, "/bioclim"),
                      full.names = T) %>% stack()
topo <- list.files(path = paste0(dir_predic, "/topo"),
                   full.names = T) %>% stack()
indices <- list.files(path = paste0(dir_predic, "/indices_esp"),
                      full.names = T) %>% stack()

dir_text <- "~/Casanova/Universidad/Master/Tesis/Trabajos/Profundidad efectiva"
texture <- list.files(path = paste0(dir_text, "/textura"),
                      full.names = T) %>% stack()

compareRaster(bioclim, topo, indices, texture)

# Extracción de valores
db.ae <- raster::extract(x = bioclim,
                         y = db.ae,
                         sp = T)
db.ae <- raster::extract(topo, db.ae, sp = T)
db.ae <- raster::extract(indices, db.ae, sp = T)
db.ae <- raster::extract(texture, db.ae, sp = T)

# Tabla de atributos con coordenadas
db.data <- cbind(db.ae@coords,
                 db.ae@data)

# Exportar tabla
write_csv(x = db.data,
          file = "prof_efect.csv")

# Selección de variables----
# Eliminación de filas que contengan valores NAs
db.data <- db.data[complete.cases(db.data),]

# Definición VD y VIs (covariables)
y.var <- db.data[,3]
x.var <- db.data[,4:ncol(db.data)]

# Punto semilla
set.seed(1)

# Selección de variables por RF
var.select <- VSURF(x = x.var,
                    y = y.var,
                    ntree = 1000,
                    RFimplem = "ranger")

# Importancia predictiva
imp <- var.select$imp.mean.dec

pred.imp <- x.var[var.select$imp.mean.dec.ind] %>% names()

data.imp <- data.frame(predictores = pred.imp,
                       importancia = imp)

ggplot(data = data.imp, aes(x = reorder(predictores, importancia),
                            y = importancia,
                            fill = importancia)) +
  labs(x = "Predictor",
       y = "Importancia",
       title = "Importancia predictiva variables de profundidad efectiva") +
  geom_col() +
  coord_flip() +
  theme_bw()

pred.vars <- x.var[var.select$varselect.pred]

# Entrenamiento del modelo----
# Datos de entrenamiento y validación
db.split <- initial_split(data = db.data,
                          prop = 3/4)
db.train <- training(db.split)
db.test <- testing(db.split)

# Entrenar modelo RF
rf.model <- randomForest(x = db.train[names(pred.vars)],
                         y = db.train[,3],
                         ntree = 500,
                         replace = T,
                         nodesize = 5,
                         importance = F,
                         do.trace = T,
                         keep.forest = T)

# Predicciones del modelo (subconjunto de entrenamiento)
rf.predicted <- rf.model$predicted

# Datos observados del subconjunto de entrenamiento
rf.obs <- db.train[[3]]

# Métricas de calibración
gof(sim = rf.predicted,
    obs = rf.obs)

# Validación del modelo----
# Predicciones de los datos de testeo
rf.predicted <- predict(rf.model, db.test)

# Datos observados del subconjunto de testeo
rf.obs <- db.test[[3]]

# Métricas de validación (testeo)
gof(sim = rf.predicted,
    obs = rf.obs)

# Mapa predictivo----
# Stack con los predictores
predictores <- stack(bioclim, indices, topo, texture)
stack.pred <- predictores[[names(pred.vars)]]

# Predicción
pred.map <- predict(stack.pred,
                    rf.model)

plot(pred.map)

# Bootstrapping----
# Muestras bootstrap
boot.sample <- bootstraps(data = db.train,
                          times = 10)
boot.sample$splits

analysis(boot.sample$splits[[1]])
assessment(boot.sample$splits[[1]])

# Directorio para guardar los modelos
dir.create("./modelos/")

# Entrenar y guardar modelos
map2(.x = boot.sample$splits,
     .y = 1:nrow(boot.sample),
     .f = function(data, 
                   cont){
       data <- analysis(data)
       rf <- randomForest(x = data[,names(pred.vars)],
                          y = data[,3],
                          ntree = 500,
                          replace = T,
                          nodesize = 5,
                          importance = F,
                          do.trace = T,
                          keep.forest = T)
       write_rds(rf, file = paste0("./modelos/bootstrap_",
                                   cont,
                                   ".rds"))
     })

# Leer ubicación de los modelos guardados
modelos <- list.files(path = "./modelos",
                      full.names = T)

# Generar métricas de validación para cada modelo
metricas <- lapply(X = modelos,
                   FUN = function(m){
                     rf.model <- read_rds(m)
                     rf.predicted <- predict(rf.model, db.test)
                     rf.obs <- db.test[[3]]
                     return(gof(sim = rf.predicted,
                                obs = rf.obs) %>% t())
                   }) %>% rlist::list.rbind()

metricas %>% colMeans()

# Predicción de mapas bootstrap
dir.create("./mapas_predictivos")

# Predecir mapas con cada modelo entrenado
map2(modelos,
     1:length(modelos),
     function(m, cont){
       rf.model <- read_rds(m)
       pred.map <- predict(stack.pred, rf.model)
       writeRaster(x = pred.map,
                   filename = paste0("./mapas_predictivos/bootstrap_map_", 
                                     cont,
                                     ".tif"),
                   overwrite = T)
     })

# Cargar los mapas bootstrap predictivos como stack
bootmaps <- list.files(path = "./mapas_predictivos/",
                       full.names = T,
                       pattern = ".tif$") %>% stack()

plot(bootmaps)

# Intervalos de predicción----
# Directorio donde guardar los mapas resultantes
dir.create(paste0("./mapas_predictivos/","prof_efec"))

# Media de las predicciones
avgmap <- mean(bootmaps)
plot(avgmap)

names(avgmap) <- paste0("Mean_","prof_efec")
writeRaster(avgmap,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/mean.tif"),
            overwrite = T)

# Varianza de las predicciones
sqrdif <- (bootmaps-avgmap)^2
n <- nlayers(bootmaps)
var.res <- sum(sqrdif)/(n-1)

plot(var.res)

# Exportar raster de varianza
writeRaster(var.res,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/variance.tif"),
            overwrite = T)

# Overall Variance of Predictions
mean_metrics <- colMeans(metricas)
avgMSE <- mean_metrics["MSE"]

over_var <- var.res + avgMSE
plot(over_var)

writeRaster(over_var,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/overVar.tif"),
            overwrite = T)

# Desviación estándar
sd.map <- sqrt(over_var)

writeRaster(sd.map,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/SD.tif"),
            overwrite = T)

  # Intervalos de predicción
qp <- qnorm(0.95)

# Error estándar
se.map <- sd.map*qp

# Upper limit interval
ulim_map <- avgmap + se.map

plot(ulim_map)

writeRaster(ulim_map,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/Ulim.tif"),
            overwrite = T)

# Lower limit interval
llim_map <- avgmap - se.map

plot(llim_map)

writeRaster(llim_map,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/Llim.tif"),
            overwrite = T)

# Range of Prediction----
range_map <- ulim_map - llim_map

plot(range_map)

writeRaster(range_map,
            filename = paste0("./mapas_predictivos/",
                              "prof_efec",
                              "/PIRange.tif"),
            overwrite = T)

# Graficar métricas
map.metrics <- list.files(path = paste0("./mapas_predictivos/",
                                        "prof_efec"),
                          full.names = T) %>% stack()
plot(map.metrics)

# PICP----
# Vector de quantiles (valores z)
qp <- qnorm(c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525))

# Percentiles
cs <- c(99, 97.5, 95, 90, 80, 60, 40, 20, 10, 5)
names(qp) <- cs

# Extracción de las predicciones para cada uno de los valores observados
predicciones <- lapply(modelos, function(m){
  rf.model <- read_rds(m)
  rf.predicted <- predict(rf.model,
                          db.test) 
}) %>% rlist::list.cbind()

# Datos observados
rf.obs <- db.test[[3]]

# ESTADÍSTICAS
# Media y sd de c/ predicción
pred.stats <- predicciones %>% as_tibble() %>% 
  mutate(pred.mean = rowMeans(predicciones),
         pred.sd = matrixStats::rowSds(predicciones)) %>% 
  dplyr::select(pred.mean, pred.sd)

# Error estándar de las predicciones (sd * quantile)
pred.qp <- lapply(qp, function(q){
  q * pred.stats$pred.sd
}) %>% rlist::list.cbind()

# Límites superior e inferior del intervalo de predicción para cada nivel de confianza
lim_sup <- pred.stats$pred.mean + pred.qp
lim_inf <- pred.stats$pred.mean - pred.qp

# Contar las veces que el valor observado se encuentra dentro del intervalo [lim_inf, lim_sup]
bound_Mat <- matrix(NA, nrow = nrow(pred.stats), ncol = length(qp))
#Ciclo for para rellenar la matriz
for (i in 1:length(qp)){
  bound_Mat[,i] <- as.numeric(rf.obs >= lim_inf[,i] & rf.obs <= lim_sup[,i])
}
colnames(bound_Mat) <- cs

# Cantidad de veces que el valor se encuentra dentro del intervalo
sum <- bound_Mat %>% colSums()

nrow <- nrow(bound_Mat)

# Calcular PICP
PICP <- 100*sum/nrow

# Creación de un data.frame (con los valores de porcentaje y los percentiles)
data.picp <- data.frame(PICP, percentil = cs)

#Graficar
ggplot(data.picp, aes(x = percentil, y = PICP)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red") + 
  coord_fixed(xlim = c(0,100), ylim = c(0,100))











