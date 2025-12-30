### revisar info SINCHI
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## mayo 2025


# Libraries
# library(tidyverse)
# library(terra)
# library(sf)
# library(lwgeom) 


# - Capa en formato vector trabajada con anterioridad con los campos de municipio y departamento del año 2023
# 
# - Capa de deforestación 2023 formato raster
# - Capa de cicatrices 2023 formato raster
# - División municipal y departamental 2014 (la que se usa en el instituto)
 # link = https://drive.google.com/drive/folders/1ab5eIOMRMtYXnrO_gtH2WGq2KuZgecin?usp=sharing

files_shp_cq <- list.files("data/cicatrices_quema/", recursive = T, full.names = TRUE,
                           pattern = ".shp$")

## archivos raster
# file_bnb <- "data/cicatrices_quema/Raster/2022_2023/CCbnb2223.tif"
# file_cq <- "data/cicatrices_quema/Raster/2022_2023/CCiq2023.tif"

## Rasters
# rast_bnb <- rast(file_bnb)
# rast_cq <- rast(file_cq)
# # Visualizar metadata
# rast_bnb
# rast_cq


# Archivos vectoriales "shapefiles"
file_shp_dpto_mpio <- "data/UER/CPai2014_CCar2014_CDep2014_CMun2014_CElt2019.shp"
# file_shp_cicatrices <- "data/cicatrices_quema/Vector/2022_2023/CCbnb2223v1_CCiq2023_CFgo2023_Pai_Car_Dep_Mun_Elt.shp"

# Vectoriales
limites_adm <- read_sf(file_shp_dpto_mpio)

cq_names <- word(files_shp_cq, 3, sep = "/")


files_shp_cq
cicatrices_list <- map(files_shp_cq, ~st_read(.x, as_tibble = TRUE)) %>% 
  setNames(cq_names)

#Visualizar tablas atributos
# limites_adm
# cicatrices_shp
# cicatrices_shp <- shp2_ %>% 
#   mutate(cicatriz_quema = ifelse(area_ha_ci>0, 1, 0))

# plot(cicatrices_shp['cicatriz_quema'])

# Explorar contenidos
# shp1_$sigla_elt %>% unique()
# shp1_$leyenda %>% unique()
# shp2_$id_ciq %>% hist
# shp2_$tipo_cober %>% unique()
# table(shp2_$id_ciq>0)
# table(shp2_$area_ha_ci>0)
# shp2_$Point_Coun %>% hist


#table(shp2_$id_ciq, shp2_$area_ha_ci)

## 
## Simplificar layers dptos y municipios ----


## Limites administrativos, clasificaciones, jurisdiccion, areas
departamentos_sinchi <- limites_adm %>%
  group_by(id_uer_dep, departamen) %>%
  summarise(geometry = st_union(geometry), .groups="drop")

municipios_sinchi <- limites_adm %>%
  group_by(departamen, id_uer_mun, municipio) %>%
  summarise(geometry = st_union(geometry), .groups="drop")

car_sinchi <- limites_adm %>%
  group_by(id_uer_car, car) %>%
  summarise(geometry = st_union(geometry), .groups="drop")

zonificacion_sinchi <- limites_adm %>%
  group_by(id_uer_elt, sigla_elt, leyenda) %>%
  summarise(geometry = st_union(geometry), .groups="drop")

paisaje_sinchi <- limites_adm %>%
  group_by(id_uer_pai, paisaje) %>%
  summarise(geometry = st_union(geometry), .groups="drop")


## Visualizar prueba
# plot(rast_bnb)
# lines(departamentos_sinchi)
# 
# plot(zonificacion_sinchi)
# 
# plot(paisaje_sinchi)
# 
# plot(CARs_sinchi)


# Generar sample - test - Caqueta


## Crop test Caqueta
# caqueta_shp <- departamentos_sinchi %>% filter(departamen == "CAQUETÁ")
# caqueta_mpios <- municipios_sinchi %>% filter(departamen == "CAQUETÁ")
# 
# caqueta_cq <- crop(rast_cq, caqueta_shp, mask = T)
# caqueta_bnb <- crop(rast_bnb, caqueta_shp, mask = T)
# # freq(caqueta_bnb)
# 
# caqueta_bnb_bin <- ifel(round(caqueta_bnb) == 5, 1, 0)
# 
# #plot(caqueta_bnb)
# plot(caqueta_bnb_bin)
# lines(caqueta_mpios)
# 
# 
# # plot(caqueta_cq)  # No existe info en el raster entregado
# 
# # freq(round(caqueta_cq))
# 
# 
# ## save UER
# 
# uer_objs <- c(
#   "departamentos_sinchi",
#   "municipios_sinchi",
#   "car_sinchi",
#   "zonificacion_sinchi",
#   "paisaje_sinchi"
# )
# 
# ## Guarda objetos vectoriales como GPKS
# walk(
#   uer_objs,
#   ~ {
#     file  <- file.path("output/uer", paste0(.x, ".gpkg"))
#     st_write(get(.x), dsn = file, delete_dsn = TRUE)
#   }
# )

