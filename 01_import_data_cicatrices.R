### revisar info SINCHI

# Libraries
library(tidyverse)
library(terra)
library(sf)


# - Capa en formato vector trabajada con anterioridad con los campos de municipio y departamento del año 2023
# 
# - Capa de deforestación 2023 formato raster
# - Capa de cicatrices 2023 formato raster
# - División municipal y departamental 2014 (la que se usa en el instituto)


## archivos raster
file_bnb <- "data/cicatrices_quema/Raster/2022_2023/CCbnb2223.tif"
file_cq <- "data/cicatrices_quema/Raster/2022_2023/CCiq2023.tif"

## Raster
rast_bnb <- rast(file_bnb)
rast_cq <- rast(file_cq)

rast_bnb
rast_cq

file_shp_dpto_mpio <- "data/cicatrices_quema/Vector/UER/CPai2014_CCar2014_CDep2014_CMun2014_CElt2019.shp"
file_shp_cicatrices <- "data/cicatrices_quema/Vector/2022_2023/CCbnb2223v1_CCiq2023_CFgo2023_Pai_Car_Dep_Mun_Elt.shp"


shp1_ <- read_sf(file_shp_dpto_mpio)
shp2_ <- read_sf(file_shp_cicatrices)


shp1_$sigla_elt %>% unique()
shp1_$leyenda %>% unique()

shp2_$id_ciq %>% hist
table(shp2_$id_ciq>0)
table(shp2_$area_ha_ci>0)

shp2_$Point_Coun %>% hist


table(shp2_$id_ciq, shp2_$area_ha_ci)

municipios_sinchi <- shp1_ %>%
  group_by(departamen) %>%
  summarise(geometry = st_union(geometry))


departamentos_sinchi <- shp1_ %>%
  group_by(departamen) %>%
  summarise(geometry = st_union(geometry))

municipios_sinchi <- shp1_ %>%
  group_by(departamen) %>%
  summarise(geometry = st_union(geometry))


plot(rast_bnb)
lines(departamentos_sinchi)



## Crop test Caqueta


caqueta_shp <- departamentos_sinchi %>% filter(departamen == "CAQUETÁ")

caqueta_cq <- crop(rast_cq, caqueta_shp, mask = T)


plot(caqueta_cq)
