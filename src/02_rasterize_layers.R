## Rasterizar layers Sinchi 
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## mayo 2025



## Rasterize layers ----


## Cicatrices Quema 

cq_raster_list <- map2(cq_to_raster, names(cq_to_raster), 
                       ~rasterize_sinchi(.x, paste0("cq_", .y),
                                         field_to_rast = "id_ciq", 
                                         resol = 30, out_path = "output/raster/"))
                                         
                                        

cq_raster <- rasterize_sinchi(cicatrices_quema, "Cicatrices_quema", 
                              field_to_rast = "id_ciq", 
                              resol = 30, out_path = "output/raster/")


## Visualizar raster cicatrices
plot(cq_raster)
lines(st_transform(departamentos_sinchi, crs = 9377))

## Transform to binary map
cq_raster_bin <- cq_raster > 0

plot(cq_raster_bin)
lines(st_transform(departamentos_sinchi, crs = 9377))

## Save rasters
writeRaster(cq_raster_bin, filename = "output/cic_quema_rbin_crs9377.tif", overwrite = TRUE)


## Corte caqueta
caqueta_shp_9377 <- st_transform(caqueta_shp, 9377)

cq_caqueta_raster <- crop(cq_raster_bin, vect(caqueta_shp_9377), mask = T) 
# %>% mask(st_transform(caqueta_shp, 9377))
# 
# 
# 
## 2. Calcular el área de cada fuente de datos  ---
#cicatrices_caqueta_mask <- cicatrices_caqueta > 0

# sum(cq_raster_bin)
area_raster_caqueta <- sum(values(cq_caqueta_raster), na.rm=TRUE) * (30 * 30) / 10000

# Area corte del shapefile
area_cq_caqueta_origen <- cicatrices_shp_proj %>% 
  filter(departamen == "CAQUETÁ",  area_ha_ci >0 ) %>%
  summarise(area_total =  sum(area_ha_ci)) %>% 
  pull(area_total)



## Coberturas ----

library(furrr)
coberturas_raster_list <- map2(coberturas_to_raster, names(coberturas_to_raster), 
                       ~rasterize_sinchi(.x, paste0("coberturas_", .y),
                                         field_to_rast = "cobertura_code", 
                                         resol = 30, out_path = "output/raster/"))

coberturas_raster <- rasterize_sinchi(coberturas_sinchi, "Tipos_cobertura", 
                                      field_to_rast = "cobertura_code", 
                                      resol = 30, out_path = "output/")




# cicatrices_shp_proj$tipo_cober %>% unique()
# c("BE", "NI", " NB", "DF", "RG")


## Transform to binary - deforestacion
deforest_raster <- coberturas_raster == "DF"
## Save rasters
writeRaster(deforest_raster, filename = "output/deforest_rbin_crs9377.tif", overwrite = TRUE)

