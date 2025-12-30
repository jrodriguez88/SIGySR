## Preprocesamiento de layers antes de rasterizacion
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025


# cicatrices_shp
# sf::st_crs(cicatrices_shp)             # confirmar CRS 
# validos <- st_is_valid(cicatrices_shp) # vector lógico de validez  
# table(validos)  
# reasons <- sf::st_is_valid(cicatrices_shp, reason = TRUE)
# table(reasons) %>% head()


# malos <- shp2_[!validos, ]
# plot(malos["geometry"])  # inspección manual
# malos_fix <- st_make_valid(malos)  

## Corregir geometrias invalidas 
#cicatrices_shp_valid <- st_make_valid(cicatrices_shp)
#shp_valid2 <- st_buffer(shp_valid, 0)

valida_reproj_cq <- function(cicatrices_shp, epsg = 9377){
  
  ## Corregir geometrias invalidas 
  
  invalidas <- !is.valid(vect(cicatrices_shp))
if (any(invalidas)) {
  cat("Hay geometrías inválidas. Corrigiendo...\n")
  cicatrices_shp_valid <- makeValid(vect(cicatrices_shp))
  #  layer_vector <- buffer(layer_vector, width = 0) # Reparar geometrías
}

  ## Verificar validez de geometrias
  #table(st_is_valid(st_as_sf(cicatrices_shp_valid)))
  
  # library(s2)
  # shp_s2 <- shp_valid %>% 
  #   mutate(geometry = geometry %>% s2_rebuild() %>% st_as_sfc())
  
  
  # Proyectar a  UTM zona adecuada o proyección adecuada
  #Opción 1: MAGNA-SIRGAS Origen-Nacional (EPSG:9377)
  # Cobertura: todo el territorio colombiano, incluidos departamentos amazónicos, 
  # con límites WGS84 de –84.77° a –66.87° longitud y –4.23° a 15.51° latitud
  # https://epsg.io/9377?
  
  
  cicatrices_shp_proj <- st_transform(st_as_sf(cicatrices_shp_valid), crs = epsg)  
  
  cicatrices_shp_proj
  
}



cq_valid_to_proc <- map(cicatrices_list, valida_reproj_cq)



# In group 1: `id_ciq = 0`.
# Caused by error:
#   ! TopologyException: side location conflict at 4541659.2488450399 1654306.6353721665. 
# This can occur if the input geometry is invalid.


## Cicatrices quema ----

cq_to_raster <- map(cq_valid_to_proc, ~ .x %>%
                      group_by(id_ciq) %>%
                      summarise(geometry = st_union(geometry),,
                                area_ha_ci = sum(area_ha_ci), .groups="drop"))


dir.create("output/vector")
map2(cq_to_raster, names(cq_to_raster), ~write_sf(.x, paste0("output/vector/cicatrices_quema_", .y, ".gpkg")))



## Campos a rasterizar
cicatrices_quema <- cicatrices_shp_proj %>%
#  filter(id_ciq != 0) %>%
  group_by(id_ciq) %>%
  summarise(geometry = st_union(geometry),
            area_ha_ci = sum(area_ha_ci), .groups="drop")

## Verificar longitud
# cicatrices_quema
# length(unique(cicatrices_shp_proj$id_ciq))

# sf_use_s2(TRUE)  
# quemas2 <- shp_valid %>%
#   filter(id_ciq != 0) %>%
#   group_by(id_ciq) %>%
#   summarise(geometry = s2::s2_union(geometry), .groups="drop")


# Volver a proyectar a CRS origen SIRGAS 1995
# cicatrices_quema_raster2 <- project(cicatrices_quema_raster, crs(cicatrices_shp))
# plot(cicatrices_quema_raster2)
# lines(departamentos_sinchi)
# writeRaster(cicatrices_quema_raster2, filename = "output/cicatriz_quema_SIRGAS1995.tif")

# > area_shape_origen (has)
# [1] 33361.25

# area_ha 9004598

# > area_raster_caqueta
# [1] 33354.81


## Coberturas Bosque No Bosque ----

coberturas_to_raster <- map(cq_valid_to_proc, ~.x %>%
                              group_by(id, tipo_cober) %>%
                              summarise(geometry = st_union(geometry), .groups="drop") %>%
                              mutate(cobertura_code = fct_recode(tipo_cober,
                                                                 BE = "Bosque Estable",
                                                                 NI = "Sin Información",
                                                                 NB = "No Bosque Estable",
                                                                 DF = "Deforestación",
                                                                 RG = "Regeneración")) %>%
                              mutate(code_num = case_when(
                                tipo_cober == "Bosque Estable"    ~ 1,
                                tipo_cober == "Deforestación"     ~ 2,
                                tipo_cober == "No Bosque Estable" ~ 3,
                                tipo_cober == "Sin Información"   ~ 4,
                                tipo_cober == "Regeneración"      ~ 5
                              )))

map2(coberturas_to_raster, names(coberturas_to_raster), ~write_sf(.x, paste0("output/vector/coberturas_bnb_", .y, ".gpkg")))



coberturas_sinchi <- cicatrices_shp_proj %>%
  group_by(tipo_cober, cobertura_code) %>%
  summarise(geometry = st_union(geometry), .groups="drop") %>%
  mutate(cobertura_code = fct_recode(tipo_cober,
                                     BE = "Bosque Estable",
                                     NI = "Sin Información",
                                     NB = "No Bosque Estable",
                                     DF = "Deforestación",
                                     RG = "Regeneración"))



shp_deforestacion <- cicatrices_shp_proj %>%
  filter(tipo_cober == "Deforestación") %>% 
  group_by(id) %>%
  summarise(geometry = st_union(geometry), .groups="drop")




cicatrices_shp_proj %>% group_by(departamen, tipo_cober) %>%
  summarise(area_ha_ci_tot = sum(area_ha_ci))


