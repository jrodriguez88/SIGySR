# Species Distribution Modelling
# 02 _ Download occurrencies data, cleaning and sampling
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025


## Especies ----

# # Caqueta
# cacao <- c("theobroma", "cacao")
# canangucha <- c("mauritia", "flexuosa")
# copoazu <- c("theobroma", "grandiflorum")
# 
# # Choco
# coco <- c("cocos", "nucifera")
# vainilla <- c("vanilla", "planifolia")

# c("Vanilla", "rivasii")
# c("Vanilla", "cribbiana")
# c("Vanilla", "trigonocarpa")
# c("Vanilla", "dresslerii")

file_gbif <- paste0(output_specie, tag_spe, "datos_GBIF_raw.gpkg")

if(file.exists(file_gbif)){
  
  datos_gbif_crop <- st_read(file_gbif)
  
  print(paste(nrow(datos_gbif_crop), "Datos de ocurrencias de ocurrencias descargados para", tag_spe, "guardados en", "output folder"))
  
  
} else {
  
  ## Descarga de datos GBIF ----
  if(is.list(especie)) {
    
    datos_gbif_crop <- map(especie, ~sp_occurrence(.x[1], species=.x[2], ext = ext_sel)) %>% 
      bind_rows()
    
    print(paste(nrow(datos_gbif_crop), "Datos de ocurrencias de ocurrencias descargados para", tag_spe, "guardados en", "output folder"))
    
    
  } else { datos_gbif_crop <- sp_occurrence(especie[1], species=especie[2], ext = ext_sel)}
  
  #write_csv(datos_gbif_crop, file = "data/descarga_gbif.csv")
  
  datos_gbif_crop <- vect(datos_gbif_crop, geom=c("lon", "lat"), 
                          crs=crs(all_predictors), keepgeom = TRUE) %>% st_as_sf()
  
  st_write(datos_gbif_crop, file_gbif, append=TRUE)
  
  
  
  
}


#Define Tag by specie
tag_esp <- paste(especie, collapse = " ") %>% str_to_title()

par(mfrow = c(1,1))
plot(terrain_predictors[["elevation"]],
     main = paste0("Ocurrencia Especies GBIF - ",
                   tag_esp), col = map.pal("elevation"))
lines(study_area)
points(datos_gbif_crop, col='red', cex= 1)
legend("bottom", #x = -73.5, y = 3, # Coordinates
       legend = tag_esp,
       col = c('red'))

plot(study_area)
plot(datos_gbif_crop, col='red', cex= 1, add = T)
plot(datos_gbif_crop %>% dplyr::select(contains("cultivated")), add = T )


## Data cleaning ----

# # searching for records with longitudes of zero.
# lonzero <-  subset(datos_gbif_crop %>% st_as_sf(), lon==0)
# 
# if(nrow(lonzero) > 0) {message("Hay latitudes cero en los datos")}
# 
# 
# # which records are duplicates (only for the first 10 columns)?
# dups <- duplicated(lonzero)
# 
# if(sum(dups) > 0) {message("Existen datos duplicados")}
# 
# # differentiating by (sub) species
# # dups2 <- duplicated(acgeo[, c('species', 'lon', 'lat')])
# # ignoring (sub) species and other naming variation
# dups2 <- duplicated(datos_gbif_crop[, c('species', 'lon', 'lat')])
# # number of duplicates
# 
# if(sum(dups2) > 0) {message(paste0("Existen ", sum(dups2), " datos duplicados"))}
# 
# # keep the records that are _not_ duplicated
# data_cleaned <- datos_gbif_crop[!dups, ] 
# data_cleaned <- datos_gbif_crop[!dups2, ] 

data_cleaned1 <- CoordinateCleaner::clean_coordinates(
  x = sf::st_drop_geometry(datos_gbif_crop) %>% drop_na(lat, lon),
  species = "species",
  lon = "lon",
  lat = "lat",
  outliers_mtp = 2,
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon
  )) %>%
  tibble::as_tibble() %>% filter( .summary == 'TRUE') %>%
  dplyr::select(species, lon, lat, year)


#  Correcci?n de sesgo: Thinning espacial
# Para utilizar spThin se requiere que los datos tengan una columna con el nombre de la especie.
# Si s?lo hay un ?nico tax?n, se puede agregar una columna constante.

# Establecer semilla para reproducibilidad
set.seed(42)

# Aplicar thinning con una distancia de 1 km (puedes ajustar thin.par)
thin_out <- thin(loc.data = data_cleaned1, 
                 lat.col = "lat", 
                 long.col = "lon", 
                 spec.col = "species", 
                 thin.par = 1,      # distancia en km
                 reps = 5, 
                 locs.thinned.list.return = TRUE,
                 write.files = FALSE,
                 out.dir = tempdir())

max_log <- which.max(map_int(thin_out, nrow))

# 'thin_out' es una lista; obtenemos la primera (y en este caso, ?nica) r?plica
thinned_df <- dplyr::distinct(thin_out[[max_log]])

# Para unir los resultados del thinning con el resto de la informaci?n,
# se recomienda hacer un join aproximado (por ejemplo, con un peque?o margen) 
# en lugar de filtrar exactamente por coordenadas. Aqu? usamos inner_join con redondeo.
# thinned_df_reduced <- thinned_df %>%
#   mutate(Long_round = round(Longitude, 4),
#          Lat_round = round(Latitude, 4)) %>%
#   dplyr::select(Long_round, Lat_round)
# 
# data_cleaned <- data_cleaned1 %>%
#   mutate(Long_round = round(lon, 4),
#          Lat_round = round(lat, 4)) %>%
#   inner_join(thinned_df_reduced, by = c("Long_round", "Lat_round"))
# 
# names(data_cleaned)
# 8. Visualizar los puntos limpios sobre el mapa de Tanzania
points_cleaned <- vect(thinned_df, geom = c("Longitude", "Latitude"), 
                       crs = "EPSG:4326")

par(mfrow = c(1,1))

png(paste0(output_specie, tag_spe, "_GBIF_Mapa_Ocurrencias.png"),
    width = 11.7, height = 8.3, units="in", res=300)
plot(terrain_predictors[["elevation"]], 
     main = paste0("Ocurrencia Especies GBIF - Cleaned - ", 
                   tag_spe), col = map.pal("elevation"))
lines(study_area)
plot(vect(data_cleaned1, geom = c("lon", "lat"), 
          crs = "EPSG:4326"), add = TRUE, col = "red")
plot(points_cleaned, add = TRUE, col = "blue", pch = 20)
dev.off()

print(paste(nrow(thinned_df), "Datos de ocurrencias de GBIF", tag_spe, "guardados en", "output folder"))
write_csv(thinned_df, paste0(output_specie, tag_spe, "_", "gbif_cleaned_data.csv"))










