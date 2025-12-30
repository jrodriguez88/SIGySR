## Funciones Sinchi 2025
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## mayo 2025




# layer <- shp2_
# layer_name <- "Tipo_cobertura" 
# out_path <- "data/"
# Convierte vector layer to raster
rasterize_sinchi <- function(layer, layer_name, field_to_rast = "codigo", resol = 100, out_path = NULL) {
  
  
  tag <- layer_name 
  
  # #Extraer y procesar los códigos
  
  layer_vector <- layer %>%
    #   rename_with(tolower) %>% 
    vect()
  
  # tag level
  
  # Revisar geometrías inválidas
  invalidas <- !is.valid(layer_vector)
  if (any(invalidas)) {
    cat("Hay geometrías inválidas. Corrigiendo...\n")
    layer_vector <- makeValid(layer_vector)
  #  layer_vector <- buffer(layer_vector, width = 0) # Reparar geometrías
  }
  
  # Verificar si las geometrías están vacías
  # if (nrow(layer_vector) == 0) {
  #   stop("El shapefile no contiene geometrías válidas después de la corrección.")
  # }
  
  # Verificar el CRS del shapefile
 # print(crs(layer_vector))
  
  # Crear el raster de plantilla con el CRS del shapefile
  template_raster <- rast(
    extent = ext(layer_vector),  # Extensión del shapefile
    resolution = resol, #c(resol, resol),           # Ajustar resolución
    crs = crs(layer_vector)     # Usar el CRS del shapefile
  )
  
  
  
  # Verificar valores únicos ategorias CLC
#  print(unique(layer_vector[[field_to_rast]][[1]]))
  
  # Remover geometrías con valores NA
  layer_vector <- layer_vector[!is.na(layer_vector[[field_to_rast]][[1]]), ]
  if (nrow(layer_vector) == 0) {
    stop("No quedan geometrías válidas después de remover valores NA.")
  }
  
  # Rasterizar
  
  raster_sinchi <- rasterize(layer_vector, template_raster, field = field_to_rast)
  
  #raster_sincgi_wgs84 <- project(raster_sinchi, "EPSG:4326")
  
  
  if(!is.null(out_path)){
    
    writeRaster(raster_sinchi, filename = paste0(out_path, tag,"_", field_to_rast, ".tif"),  overwrite = TRUE)
    #writeRaster(raster_sinchi_wgs84, filename = paste0(out_path, tag,"_", nivel_clc, "_wgs84.tif"),  overwrite = TRUE)
  }
  
  
  
  # Verifisinchi si el raster tiene datos
  # if (is.na(minmax(raster_sinchi)[1])) {
  #   stop("El raster no contiene valores. Revisa la superposición entre las geometrías y el raster.")
  # }
  
  
  # Plot para verificar
  #plot(raster_cobertura, main = "Coberturas CLC")
  # Extraer los códigos y leyendas únicos
  
  raster_sinchi 
  
}
