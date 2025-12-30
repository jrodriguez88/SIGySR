library(terra)
library(furrr)

plan(multisession, workers = 7)

coberturas_raster_list <- future_map2(
  coberturas_to_raster,
  names(coberturas_to_raster),
  ~ rasterize_sinchi(
    .x,
    paste0("coberturas_", .y),
    field_to_rast = "cobertura_code",
    resol = 30,
    out_path = "output/raster/"
  ),
  .options = furrr_options(seed = TRUE)
)

# Opcional: restaurar plan secuencial
plan(sequential)



library(terra)      # vect(), rast(), writeRaster(), terraOptions()
library(sf)         # st_read()
library(purrr)      # map()
library(dplyr)      # %>%
library(foreach)
library(doParallel)


# 1. Listar todos los archivos GPKG que empiecen por "coberturas"
gpkg_files <- list.files(
  path       = "output/vector/",
  pattern    = "^coberturas.*\\.gpkg$",
  full.names = TRUE
)

# 1 hilo por worker
terraOptions(threads = 1)
# para GDAL si hace falta
# En R, deshabilita Persistent Auxiliary Metadata
Sys.setenv(GDAL_PAM_ENABLED = "FALSE")
Sys.setenv(OGR_PAM_ENABLED = "FALSE") 


# 2. Leer cada GPKG como SpatVector y nombrar la lista según el año
coberturas_to_raster <- gpkg_files %>%
  set_names( tools::file_path_sans_ext(basename(.)) ) #%>% 
#  map( ~ st_read(.x) )

# 2.1. Registra el back-end
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)

# 4.1. Exportar la lista de rutas y la función
clusterExport(cl, c("coberturas_to_raster", "rasterize_sinchi"))
clusterEvalQ(cl, {
  library(terra)
  # si usas dplyr dentro de rasterize_sinchi()
  library(dplyr)
})

# 2.2. Ejecuta el bucle paralelo
coberturas_raster_list <- foreach(i = seq_along(coberturas_to_raster),
                                  .packages = c("terra","dplyr")) %dopar% {
                                    nm    <- names(coberturas_to_raster)[i]
                                    layer <- coberturas_to_raster[[i]]
                                    rasterize_sinchi(
                                      layer,
                                      paste0("coberturas_", nm),
                                      field_to_rast = "cobertura_code",
                                      resol = 30,
                                      out_path = "output/raster/"
                                    )
                                  }

# 2.3. Detén el clúster
stopCluster(cl)

