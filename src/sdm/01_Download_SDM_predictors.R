# Species Distribution Modelling
# 01_Download_SDM_predictors
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025




## Limites Territoriales----


# w <- world(path=tempdir())
# w2 <- gadm(country = "COL", path=tempdir())
# 
# limites_sel <- w[w$GID_0 == "COL"]
# ext_sel <- ext(limites_col)

# limites_caqueta <- w2[w2$NAME_1 == "Caquetá"]
# limites_choco <- w2[w2$NAME_1 == "Chocó"]

par(mfrow = c(1,1))
plot(limites_sel, main = paste0("Colombia - ", round(ext_sel, 2)))
# lines(limites_caqueta, col = "darkgreen")
# lines(limites_choco, col = "darkgreen")



# Digital Elevation Model - Terrain
elevation <- geodata::elevation_global(0.5, path=tempdir())
elevation <- crop(elevation, ext_sel)
slope <- terra::terrain(elevation, v="slope", neighbors=8, unit="radians")
aspect <- terra::terrain(elevation, v = "aspect", neighbors=8, unit="radians")
hillshade <- terra::shade(slope, aspect, angle = c(45, 45, 45, 80), 
                   direction = c(225, 270, 315, 135)) %>% Reduce(mean, .)

par(mfrow = c(2,2))
plot(slope*180/pi)
plot(aspect*180/pi)
plot(elevation)
plot(hillshade*100)

terrain_predictors <- rast(list(elevation = elevation, 
                                slope = slope*180/pi, 
                                aspect = aspect*180/pi, 
                                hillshade = hillshade*100))

writeRaster(terrain_predictors, filename = "data/predictors/terrain_predictors.tiff", overwrite = T)


## Predictors - Environmental data ----

# Climate -  WorldClim

bioclim_predictors <- worldclim_global(var="bio", res = 0.5, path=tempdir())
bioclim_predictors <- crop(bioclim_predictors, ext_sel)

writeRaster(bioclim_predictors, filename = "data/predictors/bioclim_predictors.tiff")



# Soil Predictors
# Profundidades
depths <- c(5, 15, 30, 60)#, 100, 200)

# Variables disponibles
vars = c("clay", "sand", "silt", "soc", "phh2o", "bdod", "ocd")

set_soil <- expand_grid(vars, depths)


## descarga 
soilgrids_rasters <- map2(set_soil$vars, set_soil$depths, 
                          ~soil_world(var=.x, depth=.y, path=tempdir()))


crop_safe <- possibly(terra::crop, NULL)

soil_predictors <- set_soil %>% 
  bind_cols(
soilgrids_rasters %>% enframe(name = NULL, value = "global_raster") %>% 
  mutate(crop_raster = map(global_raster, crop_safe, y = ext_sel))) %>%
  dplyr::select(-global_raster)  %>%
  mutate(is_null = map_lgl(crop_raster, is_null)) %>%
  filter(!is_null) %>% dplyr::select(-is_null) %>%
  nest(data = -c(vars)) %>%
  mutate(rast_var = map(data, ~rast(.x$crop_raster)),
         median_rast = map(rast_var, ~app(.x, median, na.rm = T)))
  
soil_predictors <- setNames(rast(soil_predictors$median_rast), soil_predictors$vars)
 
writeRaster(soil_predictors, filename = "data/predictors/soil_predictors.tiff")

# all_predictors <- list(bioclim_predictors, soil_predictors) %>% 
#   map(~project(.x, terrain_predictors[[1]], method = "bilinear")) %>% 
#   rast() %>% c(., terrain_predictors)

all_predictors <- c(bioclim_predictors, soil_predictors, terrain_predictors)

predictor_names <- names(all_predictors) %>% str_remove_all("wc2.1_30s_")
all_predictors <- setNames(all_predictors, predictor_names)

writeRaster(all_predictors, filename = "data/predictors/all_predictors.tiff")
  





