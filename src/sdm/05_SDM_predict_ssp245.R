## Climate change escenarios 
# options(java.parameters = "-Xmx20g")

# bio_ssp245_predictors <- rast("data/predictors/bioclim_ensemble_raster_ssp245_2040_2060.tif")
# # soil_predictors <- rast("data/predictors/soil_predictors.tiff")
# # terrain_predictors <- rast("data/predictors/terrain_predictors.tiff")
# 
# ssp245_predictors <- c(bio_ssp245_predictors, soil_predictors, terrain_predictors) 
# 
# predictors_ssp245 <- ssp245_predictors[[pred_select]]
# 
# 
# # load("entregable_19_03/Vanilla_/Vanilla__sdm_trained_models.RData")
# 
# # 7. Create predictions ---------------------------------------------------
# # GAM prediction
# gam_pred_ssp245 <- predict(predictors_ssp245, gam_model, type = "response")
# 
# # BRT prediction
# gbm_pred_ssp245 <- predict(predictors_ssp245, gbm_model, n.trees = best_trees, type = "response",na.rm=TRUE)
# 
# # MaxEnt prediction
# maxent_pred_ssp245 <- predict(predictors_ssp245, maxent_model,na.rm=TRUE)
# 
# # Random Forest prediction
# rf_pred_ssp245 <- predict(predictors_ssp245, rf_model, type = "prob", index = 2, na.rm=TRUE)

# # 8. Plot predictions ----------------------------------------------------
# jpeg(paste0(output_specie, tag_spe, "_SDM_prediction_SSP245_Map.jpg"),
#      width = 8.3, height = 11.7, units="in", res=300)
# par(mfrow = c(2, 2))
# plot(gam_pred, main = "GAM Prediction")
# plot(gbm_pred, main = "BRT Prediction")
# plot(maxent_pred, main = "MaxEnt Prediction")
# plot(rf_pred, main = "Random Forest Prediction")
# dev.off()

# 9. Ensemble model ------------------------------------------------------
# Rescale predictions to 0-1 range if necessary

# models <- rast(list(gam_pred, gbm_pred, maxent_pred, rf_pred))
# ensemble_media <- app(ensemble_t, mean)

# eval_results
# names(models) <- c("GAM_Logistic", "BRT_Regression", "Maxent", "RandomForest")
# plot(models, main='average score')


# # Función para re-escalar un SpatRaster a valores entre 0 y 1
# rescaleRaster <- function(r) {
#   mm <- minmax(r)  # mm[1]: mínimo, mm[2]: máximo
#   (r - mm[1]) / (mm[2] - mm[1])
# }
# 
# # Re-escalar cada raster
# gam_pred_ssp245_scaled    <- rescaleRaster(gam_pred_ssp245)
# gbm_pred_ssp245_scaled    <- rescaleRaster(gbm_pred_ssp245)
# maxent_pred_ssp245_scaled <- rescaleRaster(maxent_pred_ssp245)
# rf_pred_ssp245_scaled     <- rescaleRaster(rf_pred_ssp245)
# 
# 
# # Let’s combine three models weighted by their AUC scores. Here, to create the weights,
# # we substract 0.5 (the random expectation) and square the result to give further weight to higher AUC values.
# w <- (eval_results$AUC-0.5)^2
# # ensemble_ssp245 <- weighted.mean(rast(list(gam_pred_ssp245, gbm_pred_ssp245, maxent_pred_ssp245, rf_pred_ssp245)), w)
# ensemble_ssp245 <- weighted.mean(rast(list(gam_pred_ssp245_scaled, gbm_pred_ssp245_scaled, maxent_pred_ssp245_scaled, rf_pred_ssp245_scaled)), w)
# #plot(ensemble, main=paste0('Weighted mean of four models - ', tag_spe))
# ensemble_ssp245_scaled     <- rescaleRaster(ensemble_ssp245)
# # plot(ensemble_ssp245_scaled)

# ensemble <- (gam_pred + gbm_pred + maxent_pred + rf_pred) / 4
# writeRaster(gam_pred_ssp245, paste0(output_specie, tag_spe, "_", "GAM_ssp245.tif"), overwrite=TRUE)
# writeRaster(gbm_pred_ssp245,paste0(output_specie, tag_spe, "_", "Boosted_Regression_Trees_ssp245.tif"), overwrite=TRUE)
# writeRaster(maxent_pred_ssp245,paste0(output_specie, tag_spe, "_", "Maxent_ssp245.tif"), overwrite=TRUE)
# writeRaster(rf_pred_ssp245,paste0(output_specie, tag_spe, "_", "RandomForest_ssp245.tif"), overwrite=TRUE)
# writeRaster(ensemble_ssp245_scaled,paste0(output_specie, tag_spe, "_", "ensemble_ssp245_scaled.tif"), overwrite=TRUE)

clusterExport(cl, varlist = c("periodo"))
# 
tasks <- list(
  list(model = "GAM",    output = paste0(output_specie, tag_spe, "_", periodo, "_SSP245_GAM.tif")),
  list(model = "BRT",    output = paste0(output_specie, tag_spe, "_", periodo, "_SSP245_BRT.tif")),
  list(model = "MaxEnt", output = paste0(output_specie, tag_spe, "_", periodo, "_SSP245_MaxEnt.tif")),
  list(model = "RF",     output = paste0(output_specie, tag_spe, "_", periodo, "_SSP245_RF.tif"))
)

# Función que, según la tarea, realiza la predicción y guarda el raster
predict_and_write <- function(task) {
  predictors <- c(rast(paste0(predictors_path, "bioclim_ensemble_raster_ssp245_", periodo, ".tif")),
                  rast(paste0(predictors_path, "soil_predictors.tiff")),
                  rast(paste0(predictors_path, "terrain_predictors.tiff")))
  
  if(is.null(region_crop)) {
    
  } else {
    region_crop = vect(paste0(predictors_path, "region_crop.gpkg"))
    study_area <- region_crop
    predictors <- crop(predictors, study_area, mask = TRUE)
    
  }
  
  if (task$model == "GAM") {
    pred <- predict(predictors, gam_model, type = "response")
  } else if (task$model == "BRT") {
    pred <- predict(predictors, gbm_model, n.trees = best_trees, type = "response", na.rm = TRUE)
  } else if (task$model == "MaxEnt") {
    pred <- predict(predictors, maxent_model, na.rm = TRUE)
  } else if (task$model == "RF") {
    pred <- predict(predictors, rf_model, type = "prob", index = 2, na.rm = TRUE)
  }
  
  writeRaster(pred, task$output, overwrite = TRUE)
  return(task$model)
}

# Ejecutar las tareas en paralelo
results_ssp245 <- parLapply(cl, tasks, predict_and_write)

# 7. Create predictions ---------------------------------------------------
# GAM prediction
gam_pred_ssp245 <- rast(tasks[[1]]$output)

# BRT prediction
gbm_pred_ssp245 <- rast(tasks[[2]]$output)

# MaxEnt prediction
maxent_pred_ssp245 <- rast(tasks[[3]]$output)

# Random Forest prediction
rf_pred_ssp245 <- rast(tasks[[4]]$output)


message(paste0("Prediccion de escenario SSP245 completa. Periodo: ", periodo))

