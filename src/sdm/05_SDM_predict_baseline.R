
# 
# predictors <- rast(paste0(output_specie, tag_spe, "_model_predictors_base.tif"))
# 
# 
# # 7. Create predictions Linea base-----------------------------------------------
# # GAM prediction
# gam_pred <- predict(predictors, gam_model, type = "response")
# 
# # BRT prediction
# gbm_pred <- predict(predictors, gbm_model, n.trees = best_trees, type = "response", na.rm=TRUE)
# 
# # MaxEnt prediction
# maxent_pred <- predict(predictors, maxent_model,na.rm=TRUE)
# 
# # Random Forest prediction
# rf_pred <- predict(predictors, rf_model, type = "prob", index = 2, na.rm=TRUE)



# Número de workers: 4
n_workers <- 4
cl <- makeCluster(n_workers)

# Cargar 'terra' en cada worker
clusterEvalQ(cl, {
  options(java.parameters = "-Xmx8g")
  library(terra)
  library(gbm)
  library(randomForest)
  library(dismo)
  library(mgcv)
  library(rJava)
  
})

# Cargar el raster y exportarlo junto con los modelos y parámetros a cada worker
# predictors <- rast(paste0(output_specie, tag_spe, "_model_predictors_base.tif"))
clusterExport(cl, varlist = c("gam_model", "gbm_model", "maxent_model", "rf_model", "best_trees", "output_specie", "tag_spe", "predictors_path", "region_crop"))

# Lista de tareas: cada elemento indica el modelo y el nombre de salida
tasks <- list(
  list(model = "GAM",    output = paste0(output_specie, tag_spe, "_", "Baseline_GAM.tif")),
  list(model = "BRT",    output = paste0(output_specie, tag_spe, "_", "Baseline_BRT.tif")),
  list(model = "MaxEnt", output = paste0(output_specie, tag_spe, "_", "Baseline_MaxEnt.tif")),
  list(model = "RF",     output = paste0(output_specie, tag_spe, "_", "Baseline_RF.tif"))
)

# Función que, según la tarea, realiza la predicción y guarda el raster
predict_and_write <- function(task) {
  predictors <- c(rast(paste0(predictors_path, "bioclim_predictors.tiff")),
                  rast(paste0(predictors_path, "soil_predictors.tiff")),
                  rast(paste0(predictors_path, "terrain_predictors.tiff")))
  predictor_names <- gsub("wc2.1_30s_", "", names(predictors))
  
  predictors <- setNames(predictors, predictor_names)
  
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
results_base <- parLapply(cl, tasks, predict_and_write)
# maxent_pred2 <- rast(tasks[[3]]$output)

# 7. Create predictions ---------------------------------------------------
# GAM prediction
gam_pred <- rast(tasks[[1]]$output)

# BRT prediction
gbm_pred <- rast(tasks[[2]]$output)

# MaxEnt prediction
maxent_pred <- rast(tasks[[3]]$output)

# Random Forest prediction
rf_pred <- rast(tasks[[4]]$output)



# 8. Plot predictions ----------------------------------------------------
if(is.null(region_crop)) {
  jpeg(paste0(output_specie, tag_spe, "_SDM_prediction_Map.jpg"),
       width = 8.3, height = 11.7, units="in", res=300)
  par(mfrow = c(2, 2))
  plot(gam_pred, main = "GAM Prediction")
  plot(study_area, add=TRUE, fill=NA)
  plot(gbm_pred, main = "BRT Prediction")
  plot(study_area, add=TRUE, fill=NA)
  plot(maxent_pred, main = "MaxEnt Prediction")
  plot(study_area, add=TRUE, fill=NA)
  plot(rf_pred, main = "Random Forest Prediction")
  plot(study_area, add=TRUE, fill=NA)
  dev.off()
  
} else {
  
  study_area <- region_crop

  jpeg(paste0(output_specie, tag_spe, "_SDM_prediction_Map.jpg"),
       width = 8.3, height = 11.7, units="in", res=300)
  par(mfrow = c(2, 2))
  plot(crop(gam_pred, st_geometry(study_area), mask = TRUE), main = "GAM Prediction")
  plot(st_geometry(region_crop), add=TRUE, fill=NA)
  plot(crop(gbm_pred, st_geometry(study_area), mask = TRUE), main = "BRT Prediction")
  plot(st_geometry(region_crop), add=TRUE, fill=NA)
  plot(crop(maxent_pred, st_geometry(study_area), mask = TRUE), main = "MaxEnt Prediction")
  plot(st_geometry(region_crop), add=TRUE, fill=NA)
  plot(crop(rf_pred, st_geometry(study_area), mask = TRUE), main = "Random Forest Prediction")
  plot(st_geometry(region_crop), add=TRUE, fill=NA)
  dev.off()
  
  
}

message("Prediccion de Linea Base completa")

# 9. Ensemble model ------------------------------------------------------
# Rescale predictions to 0-1 range if necessary

# models <- rast(list(gam_pred, gbm_pred, maxent_pred, rf_pred))
# ensemble_media <- app(ensemble_t, mean)

# eval_results
# names(models) <- c("GAM_Logistic", "BRT_Regression", "Maxent", "RandomForest")
# plot(models, main='average score')

# Let’s combine three models weighted by their AUC scores. Here, to create the weights,
# we substract 0.5 (the random expectation) and square the result to give further weight to higher AUC values.
# w <- (eval_results$AUC-0.5)^2
# ensemble <- weighted.mean(rast(list(gam_pred, gbm_pred, maxent_pred, rf_pred)), w)
#plot(ensemble, main=paste0('Weighted mean of four models - ', tag_spe))

# # Función para re-escalar un SpatRaster a valores entre 0 y 1
# rescaleRaster <- function(r) {
#   mm <- minmax(r)  # mm[1]: mínimo, mm[2]: máximo
#   (r - mm[1]) / (mm[2] - mm[1])
# }
# 
# # Re-escalar cada raster
# gam_pred_scaled    <- rescaleRaster(gam_pred)
# gbm_pred_scaled    <- rescaleRaster(gbm_pred)
# maxent_pred_scaled <- rescaleRaster(maxent_pred)
# rf_pred_scaled     <- rescaleRaster(rf_pred)



# ensemble <- (gam_pred + gbm_pred + maxent_pred + rf_pred) / 4
# writeRaster(gam_pred, paste0(output_specie, tag_spe, "_", "GAM.tif"), overwrite=TRUE)
# writeRaster(gbm_pred,paste0(output_specie, tag_spe, "_", "Boosted_Regression_Trees.tif"), overwrite=TRUE)
# writeRaster(maxent_pred,paste0(output_specie, tag_spe, "_", "Maxent.tif"), overwrite=TRUE)
# writeRaster(rf_pred,paste0(output_specie, tag_spe, "_", "RandomForest.tif"), overwrite=TRUE)

# Plot ensemble
# ensemble_corte <- mask(ensemble,study_area)
# binario_ensamble <- ensemble_corte > 0.5
# 
# # Plot the binary raster using white for 0 and darkgreen for 1
# par(mfrow = c(1,2))
# plot(ensemble, main = "Ensemble Model (Mean)")
# plot(binario_ensamble, col = c("white", "darkgreen"), main = "Binary Ensemble Prediction")
# plot(study_area ,add=TRUE, fill=NA)

# Save ensemble raster if needed


# Corte por departamento

# gam_pred<-mask(gam_pred,tanzania)
# gbm_pred<-mask(gbm_pred,tanzania)
# maxent_pred<-mask(maxent_pred,tanzania)
# rf_pred<-mask(rf_pred,tanzania)

# --- Convert Rasters to Data Frames ---
# # Convert each raster to a data frame with x, y coordinates and prediction values
# gam_df    <- as.data.frame(gam_pred, xy = TRUE)
# gbm_df    <- as.data.frame(gbm_pred, xy = TRUE)
# maxent_df <- as.data.frame(maxent_pred, xy = TRUE)
# rf_df     <- as.data.frame(rf_pred, xy = TRUE)
# 
# # Rename the third column to "prediction" (assuming each has one layer)
# names(gam_df)[3] <- "prediction"
# names(gbm_df)[3] <- "prediction"
# names(maxent_df)[3] <- "prediction"
# names(rf_df)[3] <- "prediction"
# 
# # Add a column to indicate the model type
# gam_df$Model    <- "GAM"
# gbm_df$Model    <- "BRT"
# maxent_df$Model <- "MaxEnt"
# rf_df$Model     <- "RandomForest"
# 
# # --- Merge All Data Frames ---
# df_all <- rbind(gam_df, gbm_df, maxent_df, rf_df)

# # --- Define Global Limits for the Color Scale ---
# global_min <- min(df_all$prediction, na.rm = TRUE)
# global_max <- max(df_all$prediction, na.rm = TRUE)

# --- Plot with ggplot2 ---
# p <- ggplot(df_all, aes(x = x, y = y, fill = prediction)) +
#   geom_raster() +
#   # Overlay the tanzania sf object without inheriting global aesthetics
#   geom_sf(data = st_as_sf(study_area), 
#           inherit.aes = FALSE, 
#           mapping = aes(geometry = geometry),
#           fill = NA, color = "black", size = 0.5) +
#   facet_wrap(~ Model, ncol = 2) +
#   scale_fill_viridis_c(option = "viridis", labels = function(x) round(x, 2),
#                        begin = global_min, end = global_max,
#                        name = "Idoneidad", ) +
#   coord_sf() +
#   theme_minimal(base_size = 14) +
#   labs(title = str_to_title(paste0(tag_spe, " - ", "Mapas de Idoneidad"))) +
#   theme(panel.background = element_rect(fill = "white", color = NA),
#         plot.background  = element_rect(fill = "white", color = NA),
#         panel.grid       = element_blank(),
#         axis.title       = element_blank(),
#         axis.text        = element_blank(),
#         axis.ticks       = element_blank(), 
#         legend.position = "bottom", legend.key.width = unit(2, "cm"))


#ggsave(filename = paste0(output_specie, tag_spe, "_Suitability_Map.jpg"), plot = p, width = 8.3, height = 11.7 )

#binario

# --- Optimal Threshold Determination ---
# Use the test set predictions from one model (e.g., MaxEnt) to compute the optimal threshold using the Youden index.
# You could compute this separately per model; here we illustrate a single threshold for simplicity.
#presence_values <- terra::extract(predictors, species_points, ID = FALSE)
# roc_obj <- roc(test_data$presence, predict(maxent_model, test_data[, predictor_names]))
# best_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = FALSE)
# threshold_value <- best_threshold$threshold[1]
# plot(roc_obj)
# Apply the threshold to create a binary variable for each prediction in df_all

# # w2 <- (eval_results$AUC-0.5)^2
# best_threshold <- weighted.mean(eval_results$threshold, w)
# #best_threshold <- mean(eval_results$threshold)
# 
# df_all <- df_all %>% left_join(eval_results %>% dplyr::select(Model, threshold)) %>%
#   mutate(binary = ifelse(prediction >= threshold, 1, 0))
# # df_all$binary <- ifelse(df_all$prediction >= best_threshold, 1, 0)
# 
# # -------------------------------
# # Plotting the Binary Predictions Map
# # -------------------------------

# labs_th <- set_names(paste0(eval_results$Model,
#                             "\nThreshold: ", eval_results$threshold),  
#                      eval_results$Model)
# 
# p2 <- ggplot(df_all, aes(x = x, y = y, fill = factor(binary))) +
#   geom_raster() +
#   geom_sf(data = st_as_sf(study_area), 
#           inherit.aes = FALSE, 
#           mapping = aes(geometry = geometry),
#           fill = NA, color = "black", size = 0.5) +
#   facet_wrap(~ Model, ncol = 2, labeller  = labeller(Model = labs_th)) +
#   scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"),
#                     name = paste("Threshold mean:", round(best_threshold, 2)),
#                     labels = c("Absence", "Presence")) +
#   coord_sf() +
#   theme_minimal(base_size = 14) +
#   labs(title = str_to_title(paste0(tag_spe, " - ", "Mapas Binarios"))) +
#   theme(panel.background = element_rect(fill = "white", color = NA),
#         plot.background  = element_rect(fill = "white", color = NA),
#         panel.grid       = element_blank(),
#         axis.title       = element_blank(),
#         axis.text        = element_blank(),
#         axis.ticks       = element_blank(), 
#         legend.position = "bottom")
# 
# ggsave(filename = paste0(output_specie, tag_spe, "_Binary_Map.jpg"), plot = p2, width = 8, height = 11)


# plot_final



# # Calculate evaluation metrics
# pred_ensemble <- terra::extract(as.numeric(binario_ensamble_f), 
#                        data.frame(x= test_data$x, y = test_data$y), xy = T) %>%
#   dplyr::select(x, y, sim = sum) %>% mutate(obs = test_data$presence)
# 
# roc_obj <- roc(pred_ensemble$obs, pred_ensemble$sim)
# auc_val <- auc(roc_obj)
# 
# #pred_class <- factor(pred_class, levels = c(1,0))
# #test_df$presence <- factor(test_df$presence, levels = c(1,0))
# conf_matrix_ens <- confusionMatrix(factor(pred_ensemble$sim), factor(pred_ensemble$obs), positive = "1")
# plot(roc_obj, main = paste0("Ensemble model", " - AUC:", round(auc_val, 3)))

