# Species Distribution Modelling
## Preprocessing ocurrence data VIF and Background
## github.com/jrodriguez88
## Febrero 2025

# library(tidymodels)
# library(car)


# ## Prepare data
# predictors <- raster::stack(all_predictors)
# species_dta <- data_cleaned
# species_dta <- species_dta %>% 
#   terra::as.data.frame() %>% 
#   dplyr::select(lon, lat) 
# 
# ## Valores de presencia
# presvals <- extract(predictors, species_dta)
# #presvals <- presvals[,-1] 
# 
# 
# 
# n_points <- nrow(data_cleaned)
# set.seed(2024)
# backgr <- randomPoints(predictors, n_points)
# absvals <- extract(predictors, backgr)
# pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
# sdm_data <- data.frame(cbind(pb, rbind(presvals, absvals)))


# # Tidymodels preprare recipe
#   prep_recipe_all <- 
#     recipe( ~ ., data = sdm_data)  %>%
#     update_role(pb, new_role = "outcome")  %>% 
#     step_filter_missing(all_predictors(), threshold = 0.2) %>% 
#     step_normalize(all_numeric_predictors()) %>%
#     step_nzv(all_numeric()) %>%
#     
#     step_corr(all_numeric_predictors(), threshold = corr_threshold, method = "spearman") %>% 
#     
#     # Impute missing values using the knn imputation algorithm
#     step_impute_median(all_numeric_predictors()) %>% 
#     
#     # Create dummy variables for all nominal predictors
#     #  step_dummy(all_nominal_predictors()) %>%
#     
#     step_nzv(all_numeric(), ) %>%
#     prep()
#   
#   
#   #
#   
#   # Apply the recipe to the data
#   filtered_test_data <- 
#     prep_recipe_all %>% 
#     bake(new_data = NULL)
  
  # Visualize missing data
#  sdm_data %>% vis_miss()
  # filtered_test_data %>% vis_miss()
  
  
  # tidy(prep_recipe_all, number = 4)
  

  
#   ## VIF filter
# 
#   m1 <- lm(pb ~ ., data = filtered_test_data)
#   #m2 <- lm(pb ~ ., data = sdm_data)
#   
#   summary(m1)
#   
#   vif(m1)
#   
#   
# pred_select <- which(vif(m1) < vif_threshold) %>% names




# vif(sdm_data) # calculates vif for the variables in r

# v1 <- vifcor(sdm_data, th=0.9) # identify collinear variables that should be excluded
# 
# v1




##### background

# Set seed for reproducibility
set.seed(123)

# 1. Load data ------------------------------------------------------------
species_points <- points_cleaned
predictors <- all_predictors#[[pred_select]]
#writeRaster(predictors, filename = paste0(output_specie, tag_spe, "_model_predictors_base.tif"))


# 2. Prepare data ---------------------------------------------------------
# 2. Prepare data ---------------------------------------------------------
# Extraer valores para puntos de presencia

threshold_distance <- buffer_presence  # 20 km

# Buffer the presence points to exclude nearby areas
presence_buffer <- terra::buffer(species_points, width = threshold_distance)

# Remove buffered areas from the study area to define the background area
background_area <- terra::erase( w1 %>% crop(ext_sel), presence_buffer)
# plot(background_area, color = "blue")

# --- Step 3: Generate Candidate Pseudo-absence Points ---
# We want to generate pseudo-absence points using the "2°far" method.
# The recommendation here is to use a number equal to the number of presences (i.e. 49).
# To ensure we get enough non-NA points, we first sample more candidates and then randomly select.
# num_PA <- nrow(terra::extract(predictors, species_points, ID = FALSE)) 
num_PA <- nrow(thinned_df)  


# Sample candidate points from the background area
# Here we sample 5 times as many candidate points as needed.
candidate_PA <- spatSample(background_area, size = num_PA * pseudo_ausencia_mult, 
                           method = "random")

# Extract environmental values at candidate pseudo-absence locations
candidate_PA_values <- terra::extract(predictors, candidate_PA, ID = FALSE, xy = TRUE)
candidate_PA_values <- na.omit(candidate_PA_values)
background_values <- candidate_PA_values

# From the candidate pool, randomly select the desired number of pseudo-absence points (49)
# set.seed(123)  # for reproducibility
# if(nrow(candidate_PA_values) >= num_PA) {
#   selected_indices <- sample(1:nrow(candidate_PA_values), num_PA)
#   background_values <- candidate_PA_values[selected_indices, ]
# } else {
  # background_values <- candidate_PA_values
# }

# --- Step 4: Create the Response Data ---
# Extract environmental values at the presence locations (if not already done)
presence_values <- terra::extract(predictors, species_points, ID = FALSE, xy = TRUE)
presence_values <- na.omit(presence_values)

# Create data frames for presences (coded as 1) and pseudo-absences (coded as 0)
presence_data <- data.frame(presence = 1, presence_values)
background_data <- data.frame(presence = 0, background_values)

# Combine the two datasets
full_data <- rbind(presence_data, background_data)
full_data <- na.omit(full_data)


# Convert SpatVector objects to sf objects for plotting
species_sf <- st_as_sf(species_points)
background_sf <- st_as_sf(candidate_PA)  # candidate_PA includes more points; selected ones can be highlighted later
area_sf <- st_as_sf(w1 %>% crop(ext_sel))
presence_buffer_sf <- st_as_sf(presence_buffer)
# Create a base map with the Tanzania boundary
background_plot <- ggplot() +
  geom_sf(data = area_sf, fill = "lightgray", color = "black") +
  # Plot presence points in red
  geom_sf(data=presence_buffer_sf,color="yellow",size=2)+
  geom_sf(data = species_sf, color = "red", size = 2) +
  
  # Plot candidate pseudo-absence points in blue
  geom_sf(data = st_as_sf(background_sf), color = "blue", size = 1, alpha = 0.5) +
  ggtitle("Presence Points (red) and Pseudo-Absences (blue)") +
  theme_minimal()

ggsave(filename = paste0(output_specie, tag_spe, "_Background_plot.jpg"), 
       plot = background_plot, height = 7, width = 10)


  
to_cor_step <- dplyr::select(full_data, -c(presence, x, y)) %>% scale()

# test_km <- kmeans(x = to_cor_step, centers = 5)
# test_km
# 
# test_2 <- k_means(all_predictors, centers = 5)
# plot(test_2)


#cor_sdm <- vifcor(to_cor_step, th = corr_threshold)
vif_sdm <- vifstep(to_cor_step, th=vif_threshold) # identify collinear variables that should be excluded

#pred_select <- names(exclude(full_data, cor_sdm))[-1]

pred_select <- names(exclude(dplyr::select(full_data, -c(presence, x, y)), vif_sdm)) #%>% c(., "elevation")

#cor_matrix <- cor(sdmdata, use = "complete.obs", method = "spearman")
cor_matrix <- cor(dplyr::select(full_data, all_of(pred_select)), use = "complete.obs", method = "pearson")
png(paste0(output_specie, tag_spe, "_Pearson_correlacion_predictores.png"),
    width = 12, height = 12, units="in", res=300)
par(mfrow = c(1,1))
corrplot::corrplot.mixed(cor_matrix, main = "\nPearson Correlation")
dev.off()


predictors <- all_predictors[[c(pred_select)]]
# writeRaster(predictors, filename = paste0(output_specie, tag_spe, "_model_predictors_base.tif"), overwrite = T)


# 3. Extraer valores ambientales en los puntos
v <- extract(predictors, points_cleaned)

write_csv(v, paste0(output_specie, tag_spe, "_to_mess.csv" ))
