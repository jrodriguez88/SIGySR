# Run Main Modelacion SINCHI
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025

# Load dependencies

#install.packages(c('tidyverse', 'terra', 'dismo', 'rJava', 'geodata', "tidymodels", "car", "naniar","randomForest", "corrplot"))
options(java.parameters = "-Xmx16g")

library(tidyverse)
library(terra)
library(raster)
library(dismo)
library(usdm)
library(rJava)
library(geodata)
# library(tidymodels)
library(randomForest)
library(gbm)
library(mgcv)
library(caret)
library(pROC)
library(car)
library(sf)
# library(naniar)
library(corrplot)
library(spThin)
library(CoordinateCleaner)
library(blockCV)
library(viridis)
#library(biooracler)
# library(exactextractr)
library(parallel)
library(gridBase)
library(grid)


## Limites Territoriales----
w1 <- geodata::world(path=tempdir())
w2 <- gadm(country = "COL", path=tempdir())
limites_sel <- w1[w1$GID_0 == "COL"]
ext_sel <- ext(limites_sel)

#https://daac.ornl.gov/LBA/guides/LC15_SRTM_Topography.html
# https://forobs.jrc.ec.europa.eu/amazon
amazonia_extent <- ext(-82,  -42,  -20,   10)
ext_sel <- amazonia_extent

study_area <- w1 %>% crop(ext_sel)
plot(study_area)
predictors_path <- paste0(getwd(), "/data/predictors/")
sim_folder <- "outputs19/"

descargar_predictores <- FALSE
if(isTRUE(descargar_predictores)){
  dir.create("data/predictors/")
  source("01_Download_SDM_predictors.R")
} else {
  bioclim_predictors <- rast("data/predictors/bioclim_predictors.tiff")
  soil_predictors <- rast("data/predictors/soil_predictors.tiff")
  terrain_predictors <- rast("data/predictors/terrain_predictors.tiff")
  
  all_predictors <- c(bioclim_predictors, soil_predictors, terrain_predictors)
  predictor_names <- names(all_predictors) %>% str_remove_all("wc2.1_30s_")
  all_predictors <- setNames(all_predictors, predictor_names)
  }


#all_predictors <- all_predictors[[setdiff(names(all_predictors), c("phh2o",     "bdod",      "ocd", "hillshade", "silt"))]]
#all_predictors <- all_predictors[[1:19]]

## Especies ----
# Amazonia

# Encadenamientos productivos 
cacao <- c("theobroma", "cacao")
copoazu <- c("theobroma", "grandiflorum")


# Palmas
canangucha <- c("mauritia", "flexuosa")
asai <- c("euterpe", "precatoria") ## Acai

milpesos <- c("oenocarpus", "bataua") #*


# Forestales
cedro <- c("cedrela", "odorata") 
sangretoro <- c("virola", "elongata") 
cuyubi <- c("minquartia", "guianensis")


milpo <- c("erisma", "uncinatum")


inchi <- c("caryodendron", "orinocense")
cachicamo <- c("aspidosperma", "desmanthum") 
guayacán_rosado <- c("tabebuia", "rosea") 
abarco <- c("cariniana", "pyriformis")



# brachiaria1 <- c("brachiaria", "brizantha")
# brachiaria2 <- c("urochloa", "brizantha")
# brachiaria3 <- c("brachiaria", "decumbens")
# brachiaria4 <- c("urochloa", "decumbens")
# brachiaria5 <- c("brachiaria", "humidicola")
# brachiaria6 <- c("urochloa", "humidicola")

# brachiaria <- list(brachiaria1, brachiaria2, brachiaria3, brachiaria4, brachiaria5, brachiaria6)


## Modeling
set.seed(2024)

especies_list <- list(copoazu, cacao, canangucha, milpesos, asai, cedro, cuyubi, sangretoro) #, guayacán_rosado, abarco, inchi, cachicamo, )

# especies_list <- list(cacao, canangucha, copoazu, milpo, cuyubi, inchi, cachicamo, cedro, 
#                       sangretoro, guayacán_rosado, abarco, milpesos, chonta)

# especies_list <- list(cuyubi, milpo, inchi, cachicamo, cedro, 
#                       sangretoro, guayacán_rosado, abarco, milpesos, chonta, brachiaria)
# especies_list <- list(coco, vainilla)
# especies_list <- list(cuyubi, sangretoro)
# especie = c("urochloa", "")
# especies_list <- list(milpesos, asai)

# Global args
corr_threshold = 0.9
vif_threshold = 10
buffer_presence <- 20000
pseudo_ausencia_mult <- 3 # Factor de seudoausencias multiplica el numero de presencias
path <- getwd()
#especie = 1

region_crop <- read_sf(paste0(predictors_path, "region_crop.gpkg")) #%>% st_transform(crs = 4326) %>% vect()
#region_crop = NULL
sim_folder <- "Amazon_All_SDM_BioclimaticSoil/"

# region_crop =  limites_choco
# writeVector(region_crop, filename = paste0(predictors_path, "region_crop.gpkg"), overwrite=TRUE)


tictoc::tic()
for (especie in seq_along(especies_list)){
  
  tryCatch({
    especie <- especies_list[[especie]]
    
    output_dir <- file.path(path, sim_folder)
    dir.create(output_dir, showWarnings = FALSE)
    
    tag_spe <- paste0(str_to_title(especie[1]), "_", str_to_title(especie[2]))
    output_specie <- file.path(output_dir, tag_spe, "/")
    dir.create(output_specie, showWarnings = FALSE)
    
    source("02_Ocurrence_data.R") # Descarga y limpieza
    
    source("03_VIF_and_background.R")  # VIF and Background
    source("04_SDM_training_eval.R")
   # # source("04_SDM_cross_validation.R")
   #  source("05_SDM_predict_baseline.R")
   #  source("06_SDM_ensemble_baseline.R")
   #  
   #  periodos <- c("2021_2040" , "2041_2060", "2061_2080", "2081_2100")
   #  
   #  for (e in periodos) {
   #    
   #    periodo <- e
   #    
   #    source("05_SDM_predict_ssp245.R")
   #    source("05_SDM_predict_ssp585.R")
   #    source("06_SDM_ensemble_futuro.R")
   #    source("06_Delta_maps.R")
   #    
      
    # }
    

    
    # closeAllConnections()
    # gc()
  },
  error = function(e){
    message(paste0("Error procesando especie ", tag_spe, ": ", e$message))
    closeAllConnections()
    gc()
  })
  
}
tictoc::toc()

# Analisis adicionales
especies_folder <- basename(list.dirs(sim_folder)[-1] %>% str_subset(pattern = "maxent", negate = TRUE))


## MESS Analysis
source("MESS_analysis.R")
source("04_SDM_cross_validation_plot.R")


## Generar Reporte
  
map(
  especies_folder, ~rmarkdown::render(
    input = "report.Rmd",
    params = list(species_name = .x,
                  folder_name = sim_folder),
    output_file = paste0(sim_folder, "Informe_SDM_", .x, ".html")
  
))





