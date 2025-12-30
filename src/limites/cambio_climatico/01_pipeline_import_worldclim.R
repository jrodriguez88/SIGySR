# ============================================================
# Pipeline optimizado (WorldClim baseline + CMIP6 futuros)
# AOI: limites_adm (EPSG:3116) con 2 polígonos (Nombre)
# Salidas: baseline_mean, future_means, deltas, ensemble
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(future)
  library(furrr)
})

# -------------------------------
# Configuración
# -------------------------------

# Ajusta esto si quieres cambiar el uso de cores/workers
n_workers <- 8                 # paralelismo por archivo (Windows: multisession)
terra_cores_sequential <- 14   # para pasos secuenciales (project/crop grandes)

# Hilos para GDAL (reproyección/warping)
terra::setGDALconfig("GDAL_NUM_THREADS", "ALL_CPUS")

# Nombre estándar BIO
bio_names <- paste0("bio", 1:19)

gdb_caso     <- "data/limites_amazonia/CSlim2025_Reg.gdb/CSlim2025_Reg.gdb/"
path_data <- "G:/00_DATA/escenarios/worldclim/"
limites_adm           <- sf::st_read(gdb_caso,      layer = "CSlim2025_Reg")


# -------------------------------
# AOI
# -------------------------------


# Requisito: limites_adm existe en el entorno y está en EPSG:3116
stopifnot(exists("limites_adm"))

# Asegura EPSG:3116
limites_adm <- st_transform(limites_adm, 3116)

# Versión lon/lat para recorte/máscara (rasters WorldClim suelen venir en EPSG:4326)
limites_ll <- st_transform(limites_adm, 4326)

# Para terra
aoi_v_3116 <- terra::vect(limites_adm)
aoi_v_ll   <- terra::vect(limites_ll)

# -------------------------------
# Funciones utilitarias
# -------------------------------

order_baseline_files <- function(files_baseline) {
  tibble(f = files_baseline) %>%
    mutate(bio = as.integer(str_extract(f, "(?<=_bio_)\\d+"))) %>%
    arrange(bio) %>%
    pull(f)
}

# Diagnóstico defensivo de unidades térmicas (solo aplica si detecta °C*10)
fix_wc_units <- function(r) {
  temp_idx <- c(1,2,5,6,7,8,9,10,11)  # variables en °C (BIO3/BIO4 tienen escalas propias)
  # muestreo para diagnosticar
  s <- terra::spatSample(r[[temp_idx]], size = 50000, method = "random", na.rm = TRUE, as.df = TRUE)
  mx <- sapply(s, function(x) max(abs(x), na.rm = TRUE))
  scaled <- mx > 100
  if (any(scaled)) {
    r[[temp_idx[scaled]]] <- r[[temp_idx[scaled]]] / 10
  }
  r
}

# Ruta rápida: recorta/maska en EPSG:4326 y reproyecta a EPSG:3116 para promediar
prep_to_3116 <- function(r, aoi_v_ll, aoi_v_3116) {
  r_aoi <- r %>% crop(aoi_v_ll) %>% mask(aoi_v_ll)
  r_aoi <- fix_wc_units(r_aoi)
  # Reproyecta a 3116 (celdas ~área constante -> medias sin ponderación)
  project(r_aoi, crs(aoi_v_3116), method = "bilinear")
}

extract_means_by_poly <- function(r_3116, aoi_sf_3116) {
  aoi_v <- terra::vect(aoi_sf_3116)
  out <- terra::extract(r_3116, aoi_v, fun = mean, na.rm = TRUE)
  # out trae ID + bio1..bio19
  out <- as_tibble(out) %>%
    mutate(Nombre = aoi_sf_3116$Nombre[ID]) %>%
    relocate(Nombre)
  out
}

# -------------------------------
# 1) Baseline
# -------------------------------

stopifnot(exists("path_data"))

files_baseline <- list.files(
  file.path(path_data, "_baseline", "wc2.1_30s_bio"),
  full.names = TRUE,
  pattern = "\\.tif$"
)

files_baseline_ord <- order_baseline_files(files_baseline)

terraOptions(threads = terra_cores_sequential, progress = 1)

baseline <- rast(files_baseline_ord)
names(baseline) <- bio_names

baseline_3116 <- prep_to_3116(baseline, aoi_v_ll, aoi_v_3116)

baseline_mean <- extract_means_by_poly(baseline_3116, limites_adm)

# -------------------------------
# 2) Futuros (indexación)
# -------------------------------

period_dirs <- c("_2021_2040", "_2041_2060", "_2061_2080", "_2081_2100")

future_files <- map(period_dirs, ~ list.files(file.path(path_data, .x), full.names = TRUE, pattern = "\\.tif$")) %>%
  unlist()

future_index <- tibble(file = future_files) %>%
  mutate(
    period = str_extract(file, "\\d{4}-\\d{4}"),
    ssp    = str_extract(file, "ssp\\d{3}") %>% str_remove("ssp"),
    gcm    = str_extract(file, "(?<=_bioc_).*(?=_ssp)")
  ) %>%
  filter(!is.na(period), !is.na(ssp), !is.na(gcm))

# -------------------------------
# 3) Procesamiento paralelo por archivo futuro
# -------------------------------

# Importante: para evitar sobre-suscripción, al paralelizar ponemos terra cores = 1.
process_future_one <- function(f, limites_ll, limites_3116) {
  suppressPackageStartupMessages({
    library(terra)
    library(sf)
    library(dplyr)
    library(tibble)
  })
  
  terraOptions(threads = 1, progress = 0)
  terra::setGDALconfig("GDAL_NUM_THREADS", "ALL_CPUS")
  
  aoi_v_ll   <- terra::vect(st_transform(limites_ll, 4326))
  aoi_v_3116 <- terra::vect(st_transform(limites_3116, 3116))
  
  r <- rast(f)[[1:19]]
  names(r) <- paste0("bio", 1:19)
  
  # Recorte/máscara en 4326 + reproyección a 3116
  r_aoi <- r %>% crop(aoi_v_ll) %>% mask(aoi_v_ll)
  
  # Unidades (defensivo)
  # (si no quieres este paso, comenta la siguiente línea)
  r_aoi <- fix_wc_units(r_aoi)
  
  r_3116 <- project(r_aoi, crs(aoi_v_3116), method = "bilinear")
  
  out <- terra::extract(r_3116, aoi_v_3116, fun = mean, na.rm = TRUE)
  out <- as_tibble(out) %>%
    mutate(Nombre = st_transform(limites_3116, 3116)$Nombre[ID]) %>%
    relocate(Nombre)
  
  out
}

plan(multisession, workers = n_workers)

future_stats <- future_map(
  future_index$file,
  process_future_one,
  limites_ll   = limites_adm,   # se re-transforma internamente según se requiera
  limites_3116 = limites_adm,
  .options = furrr_options(seed = TRUE)
)

future_means <- future_index %>%
  mutate(stats = future_stats) %>%
  unnest(stats) %>%
  select(Nombre, gcm, ssp, period, all_of(bio_names))

# -------------------------------
# 4) Deltas (futuro - baseline)
# -------------------------------

baseline_long <- baseline_mean %>%
  select(Nombre, all_of(bio_names)) %>%
  pivot_longer(-Nombre, names_to = "bio", values_to = "baseline")

future_long <- future_means %>%
  pivot_longer(all_of(bio_names), names_to = "bio", values_to = "future")

deltas <- future_long %>%
  left_join(baseline_long, by = c("Nombre", "bio")) %>%
  mutate(
    delta_abs = future - baseline,
    delta_rel = 100 * (future - baseline) / baseline
  )

# -------------------------------
# 5) Ensemble (entre GCMs)
# -------------------------------

ensemble <- deltas %>%
  group_by(Nombre, ssp, period, bio) %>%
  summarise(
    delta_mean = mean(delta_abs, na.rm = TRUE),
    delta_sd   = sd(delta_abs, na.rm = TRUE),
    delta_p05  = quantile(delta_abs, 0.05, na.rm = TRUE),
    delta_p50  = quantile(delta_abs, 0.50, na.rm = TRUE),
    delta_p95  = quantile(delta_abs, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# (Opcional) Guardar resultados
# -------------------------------

# dir.create(file.path(path_data, "_outputs"), showWarnings = FALSE, recursive = TRUE)
saveRDS(baseline_mean, file.path(path_data, "_outputs", "baseline_mean.rds"))
saveRDS(future_means,  file.path(path_data, "_outputs", "future_means.rds"))
saveRDS(deltas,        file.path(path_data, "_outputs", "deltas_long.rds"))
saveRDS(ensemble,      file.path(path_data, "_outputs", "ensemble_deltas.rds"))

# -------------------------------
# Fin
# -------------------------------

baseline_mean
future_means
ensemble
