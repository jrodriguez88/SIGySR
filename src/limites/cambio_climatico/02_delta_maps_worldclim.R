# ============================================================
# CONTINUACIÓN DEL ANÁLISIS
# Mapas: delta_mean (ensemble) para variables clave
# Ranking: calentamiento y precipitación/sequía por subregión
# Gráficos: boxplot y densidad para comparar GCMs
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(ggnewscale)
  library(future)
  library(furrr)
})

# -------------------------------
# Requisitos
# -------------------------------

# Espera que ya hayas corrido el pipeline del canvas 1 y tengas:
# - limites_adm (sf, EPSG:3116)
# - path_data (ruta base)
# - ensemble (tabla con delta_mean / delta_sd / quantiles)
# - deltas (tabla larga por gcm)
stopifnot(exists("limites_adm"), exists("path_data"), exists("ensemble"), exists("deltas"))

# -------------------------------
# Parámetros de control
# -------------------------------

# Variables clave para mapas y comparaciones
key_bios <- c("bio1", "bio4", "bio8", "bio9", "bio12", "bio15", "bio17", "bio18")
key_idx  <- as.integer(str_remove(key_bios, "bio"))

# Periodos a mapear (ajusta si quieres 1 solo)
periods_to_map <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")
ssps_to_map    <- c("245", "585")

# Mapas: si TRUE, facet por periodo (filas) y panel (columnas) en una sola figura por variable
map_all_periods_in_one <- TRUE

# Paralelización (Windows)
use_parallel_combo <- TRUE
n_workers <- 10
terra_threads_worker <- 2  # threads por worker (evita sobre-suscripción)

# Threads secuenciales (cuando no paralelizas)
terra_threads_sequential <- 16

# Salidas
out_dir_maps <- file.path(path_data, "_outputs", "maps_delta_mean")
out_dir_tbl  <- file.path(path_data, "_outputs", "tables")
out_dir_samp <- file.path(path_data, "_outputs", "samples_gcm")
dir.create(out_dir_maps, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_tbl,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_samp, recursive = TRUE, showWarnings = FALSE)

# -------------------------------
# AOI: ll (para recorte/máscara) y 3116
# -------------------------------

limites_adm <- st_transform(limites_adm, 3116)
limites_ll  <- st_transform(limites_adm, 4326)

aoi_v_ll   <- vect(limites_ll)
aoi_v_3116 <- vect(limites_adm)

# -------------------------------
# Helpers (estilo tomado de tu script)
# -------------------------------

r_to_df <- function(r, val = "value") {
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(df)[3] <- val
  df
}
pretty_breaks_var <- function(df, n = 6) pretty(df$base_value, n = n)

# Unidades: defensivo (solo aplica si detecta °C*10)
fix_wc_units <- function(r) {
  # Función robusta: trabaja por NOMBRE de capa (no por índice)
  # y solo actúa sobre las capas térmicas que existan en el raster.
  temp_bios <- c("bio1","bio2","bio5","bio6","bio7","bio8","bio9","bio10","bio11")
  present   <- intersect(temp_bios, names(r))
  
  if (length(present) == 0) return(r)
  
  # muestreo para diagnosticar escala sin recorrer todo el raster
  s <- terra::spatSample(r[[present]], size = 50000, method = "random", na.rm = TRUE, as.df = TRUE)
  if (nrow(s) == 0) return(r)
  
  mx <- sapply(s, function(x) max(abs(x), na.rm = TRUE))
  
  # si parece venir como °C*10 (max > 100), divide por 10
  scaled <- mx > 100
  if (any(scaled)) {
    r[[present[scaled]]] <- r[[present[scaled]]] / 10
  }
  r
}

is_temp_bio <- function(bio) {
  bio %in% c("bio1","bio2","bio5","bio6","bio7","bio8","bio9","bio10","bio11")
}

# -------------------------------
# 1) Baseline (cargue seguro) y recorte a AOI
# -------------------------------

# Baseline files
files_baseline <- list.files(
  file.path(path_data, "_baseline", "wc2.1_30s_bio"),
  full.names = TRUE,
  pattern = "\\.tif$"
)

order_baseline_files <- function(files_baseline) {
  tibble(f = files_baseline) %>%
    mutate(bio = as.integer(str_extract(f, "(?<=_bio_)\\d+"))) %>%
    arrange(bio) %>%
    pull(f)
}

files_baseline_ord <- order_baseline_files(files_baseline)

terraOptions(threads = terra_threads_sequential, progress = 1)

baseline <- rast(files_baseline_ord)
names(baseline) <- paste0("bio", 1:19)

baseline_aoi <- baseline %>% crop(aoi_v_ll) %>% mask(aoi_v_ll)
baseline_aoi <- fix_wc_units(baseline_aoi)

# Guardar baseline (solo variables clave) a disco para usarlo dentro de workers (evita serialización de SpatRaster)
baseline_key_file <- file.path(path_data, "_outputs", "ensemble_delta_mean_rasters", "baseline_keyvars_aoi.tif")
dir.create(dirname(baseline_key_file), recursive = TRUE, showWarnings = FALSE)
writeRaster(baseline_aoi[[key_bios]], baseline_key_file, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))

# -------------------------------
# 2) Indexación futuros (si no existe future_index, lo creamos)
# -------------------------------

if (!exists("future_index")) {
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
}

# -------------------------------
# 3) Construcción de rasters ensemble delta_mean (por periodo × SSP)
#    (se escriben a disco para evitar mover objetos grandes entre workers)
# -------------------------------

ens_dir <- file.path(path_data, "_outputs", "ensemble_delta_mean_rasters")
dir.create(ens_dir, recursive = TRUE, showWarnings = FALSE)

build_ensemble_delta_mean_file <- function(period_sel, ssp_sel,
                                           baseline_key_file,
                                           limites_ll,
                                           future_index,
                                           key_bios, key_idx,
                                           ens_dir,
                                           threads = 2) {
  
  suppressPackageStartupMessages({
    library(terra)
    library(sf)
    library(dplyr)
    library(stringr)
  })
  
  terraOptions(threads = threads, progress = 0)
  terra::setGDALconfig("GDAL_NUM_THREADS", "ALL_CPUS")
  
  # Reconstruye AOI SpatVector dentro del proceso (evita serialización problemática)
  aoi_v_ll <- terra::vect(sf::st_transform(limites_ll, 4326))
  
  # Baseline (solo key vars)
  baseline_key <- terra::rast(baseline_key_file)
  names(baseline_key) <- key_bios  # fuerza nombres esperados
  
  # Filtra archivos del combo
  fi <- future_index %>% dplyr::filter(period == period_sel, ssp == ssp_sel)
  if (nrow(fi) == 0) stop("No hay archivos para ", period_sel, " SSP", ssp_sel)
  
  message("[Ensemble] ", period_sel, " SSP", ssp_sel, " | GCMs=", nrow(fi))
  
  # Para cada GCM: delta raster (solo key vars)
  delta_list <- vector("list", nrow(fi))
  
  for (i in seq_len(nrow(fi))) {
    f <- fi$file[i]
    g <- fi$gcm[i]
    
    r_all <- terra::rast(f)
    if (terra::nlyr(r_all) < max(key_idx)) {
      stop("Archivo con menos bandas de lo esperado: ", f, " | nlyr=", terra::nlyr(r_all))
    }
    
    r <- r_all[[key_idx]]
    names(r) <- key_bios
    
    r_aoi <- terra::crop(r, aoi_v_ll) |> terra::mask(aoi_v_ll)
    r_aoi <- fix_wc_units(r_aoi)
    
    # Asegura alineación geométrica con baseline (mismo grid)
    if (!terra::compareGeom(r_aoi, baseline_key, stopOnError = FALSE)) {
      r_aoi <- terra::resample(r_aoi, baseline_key, method = "bilinear")
    }
    
    delta_list[[i]] <- r_aoi - baseline_key
    
    if (i %% 3 == 0) message("  - ok ", i, "/", nrow(fi), " (", g, ")")
  }
  
  # Ensemble mean por variable (layer)
  mean_layers <- lapply(seq_along(key_bios), function(j) {
    s <- terra::rast(lapply(delta_list, function(x) x[[j]]))
    terra::app(s, mean, na.rm = TRUE)
  })
  
  ens_mean <- terra::rast(mean_layers)
  names(ens_mean) <- key_bios
  
  out_file <- file.path(ens_dir, paste0("ens_delta_mean_", period_sel, "_ssp", ssp_sel, ".tif"))
  terra::writeRaster(ens_mean, out_file, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))
  
  out_file
}

combo_grid <- tidyr::expand_grid(period = periods_to_map, ssp = ssps_to_map)

if (use_parallel_combo) {
  plan(multisession, workers = n_workers)
  ens_files <- future_map2(
    combo_grid$period, combo_grid$ssp,
    ~ build_ensemble_delta_mean_file(
      period_sel = .x,
      ssp_sel    = .y,
      baseline_key_file = baseline_key_file,
      limites_ll        = limites_ll,
      future_index      = future_index,
      key_bios          = key_bios,
      key_idx           = key_idx,
      ens_dir           = ens_dir,
      threads           = terra_threads_worker
    ),
    .options = furrr_options(seed = TRUE)
  )
} else {
  
  plan(sequential)
  terraOptions(threads = terra_threads_sequential, progress = 1)
  ens_files <- map2(
    combo_grid$period, combo_grid$ssp,
    ~ build_ensemble_delta_mean_file(
      period_sel = .x,
      ssp_sel    = .y,
      baseline_key_file = baseline_key_file,
      limites_ll        = limites_ll,
      future_index      = future_index,
      key_bios          = key_bios,
      key_idx           = key_idx,
      ens_dir           = ens_dir,
      threads           = terra_threads_sequential
    )
  )
}

ens_index <- combo_grid %>% mutate(file = unlist(ens_files))

# -------------------------------
# 4) Mapas delta_mean (Base | Δ SSP245 | Δ SSP585)
#    - period_sel es opcional
#    - Si period_sel = NULL, facet_grid(period ~ panel) con TODOS los periodos disponibles en ens_index
# -------------------------------


# Metadatos BIO (español + unidad)
# Nota: unidades pensadas para interpretación del panel "Base".
bio_meta <- tibble::tribble(
  ~bio,   ~bio_n, ~desc_es,                                       ~unidad,
  "bio1",   1L,  "Temperatura media anual",                     "°C",
  "bio2",   2L,  "Rango diurno medio (Tmax−Tmin)",              "°C",
  "bio3",   3L,  "Isotermalidad (BIO2/BIO7 ×100)",              "%",
  "bio4",   4L,  "Estacionalidad de la temperatura (DE ×100)",   "×100",
  "bio5",   5L,  "Temperatura máxima del mes más cálido",        "°C",
  "bio6",   6L,  "Temperatura mínima del mes más frío",          "°C",
  "bio7",   7L,  "Rango anual de temperatura (BIO5−BIO6)",       "°C",
  "bio8",   8L,  "Temperatura media del trimestre más húmedo",   "°C",
  "bio9",   9L,  "Temperatura media del trimestre más seco",     "°C",
  "bio10", 10L,  "Temperatura media del trimestre más cálido",   "°C",
  "bio11", 11L,  "Temperatura media del trimestre más frío",     "°C",
  "bio12", 12L,  "Precipitación anual",                          "mm",
  "bio13", 13L,  "Precipitación del mes más húmedo",             "mm",
  "bio14", 14L,  "Precipitación del mes más seco",               "mm",
  "bio15", 15L,  "Estacionalidad de la precipitación (CV)",      "% (CV)",
  "bio16", 16L,  "Precipitación del trimestre más húmedo",       "mm",
  "bio17", 17L,  "Precipitación del trimestre más seco",         "mm",
  "bio18", 18L,  "Precipitación del trimestre más cálido",       "mm",
  "bio19", 19L,  "Precipitación del trimestre más frío",         "mm"
)

bio_meta_get <- function(bioname) {
  m <- bio_meta %>% dplyr::filter(bio == bioname)
  if (nrow(m) == 0) return(list(bio_n = NA_integer_, desc_es = bio, unidad = ""))
  list(bio_n = m$bio_n[[1]], desc_es = m$desc_es[[1]], unidad = m$unidad[[1]])
}

bio_meta_get("bio19")

plot_delta_mean_map <- function(bio, period_sel = NULL, baseline_aoi, ens_index, out_dir_maps) {
  
  stopifnot(bio %in% names(baseline_aoi))
  
  # Periodos a incluir
  periods_use <- if (is.null(period_sel)) {
    sort(unique(ens_index$period))
  } else {
    period_sel
  }
  
  # Meta
  meta <- bio_meta_get(bio)
  
  # Base
  base_r  <- baseline_aoi[[bio]]
  base_df <- r_to_df(base_r, "base_value") %>% mutate(variable = bio)
  
  # Construye DF largo: (period × panel)
  df_list <- vector("list", length(periods_use))
  
  for (k in seq_along(periods_use)) {
    per <- periods_use[k]
    
    f245 <- ens_index %>% filter(period == per, ssp == "245") %>% pull(file)
    f585 <- ens_index %>% filter(period == per, ssp == "585") %>% pull(file)
    stopifnot(length(f245) == 1, length(f585) == 1)
    
    d245 <- rast(f245)[[bio]]
    d585 <- rast(f585)[[bio]]
    
    df_list[[k]] <- bind_rows(
      base_df %>% transmute(x, y, value = base_value, panel = "Base",     period = per),
      r_to_df(d245) %>% mutate(panel = "Δ SSP245", period = per),
      r_to_df(d585) %>% mutate(panel = "Δ SSP585", period = per)
    )
  }
  
  df <- bind_rows(df_list)
  df$panel  <- factor(df$panel,  levels = c("Base", "Δ SSP245", "Δ SSP585"))
  df$period <- factor(df$period, levels = periods_use)
  
  # Límite simétrico para los Δ (considera todos los periodos incluidos)
  lim <- df %>%
    filter(panel != "Base") %>%
    summarize(m = max(abs(range(value, na.rm = TRUE)))) %>%
    pull(m)
  
  # Paletas (según tipo)
  if (is_temp_bio(bio)) {
    base_scale <- scale_fill_viridis_c(option = "H", name = "Base")
    delta_scale <- scale_fill_gradient2(
      low = "lightblue", mid = "white", high = "red", midpoint = 0,
      limits = c(-lim, lim), name = "Δ media"
    )
  } else {
    base_scale <- scale_fill_viridis_c(option = "G", direction = -1, name = "Base")
    delta_scale <- scale_fill_gradient2(
      low = "red", mid = "white", high = "blue", midpoint = 0,
      limits = c(-lim, lim), name = "Δ media"
    )
  }
  
  # Altura dinámica para que no quede aplastado cuando se facetee por periodos
  n_rows <- length(periods_use)
  h_page <- 2.8 * n_rows
  h_std  <- 2.2 * n_rows
  
  period_tag_txt <- if (n_rows == 1) paste0("Periodo: ", periods_use) else ""
  
  p <- ggplot() +
    # Base (secuencial)
    geom_raster(data = df %>% filter(panel == "Base"), aes(x, y, fill = value)) +
    base_scale +
    ggnewscale::new_scale_fill() +
    # Δ (divergente)
    geom_raster(data = df %>% filter(panel != "Base"), aes(x, y, fill = value)) +
    delta_scale +
    facet_grid(period ~ panel) +
    coord_equal() +
    labs(
      title    = paste0("BIO", meta$bio_n),
      subtitle = paste0(meta$desc_es, " (", meta$unidad, ") — Base + Δ (futuro − base). ", period_tag_txt),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank())
  
  # Guardado
  tag_period <- if (n_rows == 1) as.character(periods_use) else "ALL"
  fn_base <- file.path(out_dir_maps, paste0("map_delta_mean_", bio, "_", tag_period))
  ggsave(filename = paste0(fn_base, "_PAGE.png"), plot = p, width = 11.7, height = h_page)
  ggsave(filename = paste0(fn_base, ".png"),      plot = p, width = 8.3,  height = h_std)
  
  p
}

# Genera mapas
maps_delta_mean <- list()
if (map_all_periods_in_one) {
  for (b in key_bios) {
    maps_delta_mean[[b]] <- plot_delta_mean_map(
      bio = b, period_sel = NULL,
      baseline_aoi = baseline_aoi,
      ens_index    = ens_index,
      out_dir_maps = out_dir_maps
    )
  }
} else {
  for (per in periods_to_map) {
    for (b in key_bios) {
      maps_delta_mean[[paste(b, per, sep = "|")]] <- plot_delta_mean_map(
        bio = b, period_sel = per,
        baseline_aoi = baseline_aoi,
        ens_index    = ens_index,
        out_dir_maps = out_dir_maps
      )
    }
  }
}

# -------------------------------
# 5) Ranking por subregión
# -------------------------------




# a) Mayor calentamiento (top 5) entre variables térmicas
rank_temp <- ensemble %>%
  filter(bio %in% c("bio1","bio2","bio5","bio6","bio7","bio8","bio9","bio10","bio11")) %>%
  left_join(bio_meta, by = "bio") %>%
  group_by(Nombre, ssp, period) %>%
  arrange(desc(delta_mean)) %>%
  slice_head(n = 5) %>%
  ungroup()

# b) Mayor disminución de precipitación (top 5 más negativos)
rank_prec_drop <- ensemble %>%
  filter(bio %in% c("bio12","bio13","bio14","bio16","bio17","bio18","bio19")) %>%
  left_join(bio_meta, by = "bio") %>%
  group_by(Nombre, ssp, period) %>%
  arrange(delta_mean) %>%
  slice_head(n = 5) %>%
  ungroup()

# c) Mayor aumento de estacionalidad (bio15)
rank_season <- ensemble %>%
  filter(bio == "bio15") %>%
  left_join(bio_meta, by = "bio") %>%
  arrange(Nombre, ssp, period)

# Guardar
write.csv(rank_temp,      file.path(out_dir_tbl, "ranking_calentamiento_top5.csv"), row.names = FALSE)
write.csv(rank_prec_drop, file.path(out_dir_tbl, "ranking_precipitacion_caidas_top5.csv"), row.names = FALSE)
write.csv(rank_season,    file.path(out_dir_tbl, "ranking_bio15_estacionalidad.csv"), row.names = FALSE)

# -------------------------------
# 6) Linerange (Δ por GCM) para comparar GCMs y variables
#    Reemplaza boxplot: aquí normalmente hay pocos puntos (uno por GCM)
# -------------------------------

plot_linerange_gcm <- function(period_sel, bios = key_bios) {
  
  df_pts <- deltas %>%
    filter(period == period_sel, bio %in% bios) %>%
    mutate(
      gcm = factor(gcm),
      ssp = factor(ssp, levels = c("245","585"))
    ) %>%
    left_join(bio_meta, by = "bio")
  
  # Resumen entre GCMs (rango + media)
  df_sum <- df_pts %>%
    group_by(Nombre, bio, bio_n, desc_es, unidad, ssp) %>%
    summarise(
      ymin  = min(delta_abs, na.rm = TRUE),
      ymax  = max(delta_abs, na.rm = TRUE),
      ymean = mean(delta_abs, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot() +
    # Puntos: cada GCM (gris)
    geom_point(
      data = df_pts,
      aes(x = ssp, y = delta_abs),
      color = "grey35", alpha = 0.45, size = 1,
      position = position_jitter(width = 0.10, height = 0)
    ) +
    # Rango min–max entre GCMs
    geom_linerange(
      data = df_sum,
      aes(x = ssp, ymin = ymin, ymax = ymax, color = ssp),
      linewidth = 0.9
    ) +
    # Media entre GCMs
    geom_point(
      data = df_sum,
      aes(x = ssp, y = ymean, color = ssp),
      size = 2
    ) +
    facet_grid(bio ~ Nombre, scales = "free_y") +
    scale_color_viridis_d(option = "C", end = 0.9, name = "SSP") +
    labs(
      title = paste0("Δ (futuro − base) por escenario — ", period_sel),
      subtitle = "Puntos grises: cada GCM. Línea: rango min–max entre GCMs. Punto: media.",
      x = NULL,
      y = "Δ (abs)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

# Ejemplo (ajusta el periodo si quieres)
linerange_gcm_2041_2060 <- plot_linerange_gcm("2041-2060")

ggsave(file.path(out_dir_maps, "linerange_gcm_keybios_2041-2060_PAGE.png"), linerange_gcm_2041_2060, width = 11.7, height = 8.3)
ggsave(file.path(out_dir_maps, "linerange_gcm_keybios_2041-2060.png"),      linerange_gcm_2041_2060, width = 8.3,  height = 6.5)


# -------------------------------
# 7) Densidades (a nivel de píxel) para comparar GCMs
#    Nota: esto NO usa la tabla deltas (que son medias por polígono),
#    sino que remuestrea valores de rasters Δ por GCM.
# -------------------------------

# Control de muestreo (evita explotar RAM)
run_density <- TRUE
n_samp_per_group <- 10000  # por (GCM × bio × subregión)

# delta raster por archivo (key vars) en AOI
get_delta_keyvars_one_file <- function(f, baseline_aoi, aoi_v_ll, key_bios, key_idx) {
  r <- rast(f)[[key_idx]]
  names(r) <- key_bios
  r_aoi <- r %>% crop(aoi_v_ll) %>% mask(aoi_v_ll)
  r_aoi <- fix_wc_units(r_aoi)
  r_aoi - baseline_aoi[[key_bios]]
}

sample_delta_by_subregion <- function(delta_r, limites_ll, n = 10000) {
  # retorna df largo: Nombre + bio + value
  out <- vector("list", nrow(limites_ll))
  for (i in seq_len(nrow(limites_ll))) {
    poly_v <- vect(limites_ll[i, ])
    d_sub <- mask(delta_r, poly_v)
    
    # muestrea (1 capa por bio)
    s <- spatSample(d_sub, size = n, method = "random", na.rm = TRUE, as.df = TRUE)
    if (nrow(s) == 0) next
    
    s <- as_tibble(s) %>%
      pivot_longer(cols = all_of(names(d_sub)), names_to = "bio", values_to = "delta") %>%
      mutate(Nombre = limites_ll$Nombre[i])
    
    out[[i]] <- s
  }
  bind_rows(out)
}

plot_density_gcm <- function(df_samp, period_sel, ssp_sel) {
  ggplot(df_samp, aes(x = delta, color = gcm)) +
    geom_density(linewidth = 0.35) +
    facet_grid(bio ~ Nombre, scales = "free") +
    scale_color_viridis_d(option = "D", end = 0.95, name = "GCM") +
    labs(title = paste0("Densidades Δ por GCM — ", period_sel, " | SSP", ssp_sel), x = "Δ (abs)", y = "Densidad") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank())
}

if (run_density) {
  # Ejemplo (ajusta si quieres): un periodo por vez
  period_sel <- "2041-2060"
  
  for (ssp_sel in ssps_to_map) {
    fi <- future_index %>% filter(period == period_sel, ssp == ssp_sel)
    
    # Construye muestras por GCM
    samp <- map_dfr(seq_len(nrow(fi)), function(i) {
      f <- fi$file[i]
      g <- fi$gcm[i]
      
      d_r <- get_delta_keyvars_one_file(f, baseline_aoi, aoi_v_ll, key_bios, key_idx)
      d_s <- sample_delta_by_subregion(d_r, limites_ll, n = n_samp_per_group)
      if (nrow(d_s) == 0) return(NULL)
      
      d_s %>% mutate(gcm = g, ssp = ssp_sel, period = period_sel)
    })
    
    saveRDS(samp, file.path(out_dir_samp, paste0("samples_density_", period_sel, "_ssp", ssp_sel, ".rds")))
    
    p_den <- plot_density_gcm(samp, period_sel, ssp_sel)
    ggsave(file.path(out_dir_maps, paste0("density_gcm_keybios_", period_sel, "_ssp", ssp_sel, "_PAGE.png")), p_den, width = 11.7, height = 8.3)
    ggsave(file.path(out_dir_maps, paste0("density_gcm_keybios_", period_sel, "_ssp", ssp_sel, ".png")),      p_den, width = 8.3,  height = 6.5)
  }
}

# -------------------------------
# Objetos útiles en memoria
# -------------------------------

ens_index
rank_temp
rank_prec_drop
rank_season
maps_delta_mean[[1]]
linerange_gcm_2041_2060
