library(terra)
library(sf)
library(dplyr)
library(stringr)


out_path_caqueta <- "data/sinchi/coberturas/"
# Argumentos funcion 
clc_level = 3
resol = 100

list_shp_clc_caqueta <- list.files(out_path_caqueta, full.names = T, 
                                   recursive = TRUE,  pattern = ".shp$")

tags_amazon <- basename(list_shp_clc_caqueta) %>% str_remove_all("clc_|.shp") 



target_crs = "EPSG:9377"

rasterize_clc <- function(clc_shp_path,
                          out_path,
                          clc_level = 3,
                          resol_m = 100,
                          target_crs = "EPSG:9377"  # MAGNA-SIRGAS / Origen Nacional
) {
  stopifnot(file.exists(clc_shp_path))
  
  # 1) Leer y pasar a sf para validar con sf::st_make_valid (tu preferencia)
  clc_sf <- st_read(clc_shp_path, quiet = TRUE)
  clc_sf <- st_make_valid(clc_sf)  # preferencia del usuario
  
  # 2) Normalizar nombres y construir codigo_simple de forma segura
  clc_sf <- clc_sf |>
    rename_with(tolower)
  
  if (!"codigo" %in% names(clc_sf)) {
    stop("No encuentro la columna 'codigo' en el shapefile.")
  }
  
  # Convertir a carácter sin notación científica y tomar los primeros "clc_level" dígitos
  codigo_chr <- formatC(as.numeric(clc_sf$codigo), format = "f", digits = 0)
  codigo_chr <- str_replace_all(codigo_chr, "[^0-9]", "")  # limpia cualquier cosa rara
  clc_sf$codigo_simple <- suppressWarnings(as.integer(substr(codigo_chr, 1, clc_level)))
  
  # 3) Filtrar NAs y geometrías vacías
  clc_sf <- clc_sf |>
    filter(!is.na(codigo_simple)) |>
    filter(!st_is_empty(geometry))
  
  if (nrow(clc_sf) == 0) stop("No hay geometrías válidas luego de limpiar 'codigo_simple'.")
  
  # 4) A CRS proyectado en metros
  if (is.na(st_crs(clc_sf))) {
    stop("El shapefile no tiene CRS definido. Define el CRS correcto antes de continuar.")
  }
  clc_sf_proj <- st_transform(clc_sf, crs = target_crs)
  
  # 5) A SpatVector para rasterizar con terra
  clc_v <- vect(clc_sf_proj)
  
  # 6) Plantilla: extent del CLC proyectado y resolución en METROS
  ext_v   <- ext(clc_v)
  r_tmpl  <- rast(extent = ext_v, resolution = resol_m, crs = crs(clc_v))
  
  # Diagnóstico útil
  message("CRS origen: ", st_crs(clc_sf)$input)
  message("CRS destino: ", target_crs)
  message("Extent (proj): ", paste(signif(c(ext_v$xmin, ext_v$xmax, ext_v$ymin, ext_v$ymax), 3), collapse = ", "))
  message("Resolución (m): ", resol_m)
  message("Dim plantilla (ncol x nrow): ", paste(dim(r_tmpl)[2:1], collapse = " x "))
  
  if (ncol(r_tmpl) < 2 || nrow(r_tmpl) < 2) {
    stop("La plantilla quedó con <2 columnas/filas. Revisa el CRS y la resolución en METROS.")
  }
  
  # 7) Rasterizar (touches=TRUE ayuda con polígonos angostos)
  r_clc <- rasterize(clc_v, r_tmpl, field = "codigo_simple", touches = TRUE, background = NA)
  
  # Comprobaciones
  r_rng <- try(minmax(r_clc), silent = TRUE)
  if (inherits(r_rng, "try-error") || all(is.nan(values(r_clc)))) {
    stop("El raster resultante no tiene valores. Revisa solape/CRS/resolución.")
  }
  
  # 8) Guardar
  tag <- basename(clc_shp_path) |> str_remove("\\.shp$")
  nivel_clc <- paste0("nivel_", clc_level)
  out_file <- file.path(out_path, paste0(tag, "_", nivel_clc, "_", resol_m, "m_", "epsg", gsub("\\D","", target_crs), ".tif"))
  
  writeRaster(r_clc, out_file, overwrite = TRUE)
  message("💾 Guardado: ", out_file)
  
  return(r_clc)
}



r_list <- purrr::map(
  list_shp_clc_caqueta,
  ~rasterize_clc(.x,
                 out_path   = out_path_caqueta,
                 clc_level  = 3,
                 resol_m    = 100,          # metros
                 target_crs = "EPSG:9377")  # MAGNA-SIRGAS / Origen Nacional
)




## Graficos ----
r_list %>% rast() %>% plot



## Sett plots
colors_clc <- crear_paleta_clc("nivel_3") %>% 
  mutate(value = codigo_simple) %>%
  dplyr::select(- nivel, -codigo_simple)
names(colors_clc$color) <- colors_clc$leyenda

data_clc_amazon_col <- r_list %>% 
  map(calcula_area_clc, pixel_resol = 100) %>%
  set_names(tags_amazon %>% tolower() %>%
              str_remove_all("region_100k_")) %>% 
  bind_rows(.id = "Periodo") %>% left_join(colors_clc)

colors_in_clc <- data_clc_amazon_col$color %>% unique()

#

clc_amazon_plot <- ggplot(data_clc_amazon_col , aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Amazonia Colombiana",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria")


data_clc_caqueta <- r_list %>% map(terra::crop, caqueta_shp, mask = TRUE) %>%
  map(calcula_area_clc, pixel_resol = 100) %>%
  set_names(tags_amazon %>% tolower() %>%
              str_remove_all("region_100k_")) %>% 
  bind_rows(.id = "Periodo") %>% left_join(colors_clc)

colors_in_clc <- data_clc_caqueta$color %>% unique()

#.

expanse(lc_caqueta[[1]], unit="ha")

terra::focal(lc_caqueta[[1]], )


clc_caqueta_plot <- ggplot(data_clc_caqueta , aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Departamento del Caqueta",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria Coberturas CLC: ")

library(patchwork)
clc_amazon_plot +
  theme(legend.position = "none") | clc_caqueta_plot 

## Datos municipales

caqueta_clc_municipios <- expand_grid(
  municipio = abrigue_municipios_caqueta %>% 
    split(abrigue_municipios_caqueta$MpNombre),
  cobertura = set_names(r_list, tags_amazon %>% tolower() %>%
                          str_remove_all("region_100k_")))

data_clc_municipios_caqueta <- caqueta_clc_municipios %>% 
  mutate(
    Municipio = names(municipio),
    Periodo = names(cobertura),
    raster_crop = map2(.x = cobertura, .y = municipio, terra::crop, mask = TRUE)) %>%
  mutate(areas = map(raster_crop, calcula_area_clc, pixel_resol = 100)) %>% 
  dplyr::select(Municipio, Periodo, areas) %>%
  unnest(areas) %>% left_join(colors_clc) 

colors_in_clc_municipios <- data_clc_municipios_caqueta$color %>% unique()


clc_municipios_caqueta_plot <- data_clc_municipios_caqueta %>% 
  ggplot(aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  #facet_grid(~Municipio) +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Municipios Caqueta",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria:") +
  theme(legend.position = "bottom")

data_clc_municipios_caqueta %>% 
  ggplot(aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Municipio, scale = "free") +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Municipios Caqueta",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria:") +
  theme(legend.position = "bottom")



# Datos fincas Caqueta 


fincas_caqueta <- st_read("00_data__/poligonos_fincas_caqueta/R_TERRENO.shp") %>% 
  st_transform(crs = target_crs)


caqueta_clc_fincas <- expand_grid(
  finca = fincas_caqueta %>% 
    split(fincas_caqueta$CODIGO),
  cobertura = set_names(r_list, tags_amazon %>% tolower() %>%
                          str_remove_all("region_100k_")))


data_clc_fincas_caqueta <- caqueta_clc_fincas %>% 
  mutate(
    Finca = names(finca),
    Periodo = names(cobertura),
    raster_crop = map2(.x = cobertura, .y = finca, terra::crop, mask = TRUE)) %>%
  mutate(areas = map(raster_crop, calcula_area_clc, pixel_resol = 100)) %>% 
  dplyr::select(Finca, Periodo, areas) %>%
  unnest(areas) %>% left_join(colors_clc) 


clc_fincas_caqueta_plot <- data_clc_fincas_caqueta %>% 
  ggplot(aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  #facet_grid(~Municipio) +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Fincas Caqueta",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria:") +
  theme(legend.position = "bottom")




# Datos fincas ABRIGUE

## Datos municipales
fincas_abrigue <- st_read("00_data__/poligonos_finca/Predios_Abrigue_v4_24_10_2024.shp") %>% 
  st_transform(crs = target_crs)



caqueta_clc_abrigue <- expand_grid(
  finca_abrigue = fincas_abrigue %>% 
    split(fincas_abrigue$cu),
  cobertura = set_names(r_list, tags_amazon %>% tolower() %>%
                          str_remove_all("region_100k_")))


data_clc_fincas_abrigue <- caqueta_clc_abrigue %>% 
  mutate(
    Finca_ABRIGUE= names(finca_abrigue),
    Periodo = names(cobertura),
    raster_crop = map2(.x = cobertura, .y = finca_abrigue, terra::crop, mask = TRUE)) %>%
  mutate(areas = map(raster_crop, calcula_area_clc, pixel_resol = 100)) %>% 
  dplyr::select(Finca_ABRIGUE, Periodo, areas) %>%
  unnest(areas) %>% left_join(colors_clc) 


clc_fincas_abrigue_plot <- data_clc_fincas_abrigue %>% 
  ggplot(aes(x = Periodo, y = area_total, fill = leyenda)) +
  geom_bar(stat = "identity") +
  #facet_grid(~Municipio) +
  scale_fill_manual(values = colors_clc$color) +
  theme_minimal() +
  labs(title = "Fincas ABRIGUE",
       x = "Periodo",
       y = "Hectareas", 
       fill = "Categoria:") +
  theme(legend.position = "bottom")



library(patchwork)
(clc_amazon_plot +
  theme(legend.position = "none") | clc_caqueta_plot )/
  (clc_fincas_caqueta_plot +
  theme(legend.position = "none")| clc_fincas_abrigue_plot +
  theme(legend.position = "none"))

data_clc_amazon_col %>% tibble()
data_clc_caqueta %>% tibble()
data_clc_municipios_caqueta
data_clc_fincas_caqueta
data_clc_fincas_abrigue


caqueta_municipios_shp
fincas_abrigue
fincas_caqueta







library(terra)
library(dplyr)
library(tidyr)
library(stringr)

# -----------------------------
# 0) Setup
# -----------------------------
r2024 <- r_list[[1]] %>% terra::crop(caqueta_shp, mask = TRUE) # Cob_Region_100k_2024
r2020 <- r_list[[2]] %>% terra::crop(caqueta_shp, mask = TRUE) # Cob_Region_100K_2020
names(r2020) <- "CLC_2020"
names(r2024) <- "CLC_2024"

# Seguridad: misma grilla
stopifnot(compareGeom(r2020, r2024, stopOnError = TRUE))

# Cada celda en ha (por si cambias resolución en el futuro)
cell_ha <- prod(res(r2020)) / 1e4  # aquí debe dar 1

# -----------------------------
# 1) Matriz de transición completa (todas las clases)
# -----------------------------
# crosstab sobre stack de 2 capas (2020, 2024)
tab <- crosstab(c(r2020, r2024), long = FALSE, useNA = FALSE)

# 'tab' es una tabla base; conviértela a data.frame "bonita"
trans_df <- as.data.frame.matrix(tab)
# Asegura nombres/ids como enteros
rownames(trans_df) <- as.integer(rownames(trans_df))
colnames(trans_df) <- as.integer(colnames(trans_df))

# Tidy: from (2020), to (2024), celdas, ha, % sobre origen y % global
trans_long <- trans_df %>%
  tibble::rownames_to_column(var = "from_2020") %>%
  pivot_longer(-from_2020, names_to = "to_2024", values_to = "cells") %>%
  mutate(
    from_2020 = as.integer(from_2020),
    to_2024   = as.integer(to_2024),
    ha        = cells * cell_ha
  ) %>%
  filter(cells > 0) %>%
  group_by(from_2020) %>%
  mutate(
    ha_from_total = sum(ha),
    pct_from      = 100 * ha / pmax(ha_from_total, 1e-9)
  ) %>%
  ungroup() %>%
  mutate(
    ha_global_total = sum(ha),
    pct_global      = 100 * ha / pmax(ha_global_total, 1e-9)
  ) %>%
  arrange(desc(ha))

# -----------------------------
# 2) Bloque Bosque -> destino
# -----------------------------
forest_codes <- c(311:3115)

bosque_outflows <- trans_long %>%
  filter(from_2020 %in% forest_codes) %>%
  arrange(desc(ha))

# (Opcional) Colapsar los 3 códigos de bosque en un solo “Bosque”
bosque_outflows_collapsed <- bosque_outflows %>%
  group_by(to_2024) %>%
  summarise(
    ha = sum(ha),
    cells = sum(cells),
    pct_global = sum(pct_global),
    .groups = "drop"
  ) %>%
  arrange(desc(ha)) %>%
  mutate(
    pct_sobre_bosque = 100 * ha / sum(ha)  # % del total que salió desde bosque
  )

# -----------------------------
# 3) TOP transiciones (útil para reporte)
# -----------------------------
top10_global <- trans_long %>%
  slice_max(ha, n = 10)

top10_desde_bosque <- bosque_outflows %>%
  slice_max(ha, n = 10)

# -----------------------------
# 4) (Opcional) Matriz con MÁSCARA (e.g., fincas ABRIGUE o Caquetá)
#     -> Pasa un SpatVector (polígonos) y repite el cálculo
# -----------------------------
transition_with_mask <- function(poly_mask, r2020, r2024, forest_codes = c(311:315)) {
  # recorta y enmascara (rápido)
  r20m <- mask(crop(r2020, vect(poly_mask)), vect(poly_mask))
  r24m <- mask(crop(r2024, vect(poly_mask)), vect(poly_mask))
  
  # si la máscara deja todo NA, devuelve tibble vacío
  if (all(is.na(values(r20m))) || all(is.na(values(r24m)))) {
    return(list(
      trans_long = tibble(),
      bosque_outflows = tibble(),
      bosque_outflows_collapsed = tibble()
    ))
  }
  
  tabm <- crosstab(c(r20m, r24m), long = FALSE, useNA = FALSE)
  trans_dfm <- as.data.frame.matrix(tabm)
  rownames(trans_dfm) <- as.integer(rownames(trans_dfm))
  colnames(trans_dfm) <- as.integer(colnames(trans_dfm))
  
  trans_long_m <- trans_dfm %>%
    tibble::rownames_to_column(var = "from_2020") %>%
    pivot_longer(-from_2020, names_to = "to_2024", values_to = "cells") %>%
    mutate(
      from_2020 = as.integer(from_2020),
      to_2024   = as.integer(to_2024),
      ha        = cells * (prod(res(r2020)) / 1e4)
    ) %>%
    filter(cells > 0) %>%
    group_by(from_2020) %>%
    mutate(
      ha_from_total = sum(ha),
      pct_from      = 100 * ha / pmax(ha_from_total, 1e-9)
    ) %>%
    ungroup() %>%
    mutate(
      ha_global_total = sum(ha),
      pct_global      = 100 * ha / pmax(ha_global_total, 1e-9)
    ) %>%
    arrange(desc(ha))
  
  bosque_out_m <- trans_long_m %>%
    filter(from_2020 %in% forest_codes) %>%
    arrange(desc(ha))
  
  bosque_out_coll_m <- bosque_out_m %>%
    group_by(to_2024) %>%
    summarise(
      ha = sum(ha),
      cells = sum(cells),
      pct_global = sum(pct_global),
      .groups = "drop"
    ) %>%
    arrange(desc(ha)) %>%
    mutate(
      pct_sobre_bosque = 100 * ha / sum(ha)
    )
  
  list(
    trans_long = trans_long_m,
    bosque_outflows = bosque_out_m,
    bosque_outflows_collapsed = bosque_out_coll_m
  )
}

# --- Ejemplo de uso de la máscara (descomenta si quieres):
res_abrigue <- transition_with_mask(fincas_abrigue, r2020, r2024, forest_codes)
res_caqueta <- transition_with_mask(fincas_caqueta, r2020, r2024, forest_codes)

# Objetos clave que ya puedes inspeccionar/exportar:
#   trans_long                -> matriz completa (tidy) 2020→2024
#   bosque_outflows           -> sólo filas from ∈ {311,313,314}
#   bosque_outflows_collapsed -> “Bosque (311/313/314) → destino” colapsado
#   top10_global / top10_desde_bosque


clc_labels <- tibble::tribble(
  ~code, ~etiqueta,
  111, "1.1.1 Tejido urbano continuo",
  112, "1.1.2 Tejido urbano discontinuo",
  121, "1.2.1 Zonas industriales/comerciales",
  124, "1.2.4 Aeropuertos",
  131, "1.3.1 Minería",
  211, "2.1.1 Otros cultivos transitorios",
  212, "2.1.2 Cereales",
  223, "2.2.3 Cultivos permanentes arbóreos",
  224, "2.2.4 Agroforestales",
  231, "2.3.1 Pastos limpios",
  232, "2.3.2 Pastos arbolados",
  233, "2.3.3 Pastos enmalezados",
  241, "2.4.1 Mosaico de cultivos",
  242, "2.4.2 Mosaico cultivos y pastos",
  243, "2.4.3 Mosaico cultivos, pastos y esp. naturales",
  244, "2.4.4 Mosaico pastos con esp. naturales",
  311, "3.1.1 Bosque denso",
  313, "3.1.3 Bosque fragmentado",
  314, "3.1.4 Bosque de galería",
  321, "3.2.1 Herbazales",
  323, "3.2.3 Vegetación secundaria/transición",
  334, "3.3.4 Vegetación arbustiva densa",
  511, "5.1.1 Agua continental"
  # ... agrega las que uses
)

bosque_destinos_labeled <- bosque_outflows_collapsed %>%
  left_join(clc_labels, by = c("to_2024" = "code")) %>%
  dplyr::select(to_2024, etiqueta, ha, pct_sobre_bosque) %>%
  arrange(desc(ha))










path_files <- "D:/00_DEVELOPER/Invest_caqueta//new_sim/intermediate_outputs/"

files <- list.files(path_files, full.names = T)


lulc_2020  <- rast("D:/00_DEVELOPER/Invest_caqueta/new_sim/Cob_Region_100K_2020_nivel_3_100m_epsg9377.tif") %>% terra::crop(caqueta_shp, mask = TRUE)
lulc_2024 <- rast("D:/00_DEVELOPER/Invest_caqueta/new_sim/Cob_Region_100k_2024_nivel_3_100m_epsg9377.tif") %>% terra::crop(caqueta_shp, mask = TRUE)
total <- rast("D:/00_DEVELOPER/Invest_caqueta/new_sim/tot_c_cur.tif") %>% terra::crop(caqueta_shp, mask = TRUE)


rasters = rast(files %>% str_subset("_cur.")) %>% terra::crop(caqueta_shp, mask = TRUE)
rasters_fut = rast(files %>% str_subset("_fut.")) %>% terra::crop(caqueta_shp, mask = TRUE)
rasters = c(rasters, total, lulc_2020) #%>% terra::crop(caqueta_shp, mask = TRUE)

resumen <- map_df(names(rasters), function(layer){
  f <- freq(rasters[[layer]]) %>%
    mutate(Carbono = value * count) %>%
    summarise(Total = sum(Carbono))
  tibble(Reservorio = layer, Carbono_t = f$Total)
})

resumen_2024 <- map_df(names(rasters_fut), function(layer){
  f <- freq(rasters_fut[[layer]]) %>%
    mutate(Carbono = value * count) %>%
    summarise(Total = sum(Carbono))
  tibble(Reservorio = layer, Carbono_t = f$Total)
})




library(ggplot2)
df <- bind_rows(
  resumen %>% mutate(Año="2020"),
  resumen_2024 %>% mutate(Año="2024")
)
ggplot(df, aes(x=Reservorio, y=Carbono_t/1e6, fill=Año)) +
  geom_col(position="dodge") +
  labs(y="Carbono (10⁶ t)", x="", fill="Escenario") +
  theme_minimal()



delta <- rast("D:/00_DEVELOPER/Invest_caqueta/new_sim/delta_cur_fut.tif")
plot(delta, main="Δ Carbono Total (2030–2020)", 
     col=terrain.colors(100))

par(mfrow = c(3, 1))
map2(r_list, tags_amazon, ~plot_raster_clc(.x, clc_level = clc_level, tag = .y))
library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(11, "RdYlGn"))  # red→yellow→green
rng <- range(values(delta), na.rm = TRUE)
m  <- max(abs(rng))
plot(delta, col = pal(200), zlim = c(-m, m), main = "Δ Carbono Total (2020–2024)")




library(terra)
library(raster)           # <- importante
library(sf)
library(exactextractr)

# Recorta y fuerza a disco (igual que antes)
delta_abri <- terra::crop(delta, vect(fincas_abrigue), mask = TRUE)
delta_caq  <- terra::crop(delta, vect(fincas_caqueta), mask = TRUE)

f_abri <- tempfile(fileext = ".tif")
f_caq  <- tempfile(fileext = ".tif")
writeRaster(delta_abri, f_abri, overwrite = TRUE)
writeRaster(delta_caq,  f_caq,  overwrite = TRUE)

# Convierte a RasterLayer (raster pkg)
r_abri_r <- raster::raster(f_abri)
r_caq_r  <- raster::raster(f_caq)

# Asegura CRS/geom válidos
fincas_abrigue <- st_make_valid(fincas_abrigue)
fincas_caqueta <- st_make_valid(fincas_caqueta)
if (st_crs(fincas_abrigue) != st_crs(r_abri_r)) fincas_abrigue <- st_transform(fincas_abrigue, st_crs(r_abri_r))
if (st_crs(fincas_caqueta) != st_crs(r_caq_r))  fincas_caqueta <- st_transform(fincas_caqueta,  st_crs(r_caq_r))

# Media exacta ponderada por fracción de celda (rápido)
abri_stats <- exactextractr::exact_extract(r_abri_r, fincas_abrigue, "mean")
caq_stats  <- exactextractr::exact_extract(r_caq_r,  fincas_caqueta,  "mean")

# Arma tablas (tC/ha y tC totales por finca)
abri_tbl <- tibble::tibble(
  Finca = fincas_abrigue$cu,
  delta_mean_ha = abri_stats,
  area_ha = as.numeric(st_area(fincas_abrigue))/1e4,
  cohorte = "ABRIGUE"
) |> dplyr::mutate(delta_total_tC = delta_mean_ha * area_ha)

caq_tbl <- tibble::tibble(
  Finca = fincas_caqueta$CODIGO,
  delta_mean_ha = caq_stats,
  area_ha = as.numeric(st_area(fincas_caqueta))/1e4,
  cohorte = "Caqueta"
) |> dplyr::mutate(delta_total_tC = delta_mean_ha * area_ha)






library(dplyr)
library(purrr)
library(tidyr)

# --- 1) Unir y limpiar ---
stopifnot(exists("abri_tbl"), exists("caq_tbl"))
pool <- bind_rows(abri_tbl, caq_tbl) %>%
  filter(is.finite(delta_mean_ha), is.finite(area_ha), area_ha > 0)

# --- 2) Estratificación por quintiles de área ---
pool <- pool %>% mutate(q_area = ntile(area_ha, 5))
abri <- pool %>% filter(cohorte == "ABRIGUE")
caq  <- pool %>% filter(cohorte == "Caqueta")

# Conteos objetivo por quintil (los de ABRIGUE)
target_q <- abri %>% count(q_area, name = "n_abri")

sample_caq_by_quint <- function(target_q, caq_df){
  target_q %>%
    left_join(caq_df, by = "q_area") %>%
    group_by(q_area) %>%
    group_modify(~{
      n_take <- .x$n_abri[1]
      pool_q <- .x %>% filter(cohorte == "Caqueta")
      if (is.null(n_take) || is.na(n_take) || n_take == 0 || nrow(pool_q) == 0) {
        pool_q %>% slice(0)
      } else {
        pool_q %>% slice_sample(n = min(n_take, nrow(pool_q)),
                                replace = n_take > nrow(pool_q))
      }
    }) %>%
    ungroup()
}

# --- 3) Comparación principal en Δ tC/ha ---
set.seed(42)
B <- 1000

# Punto + IC95% (bootstrap estratificado)
boot_diff <- map_dbl(1:B, ~{
  caq_s <- sample_caq_by_quint(target_q, caq)
  mean(abri$delta_mean_ha, na.rm = TRUE) - mean(caq_s$delta_mean_ha, na.rm = TRUE)
})
diff_point <- mean(abri$delta_mean_ha, na.rm = TRUE) -
  mean(sample_caq_by_quint(target_q, caq)$delta_mean_ha, na.rm = TRUE)
diff_ci <- quantile(boot_diff, c(.025, .5, .975), na.rm = TRUE)

# Wilcoxon (muestra emparejada por quintil; no es 1-1, pero misma n por estrato)
set.seed(7)
caq_once <- sample_caq_by_quint(target_q, caq)
p_wilcox <- wilcox.test(abri$delta_mean_ha, caq_once$delta_mean_ha)$p.value

# --- 4) % fincas emisoras / secuestradoras ---
status_global <- pool %>%
  mutate(status = case_when(
    delta_mean_ha >  0 ~ "Secuestro",
    delta_mean_ha <  0 ~ "Emisión",
    TRUE               ~ "Neutro"
  )) %>%
  count(cohorte, status, name = "n") %>%
  group_by(cohorte) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  arrange(cohorte, desc(pct))

status_quint <- pool %>%
  mutate(status = case_when(
    delta_mean_ha >  0 ~ "Secuestro",
    delta_mean_ha <  0 ~ "Emisión",
    TRUE               ~ "Neutro"
  )) %>%
  count(cohorte, q_area, status, name = "n") %>%
  group_by(cohorte, q_area) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  arrange(q_area, cohorte, desc(pct))

# Diferencia en % de secuestradoras (bootstrap, global)
set.seed(2025)
pct_sec_boot <- map_dbl(1:B, ~{
  caq_s <- sample_caq_by_quint(target_q, caq)
  100*mean(abri$delta_mean_ha > 0, na.rm = TRUE) -
    100*mean(caq_s$delta_mean_ha > 0, na.rm = TRUE)
})
pct_sec_ci <- quantile(pct_sec_boot, c(.025, .5, .975), na.rm = TRUE)

# --- 5) Hotspots (por magnitud total) ---
hotspots_abri_emision   <- abri %>% arrange(delta_total_tC)             %>% slice_head(n = 20)
hotspots_abri_secuestro <- abri %>% arrange(desc(delta_total_tC))       %>% slice_head(n = 20)
hotspots_caq_emision    <- caq  %>% arrange(delta_total_tC)             %>% slice_head(n = 20)
hotspots_caq_secuestro  <- caq  %>% arrange(desc(delta_total_tC))       %>% slice_head(n = 20)

# --- 6) Salida resumida ---
cat("\n== Δ tC/ha (ABRIGUE - Caquetá) ==\n")
print(c(point = diff_point, quantile(boot_diff, c(.025,.5,.975), na.rm=TRUE)))
cat("\nWilcoxon p-value:", p_wilcox, "\n")

cat("\n== % Secuestradoras (ABRIGUE - Caquetá), IC95 bootstrap ==\n")
print(quantile(pct_sec_boot, c(.025,.5,.975), na.rm=TRUE))

cat("\n== Composición global (Emisión/Secuestro/Neutro) ==\n")
print(status_global)

cat("\n== Composición por quintil de área ==\n")
print(status_quint)



library(dplyr)
library(purrr)
set.seed(123)
B <- 1000

wilx_boot <- map_dfr(1:B, ~{
  caq_s <- sample_caq_by_quint(target_q, caq)
  wt <- suppressWarnings(
    wilcox.test(abri$delta_mean_ha,
                caq_s$delta_mean_ha,
                exact = FALSE, conf.int = TRUE, conf.level = 0.95)
  )
  tibble(
    p = wt$p.value,
    HL = unname(wt$estimate),         # Hodges–Lehmann (ABRIGUE − Caquetá)
    HL_lo = wt$conf.int[1],
    HL_hi = wt$conf.int[2]
  )
})

# Resumen a reportar
wilx_summary <- wilx_boot %>% summarize(
  p_med   = median(p, na.rm=TRUE),
  p_2.5   = quantile(p, .025, na.rm=TRUE),
  p_97.5  = quantile(p, .975, na.rm=TRUE),
  prop_p_lt_0.05 = mean(p < .05, na.rm=TRUE),
  
  HL_med  = median(HL, na.rm=TRUE),
  HL_2.5  = quantile(HL, .025, na.rm=TRUE),
  HL_97.5 = quantile(HL, .975, na.rm=TRUE),
  
  # “IC medio” (promediar límites puede ser útil como orientación)
  HL_lo_med = median(HL_lo, na.rm=TRUE),
  HL_hi_med = median(HL_hi, na.rm=TRUE)
)
wilx_summary



# install.packages("coin")
library(coin)

van_elteren_once <- function(){
  caq_s <- sample_caq_by_quint(target_q, caq)
  dat  <- dplyr::bind_rows(
    dplyr::mutate(abri, grupo="ABRIGUE"),
    dplyr::mutate(caq_s, grupo="Caqueta")
  )
  # Prueba estratificada por quintil:
  it <- independence_test(delta_mean_ha ~ factor(grupo) | factor(q_area),
                          data = dat,
                          distribution = approximate(B = 5000))  # permutaciones
  c(stat = statistic(it), p = pvalue(it))
}

set.seed(123)
B <- 400
ve_boot <- replicate(B, van_elteren_once(), simplify = "matrix")
ve_boot <- tibble::as_tibble(t(ve_boot), .name_repair = "minimal")
names(ve_boot) <- c("stat","p")

ve_summary <- ve_boot %>% summarize(
  p_med   = median(p, na.rm=TRUE),
  p_2.5   = quantile(p, .025, na.rm=TRUE),
  p_97.5  = quantile(p, .975, na.rm=TRUE),
  prop_p_lt_0.05 = mean(p < .05, na.rm=TRUE)
)
ve_summary







# Diferencia de tC/ha ponderada por área
wmean <- function(x, w) sum(x*w, na.rm=TRUE)/sum(w, na.rm=TRUE)
abri_w <- wmean(abri$delta_mean_ha, abri$area_ha)
caq_w  <- wmean(caq$delta_mean_ha,  caq$area_ha)
diff_w <- abri_w - caq_w
c(abri_w=abri_w, caq_w=caq_w, diff_w=diff_w)



thr <- 0.1
pool2 <- pool %>% mutate(status2 = case_when(
  delta_mean_ha >  thr ~ "Secuestro",
  delta_mean_ha < -thr ~ "Emisión",
  TRUE                 ~ "Neutro"
))



library(dplyr)
library(purrr)

# Asumo que ya tienes: pool (ABRIGUE + Caquetá con q_area), abri, caq, target_q (conteos de ABRIGUE por quintil)

wmean <- function(x, w) sum(x*w, na.rm=TRUE)/sum(w, na.rm=TRUE)

set.seed(2025)
B <- 1000
boot_diff_w <- map_dbl(1:B, ~{
  caq_s <- sample_caq_by_quint(target_q, caq)
  abri_w <- wmean(abri$delta_mean_ha, abri$area_ha)
  caq_w  <- wmean(caq_s$delta_mean_ha, caq_s$area_ha)
  abri_w - caq_w
})

diff_w_point <- wmean(abri$delta_mean_ha, abri$area_ha) -
  wmean(caq$delta_mean_ha,  caq$area_ha)

boot_diff_w %>% summary

diff_w_ci <- quantile(boot_diff_w, c(.025, .5, .975), na.rm=TRUE)

c(point = diff_w_point, diff_w_ci)







status2_global <- pool2 %>%
  count(cohorte, status2, name="n") %>%
  group_by(cohorte) %>% mutate(pct = 100*n/sum(n)) %>% ungroup() %>%
  arrange(cohorte, desc(pct))

status2_quint <- pool2 %>%
  count(cohorte, q_area, status2, name="n") %>%
  group_by(cohorte, q_area) %>% mutate(pct = 100*n/sum(n)) %>% ungroup()

status2_global
status2_quint

set.seed(2025)
B <- 1000
pct_sec2_boot <- map_dbl(1:B, ~{
  caq_s <- sample_caq_by_quint(target_q, caq)
  100*mean(abri$delta_mean_ha >  0.1, na.rm=TRUE) -
    100*mean(caq_s$delta_mean_ha >  0.1, na.rm=TRUE)
})
quantile(pct_sec2_boot, c(.025,.5,.975), na.rm=TRUE)




library(ggplot2); library(dplyr)

status2_global %>%
  ggplot(aes(x = cohorte, y = pct, fill = status2)) +
  geom_col(width = 0.7, color = "grey20") +
  geom_text(aes(label = scales::percent(pct/100, accuracy = 0.1)),
            position = position_stack(vjust = 0.5), size = 3) +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Proporción de fincas", fill = "Estado",
       title = "Composición global de fincas por estado de Δ carbono (umbral ±0.1 tC/ha)")



status2_quint %>%
  ggplot(aes(x = factor(q_area), y = pct, fill = status2)) +
  geom_col(position = "fill", color = "grey20") +
  facet_wrap(~ cohorte) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Quintil de área", y = "Proporción", fill = "Estado",
       title = "Composición por quintiles de área")




pool2 %>%
  ggplot(aes(x = cohorte, y = delta_mean_ha)) +
  geom_violin(trim = TRUE, fill = "grey85") +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed") +
  coord_cartesian(ylim = quantile(pool2$delta_mean_ha, c(.02, .98), na.rm=TRUE)) +
  labs(x = NULL, y = "Δ tC/ha",
       title = "Distribución de Δ tC/ha por cohorte (banda neutra ±0.1)")


pool2 %>%
  ggplot(aes(x = delta_mean_ha, color = cohorte)) +
  stat_ecdf(size = 1) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  coord_cartesian(xlim = quantile(pool2$delta_mean_ha, c(.01, .99), na.rm=TRUE)) +
  labs(x = "Δ tC/ha", y = "F(x)", title = "ECDF de Δ tC/ha por cohorte")



df_ci <- tibble::tibble(
  métrica = c("No ponderada", "Ponderada por área"),
  point = c(diff_point, diff_w_point),
  lo = c(quantile(boot_diff, .025, na.rm=TRUE),
         quantile(boot_diff_w, .025, na.rm=TRUE)),
  hi = c(quantile(boot_diff, .975, na.rm=TRUE),
         quantile(boot_diff_w, .975, na.rm=TRUE))
)

ggplot(df_ci, aes(y = métrica, x = point)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Δ tC/ha (ABRIGUE − Caquetá)", y = NULL,
       title = "Diferencias de medias con IC95% (bootstrap)")



library(forcats)

plot_hotspots <- function(df, titulo){
  df %>%
    mutate(Finca = fct_reorder(Finca, delta_total_tC)) %>%
    ggplot(aes(y = Finca, x = delta_total_tC)) +
    geom_segment(aes(x = 0, xend = delta_total_tC, yend = Finca), alpha = .6) +
    geom_point(size = 2) +
    labs(x = "Δ total tC", y = NULL, title = titulo)
}

# suponiendo que ya tienes:
# hotspots_abri_emision, hotspots_abri_secuestro,
# hotspots_caq_emision,  hotspots_caq_secuestro

plot_hotspots(hotspots_abri_emision, "ABRIGUE – Top 20 Emisión (Δ total tC)") 
plot_hotspots(hotspots_abri_secuestro %>% arrange(desc(delta_total_tC)),
              "ABRIGUE – Top 20 Secuestro (Δ total tC)")
plot_hotspots(hotspots_caq_emision,  "Caquetá – Top 20 Emisión (Δ total tC)")
plot_hotspots(hotspots_caq_secuestro %>% arrange(desc(delta_total_tC)),
              "Caquetá – Top 20 Secuestro (Δ total tC)")




set.seed(1)
pool_sample <- bind_rows(
  pool2 %>% filter(cohorte=="ABRIGUE"),
  pool2 %>% filter(cohorte=="Caqueta") %>% slice_sample(n = 6000)
)

pool_sample %>%
  ggplot(aes(x = area_ha, y = delta_mean_ha, color = cohorte)) +
  geom_point(alpha = .25, size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE) +
  scale_x_log10(labels = scales::label_number()) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed") +
  labs(x = "Área (ha, escala log10)", y = "Δ tC/ha",
       title = "Δ tC/ha vs tamaño del predio (suavizado GAM)")

# 
# 
# library(terra); library(sf)
# 
# # recorta a la envolvente de fincas (ambas cohortes)
# bb <- st_bbox(st_union(st_as_sf(fincas_abrigue), st_as_sf(fincas_caqueta)))
# delta_crop <- crop(delta, ext(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
# 
# plot(delta_crop, main = "Δ tC/ha (InVEST)")
# plot(st_geometry(fincas_caqueta), add = TRUE, border = "grey30", lwd = 0.2)
# plot(st_geometry(fincas_abrigue), add = TRUE, border = "red", lwd = 0.8)





library(dplyr)
library(terra)

# Plantilla binaria = misma grilla del delta
r0 <-  init(delta, 0)

# IDs por quintil (desde tu pool)
ids_por_q <- pool %>%
  filter(cohorte == "Caqueta") %>%
  distinct(Finca, q_area) %>%
  group_by(q_area) %>%
  summarise(ids = list(Finca), .groups = "drop")

# Huella única por quintil (ha)
Aq <- sapply(1:5, function(q){
  ids_q <- ids_por_q$ids[[which(ids_por_q$q_area == q)]]
  if (length(ids_q) == 0) return(0)
  # subset de polígonos Caquetá por esos IDs
  fincas_q <- fincas_caqueta[fincas_caqueta$CODIGO %in% ids_q, ]
  # rasterizar unión sin solapes (1/0) en la rejilla del delta
  rq <- rasterize(vect(fincas_q), r0, field = 1, fun = "max", background = 0)
  # 100 m ⇒ 1 ha por celda: la suma son las ha únicas del quintil
  global(rq, "sum", na.rm = TRUE)[[1]]
})
names(Aq) <- paste0("Q", 1:5)
Aq


sum(Aq)





# --- Supone que ya existen: abri, caq, target_q, sample_caq_by_quint, wmean, Aq (ha por Q1..Q5) ---

# 1) Diferencia por quintil (ABRIGUE - Caquetá), ponderada por área (punto)
delta_q_point <- sapply(1:5, function(q){
  abri_q <- dplyr::filter(abri, q_area == q)
  caq_q  <- dplyr::filter(caq,  q_area == q)
  if (nrow(abri_q)==0 || nrow(caq_q)==0) return(NA_real_)
  wmean(abri_q$delta_mean_ha, abri_q$area_ha) -
    wmean(caq_q$delta_mean_ha,  caq_q$area_ha)
})

# 2) Función what-if para una tasa de adopción alpha en [0,1]
whatif_once <- function(alpha, Aq, delta_q){
  # tC = sum_q ( A_q * alpha * Δ_q )
  sum(Aq * alpha * delta_q, na.rm = TRUE)
}

# 3) Bootstrap estratificado por quintil para IC95% (remuestreando Caquetá por quintil)
set.seed(2025)
B <- 1000
boot_delta_q <- replicate(B, {
  caq_s <- sample_caq_by_quint(target_q, caq)   # tu función
  sapply(1:5, function(q){
    abri_q <- dplyr::filter(abri,  q_area == q)
    caq_q  <- dplyr::filter(caq_s, q_area == q)
    if (nrow(abri_q)==0 || nrow(caq_q)==0) return(NA_real_)
    wmean(abri_q$delta_mean_ha, abri_q$area_ha) -
      wmean(caq_q$delta_mean_ha,  caq_q$area_ha)
  })
})

# 4) Escenarios de adopción
alpha_vec <- c(0.20, 0.50, 1.00)   # 20%, 50% y 100%

# Resultados puntuales por escenario (tC y tCO2e)
whatif_point_tC   <- sapply(alpha_vec, \(a) whatif_once(a, Aq, delta_q_point))
whatif_point_CO2e <- whatif_point_tC * (44/12)

# 5) IC95% por escenario (usando las réplicas bootstrap de Δ_q)
whatif_boot_tC <- sapply(alpha_vec, function(alpha){
  # cada réplica: sum_q (A_q * alpha * Δ_q^b)
  tot_b <- apply(boot_delta_q, 2, \(dq_b) sum(Aq * alpha * dq_b, na.rm = TRUE))
  stats::quantile(tot_b, c(0.025, 0.5, 0.975), na.rm = TRUE)
})

whatif_boot_CO2e <- whatif_boot_tC * (44/12)

# 6) Tabla bonita
res_esc <- tibble::tibble(
  adopcion = scales::percent(alpha_vec),
  tC_point     = as.numeric(whatif_point_tC),
  tC_lo        = as.numeric(whatif_boot_tC[1,]),
  tC_med       = as.numeric(whatif_boot_tC[2,]),
  tC_hi        = as.numeric(whatif_boot_tC[3,]),
  tCO2e_point  = as.numeric(whatif_point_CO2e),
  tCO2e_lo     = as.numeric(whatif_boot_CO2e[1,]),
  tCO2e_med    = as.numeric(whatif_boot_CO2e[2,]),
  tCO2e_hi     = as.numeric(whatif_boot_CO2e[3,])
)

res_esc








library(readr); library(dplyr); library(stringr); library(tidyr)
library(terra); library(sf); library(exactextractr); library(purrr)

# 0.1 Leer CSV de parámetros de carbono (formato InVEST)
# columnas esperadas: lucode, LULC_name, C_above, C_below, C_soil, C_dead
param <- read_csv("parametros_caqueta.csv", show_col_types = FALSE) %>%
  mutate(lucode = as.integer(lucode),
         C_total_tCha = coalesce(C_above,0) + coalesce(C_below,0) +
           coalesce(C_soil,0)  + coalesce(C_dead,0))

# 0.2 CLC rasters 2020–2024 (ya creados)
r2024 <- r_list[[1]] %>% terra::crop(caqueta_shp, mask = TRUE)  # Cob_Region_100k_2024
r2020 <- r_list[[2]] %>% terra::crop(caqueta_shp, mask = TRUE)  # Cob_Region_100K_2020
stopifnot(compareGeom(r2020, r2024, stopOnError = TRUE))



# 1.1 Asegurar geometrías válidas/CRS
fincas_caqueta <- st_make_valid(fincas_caqueta)
fincas_abrigue <- st_make_valid(fincas_abrigue)
if (st_crs(fincas_caqueta) != st_crs(r2024)) fincas_caqueta <- st_transform(fincas_caqueta, st_crs(r2024))
if (st_crs(fincas_abrigue) != st_crs(r2024)) fincas_abrigue <- st_transform(fincas_abrigue, st_crs(r2024))

# 1.2 Función: composición por lucode (ha) y carbono total (tC) para un sf de fincas
comp_carbono_por_finca <- function(fincas_sf, id_col, r_lulc, param_tbl){
  # exact_extract con fracción de cobertura por categoría (lucode)
  # devolvemos tabla larga: una fila por finca x lucode
  vals <- exactextractr::exact_extract(raster::raster(r_lulc), fincas_sf, include_cols = id_col)
  # 'vals' es una lista; convertir a tibble con área por valor ponderada por fracción
  res_list <- map(vals, ~{
    if (is.null(.x) || nrow(.x)==0) return(tibble(value=integer(), area_ha=numeric()))
    .x %>%
      group_by(value) %>%
      summarise(area_ha = sum(coverage_fraction, na.rm=TRUE) * (res(r_lulc)[1] * res(r_lulc)[2] / 1e4),
                .groups="drop")
  })
  out <- bind_rows(res_list, .id = "row_id") %>%
    mutate(row_id = as.integer(row_id)) %>%
    # traer el ID de finca
    mutate(Finca = fincas_sf[[id_col]][row_id]) %>%
    rename(lucode = value) %>%
    filter(!is.na(lucode)) %>%
    left_join(param_tbl %>% dplyr::select(lucode, C_total_tCha), by="lucode") %>%
    mutate(C_total_finca_tC = area_ha * coalesce(C_total_tCha, 0)) %>%
    group_by(Finca) %>%
    mutate(area_total_ha = sum(area_ha, na.rm=TRUE),
           C_stock_total_tC = sum(C_total_finca_tC, na.rm=TRUE)) %>%
    ungroup()
  # Devolver: tabla larga (Finca, lucode, area_ha) y un resumen por finca
  list(
    detalle = out %>% dplyr::select(Finca, lucode, area_ha, C_total_tCha, C_total_finca_tC),
    resumen = out %>% dplyr::distinct(Finca, area_total_ha, C_stock_total_tC)
  )
}

# 1.3 Caquetá (usar código de predio; ajusta si tu campo difiere)
res_caq <- comp_carbono_por_finca(fincas_caqueta, id_col = "CODIGO", r_lulc = r2024, param_tbl = param)
comp_caq_detalle <- res_caq$detalle %>% mutate(cohorte="Caqueta")
comp_caq_resumen <- res_caq$resumen %>% mutate(cohorte="Caqueta")

# 1.4 ABRIGUE (usar 'cu' como ID)
res_abri <- comp_carbono_por_finca(fincas_abrigue, id_col = "cu", r_lulc = r2024, param_tbl = param)
comp_abri_detalle <- res_abri$detalle %>% mutate(cohorte="ABRIGUE")
comp_abri_resumen <- res_abri$resumen %>% mutate(cohorte="ABRIGUE")

# 1.5 Unión
comp_detalle <- bind_rows(comp_caq_detalle, comp_abri_detalle)
comp_resumen <- bind_rows(comp_caq_resumen, comp_abri_resumen)



# 2.1 Tipologías (porcentaje de área dentro de la finca)
tipologias <- tribble(
  ~tipo,           ~p_pasto_degrad, ~p_pasto_mejor, ~p_agrofor, ~p_bosque_sec, ~p_bosque_ref, ~p_otros,
  "Tradicional",              0.25,           0.55,       0.00,          0.05,          0.10,    0.05,
  "Trans_20",                 0.21,           0.35,       0.10,          0.18,          0.10,    0.06,
  "Trans_50",                 0.14,           0.26,       0.20,          0.24,          0.10,    0.06,
  "Trans_80",                 0.03,           0.21,       0.29,          0.27,          0.13,    0.07
)

# 2.2 Crosswalk (editar si lo deseas)
# Pastos degradados ~ 233; Pasto "mejorado" ~ mezcla 231/232; Agroforestales ~ 224;
# Bosque secundario ~ 323; Bosque de referencia ~ 311/314; Otros = el resto (excluyendo anteriores)
crosswalk <- list(
  pasto_degrad = c(233),
  pasto_mejor  = c(231, 232),
  agrofor      = c(224),
  bosque_sec   = c(323),
  bosque_ref   = c(311, 314)
)

# 2.3 Valores de stock representativos por categoría (promedio del/los lucodes mapeados)
mean_stock <- function(lucodes, param_tbl){
  param_tbl %>% filter(lucode %in% lucodes) %>% summarise(val = mean(C_total_tCha, na.rm=TRUE)) %>% pull(val) %>%
    {if(length(.)==0 || is.na(.)) 0 else .}
}

stock_cat <- tibble(
  categoria = c("pasto_degrad","pasto_mejor","agrofor","bosque_sec","bosque_ref","otros"),
  C_tCha    = c(
    mean_stock(crosswalk$pasto_degrad, param),
    mean_stock(crosswalk$pasto_mejor,  param),
    mean_stock(crosswalk$agrofor,      param),
    mean_stock(crosswalk$bosque_sec,   param),
    mean_stock(crosswalk$bosque_ref,   param),
    # 'otros' = promedio de clases no incluidas en ninguna categoría previa
    mean_stock(setdiff(param$lucode, unlist(crosswalk)), param)
  )
)
stock_cat




# 3.1 Stock objetivo por finca y tipología (usando 'comp_resumen' calculado arriba)
calc_objetivo <- function(resumen_fincas, tipologias_tbl, stock_cat_tbl){
  resumen_fincas %>%
    crossing(tipologias_tbl) %>%
    rowwise() %>%
    mutate(
      C_obj_tC = area_total_ha * (
        p_pasto_degrad * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="pasto_degrad"] +
          p_pasto_mejor  * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="pasto_mejor"]  +
          p_agrofor      * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="agrofor"]      +
          p_bosque_sec   * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="bosque_sec"]   +
          p_bosque_ref   * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="bosque_ref"]   +
          p_otros        * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="otros"]
      )
    ) %>%
    ungroup() %>%
    mutate(Delta_tC = C_obj_tC - C_stock_total_tC)
}

obj_caq <- calc_objetivo(comp_caq_resumen,  tipologias, stock_cat) %>% mutate(cohorte="Caqueta")
obj_abri<- calc_objetivo(comp_abri_resumen, tipologias, stock_cat) %>% mutate(cohorte="ABRIGUE")

# 3.2 Potencial agregado por tipología (suma de Δ)
potencial_caq <- obj_caq %>% group_by(tipo) %>%
  summarise(Delta_total_tC = sum(Delta_tC/4, na.rm=TRUE), .groups="drop") %>%
  mutate(Delta_total_tCO2e = Delta_total_tC * (44/12))

potencial_caq






set.seed(123)
adopciones <- c(0.2, 0.5, 0.8)     # 20%, 50%, 80%
objetivo   <- "Trans_80"           # puedes correr también Trans_20 / Trans_50

calc_escenario <- function(obj_tab, adop_frac, B = 500){
  # obj_tab: tabla por finca con columnas (Finca, area_total_ha, C_stock_total_tC, tipo, C_obj_tC, Delta_tC)
  base <- obj_tab %>% filter(tipo == objetivo)
  
  reps <- replicate(B, {
    n_take <- ceiling(nrow(base) * adop_frac)
    adop   <- base %>% slice_sample(n = n_take)
    sum(adop$Delta_tC, na.rm = TRUE)
  })
  tibble(
    adopcion = paste0(as.integer(adop_frac*100), "%"),
    tC_point = mean(reps), tC_lo = quantile(reps, 0.025), tC_med = median(reps), tC_hi = quantile(reps, 0.975)
  ) %>%
    mutate(
      tCO2e_point = tC_point * (44/12),
      tCO2e_lo    = tC_lo    * (44/12),
      tCO2e_med   = tC_med   * (44/12),
      tCO2e_hi    = tC_hi    * (44/12)
    )
}

res_esc <- map_dfr(adopciones, ~calc_escenario(obj_caq, .x, B = 1000))
res_esc



# Carbono total actual
comp_resumen %>% group_by(cohorte) %>%
  summarise(
    n = n(),
    area_total_ha = sum(area_total_ha, na.rm=TRUE),
    C_stock_total_tC = sum(C_stock_total_tC, na.rm=TRUE),
    C_stock_media_tCha = weighted.mean(C_stock_total_tC/area_total_ha, w=area_total_ha, na.rm=TRUE)
  )




# 1) marcar bosque / no-bosque por lucode
is_forest <- function(code) code %in% c(311,312,313,314, 315)

# 2) composición actual por finca
comp_act <- res_caq$detalle |>  # o el combinado para ambas cohortes
  mutate(grupo = ifelse(is_forest(lucode), "bosque", "no_bosque")) |>
  group_by(Finca, grupo) |>
  summarise(area = sum(area_ha), C = sum(area_ha * C_total_tCha), .groups="drop") |>
  tidyr::pivot_wider(names_from = grupo, values_from = c(area, C), values_fill = 0)

# 3) definir objetivo SOLO en no-bosque (ejemplo para Trans_80)
#    proporciones internas del no-bosque: {pasto_degrad, pasto_mejor, agrofor, secundaria, otros_nb}
#    bosque_target = area_bosque_actual (no reducir)
#    área_no_bosque_target = área_no_bosque_actual (constante)
#    reasignar dentro del no-bosque con las proporciones de Trans_80 NO-BOSQUE

# 4) traducir esas proporciones a lucode y C/ha según 'param', calcular C_obj_tC
# 5) Delta_tC = C_obj_tC - C_stock_total_tC_actual
# 6) agregar por quintil y correr adopciones





# Asumiendo que ya leíste la tabla de parámetros en 'param'
# y que trae (al menos) columnas: lucode, C_total_tCha, y ojalá C_above, C_below, C_dead, C_soil

# a) forzar C_below = 10 en 223 y 224
param <- param %>%
  mutate(C_below = if ("C_below" %in% names(.)) if_else(lucode %in% c(223,224), 10, C_below) else NA_real_)

# b) si tienes los componentes, recomputa el total; si no, ajusta el total sumando delta_below
if (all(c("C_above","C_below") %in% names(param))) {
  param <- param %>%
    mutate(C_dead = coalesce(C_dead, 0),
           C_soil = coalesce(C_soil, 0),
           C_total_tCha = C_above + C_below + C_dead + C_soil)
} else {
  # ajuste conservador si no hay desagregados
  lookup_old <- param %>% dplyr::select(lucode, C_total_tCha) %>% rename(C_total_old = C_total_tCha)
  # aquí no podemos calcular el delta exacto sin C_below_old; si tu archivo trae C_below_old, úsalo.
  # de lo contrario, deja C_total_tCha como venía (ya que lo usas como "stock representativo").
}

# diccionario actualizado por código
stock_lookup <- param %>% dplyr::select(lucode, C_total_tCha)


library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# -------------------------------
# 1) Parámetros InVEST: actualizar y recomputar C_total_tCha
# -------------------------------
# param <- read_csv("parametros_caqueta.csv", show_col_types = FALSE) %>%
#   rename_with(tolower)

param <- param %>% rename_with(tolower)

stopifnot(all(c("lucode","c_above","c_below","c_soil","c_dead") %in% names(param)))

param <- param %>%
  mutate(
    lucode  = as.integer(lucode),
    # Solicitud: poner C_below = 10 para 223 y 224
    c_below = if_else(lucode %in% c(223L, 224L), 10, c_below),
    # Recalcular stock total por hectárea (tC/ha)
    c_total_tcha = c_above + c_below + c_soil + c_dead
  )

# (opcional) guardar copia actualizada
# write_csv(param, "parametros_caqueta_actualizado.csv")

# -------------------------------
# 2) Definiciones para escenarios: congelar bosque, mejorar no-bosque
# -------------------------------
forest_codes <- c(311L, 312L, 313L, 314L, 315L)    # bosque "congelado"

# Crosswalk de categorías NO BOSQUE (las que sí se reconfiguran)
crosswalk <- list(
  pasto_degrad = c(233L),
  pasto_mejor  = c(231L, 232L),
  agrofor      = c(224L)
)
nonforest_all <- setdiff(param$lucode, c(unlist(crosswalk), forest_codes))
# 'otros' = todo no-bosque que no sea pasto/agrofor
crosswalk$otros <- nonforest_all

# Promedios de stock por categoría (tC/ha) a partir de la tabla de parámetros actualizada
mean_stock <- function(lcodes, param_tbl){
  param_tbl %>% filter(lucode %in% lcodes) %>%
    summarise(val = mean(c_total_tcha, na.rm = TRUE), .groups="drop") %>%
    pull(val) %>% { if(length(.)==0 || is.na(.)) 0 else . }
}

stock_cat <- tibble(
  categoria = c("pasto_degrad","pasto_mejor","agrofor","otros"),
  C_tCha    = c(
    mean_stock(crosswalk$pasto_degrad, param),
    mean_stock(crosswalk$pasto_mejor,  param),
    mean_stock(crosswalk$agrofor,      param),
    mean_stock(crosswalk$otros,        param)
  )
)

# -------------------------------
# 3) Función de objetivo con "bosque congelado"
#    - Usa comp_detalle (área por lucode y finca) y comp_resumen (área total y stock actual por finca)
#    - Reconfigura SOLO el no-bosque según las tipologías (normalizando porcentajes a la fracción no-bosque)
# -------------------------------
calc_objetivo_freeze <- function(detalle_fincas, resumen_fincas, tipologias_tbl,
                                 stock_cat_tbl, forest_codes = c(311L,312L,313L,314L,315L)) {
  
  # a) Bosque actual por finca (área y carbono) — se mantiene
  bosque_by_finca <- detalle_fincas %>%
    mutate(is_forest = lucode %in% forest_codes) %>%
    group_by(Finca) %>%
    summarise(
      area_bosque_ha = sum(area_ha[is_forest], na.rm = TRUE),
      C_bosque_tC    = sum((area_ha * C_total_tCha)[is_forest], na.rm = TRUE),
      .groups = "drop"
    )
  
  base <- resumen_fincas %>%
    left_join(bosque_by_finca, by = "Finca") %>%
    mutate(
      area_bosque_ha  = coalesce(area_bosque_ha, 0),
      C_bosque_tC     = coalesce(C_bosque_tC, 0),
      area_nobosque_ha = pmax(area_total_ha - area_bosque_ha, 0)
    )
  
  # b) Normalizar proporciones SOLO dentro del no-bosque
  #    (tomando de tipologías los porcentajes de pasto_degrad, pasto_mejor, agrofor y otros)
  #    -> bosque (ref/sec) queda como está
  res <- base %>%
    crossing(tipologias_tbl) %>%
    rowwise() %>%
    mutate(
      # suma de fracciones no-bosque en la tipología
      s_nobosque = p_pasto_degrad + p_pasto_mejor + p_agrofor + p_otros,
      # normalización (si s_nobosque=0, deja todo en "otros" por seguridad)
      w_pd = ifelse(s_nobosque > 0, p_pasto_degrad / s_nobosque, 0),
      w_pm = ifelse(s_nobosque > 0, p_pasto_mejor  / s_nobosque, 0),
      w_ag = ifelse(s_nobosque > 0, p_agrofor      / s_nobosque, 0),
      w_ot = ifelse(s_nobosque > 0, p_otros        / s_nobosque, 1),
      # carbono por ha objetivo en el no-bosque
      C_nobosq_tCha = w_pd * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="pasto_degrad"] +
        w_pm * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="pasto_mejor"]  +
        w_ag * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="agrofor"]      +
        w_ot * stock_cat_tbl$C_tCha[stock_cat_tbl$categoria=="otros"],
      # carbono objetivo total = bosque actual (congelado) + no-bosque reconfigurado
      C_obj_tC = C_bosque_tC + area_nobosque_ha * C_nobosq_tCha,
      Delta_tC = C_obj_tC - C_stock_total_tC
    ) %>%
    ungroup()
  
  res
}

# -------------------------------
# 4) Ejecutar con tus objetos actuales
#    (asumiendo que ya existen comp_detalle y comp_resumen de tu corrida previa)
# -------------------------------
stopifnot(exists("comp_detalle"), exists("comp_resumen"), exists("tipologias"))

# comp_detalle debe tener: Finca, lucode, area_ha, C_total_tCha
# Si no trae C_total_tCha por fila, lo añadimos desde 'param':
if (!"C_total_tCha" %in% names(comp_detalle)) {
  comp_detalle <- comp_detalle %>%
    left_join(param %>% dplyr::select(lucode, C_total_tCha = c_total_tcha), by = "lucode")
} else {
  # asegurar que usamos el stock actualizado
  comp_detalle <- comp_detalle %>%
    dplyr::select(-C_total_tCha) %>%
    left_join(param %>% dplyr::select(lucode, C_total_tCha = c_total_tcha), by = "lucode")
}

# Asegurar que comp_resumen trae el stock actual base (si lo quieres recalcular, vuelve a correr tu función de extracción)
comp_resumen <- comp_resumen %>%
  rename(C_stock_total_tC = C_stock_total_tC, area_total_ha = area_total_ha)

# Calcular objetivo por finca y tipología (bosque congelado)
obj_freeze <- calc_objetivo_freeze(
  detalle_fincas = comp_detalle,
  resumen_fincas = comp_resumen,
  tipologias_tbl = tipologias,
  stock_cat_tbl  = stock_cat,
  forest_codes   = forest_codes
)

# Potencial agregado por tipología (Δ total tC y tCO2e)
potencial_freeze <- obj_freeze %>%
  group_by(tipo) %>%
  summarise(Delta_total_tC = sum(Delta_tC/4, na.rm=TRUE), .groups="drop") %>%
  mutate(Delta_total_tCO2e = Delta_total_tC * (44/12))

potencial_freeze




set.seed(123)
adopciones <- c(0.2, 0.5, 0.8)
objetivo   <- "Trans_80"   # puedes cambiar a "Trans_20" o "Trans_50"

calc_escenario_freeze <- function(obj_tab, adop_frac, objetivo, B = 1000) {
  base <- obj_tab %>% filter(tipo == objetivo)
  
  reps <- replicate(B, {
    n_take <- ceiling(nrow(base) * adop_frac)
    adop   <- base %>% slice_sample(n = n_take)
    sum(adop$Delta_tC, na.rm = TRUE)
  })
  
  tibble(
    adopcion    = paste0(as.integer(adop_frac*100), "%"),
    tC_point    = mean(reps),
    tC_lo       = quantile(reps, 0.025),
    tC_med      = median(reps),
    tC_hi       = quantile(reps, 0.975)
  ) %>%
    mutate(
      tCO2e_point = tC_point * (44/12),
      tCO2e_lo    = tC_lo    * (44/12),
      tCO2e_med   = tC_med   * (44/12),
      tCO2e_hi    = tC_hi    * (44/12)
    )
}

res_esc_freeze <- map_dfr(adopciones, ~calc_escenario_freeze(obj_freeze, .x, objetivo, B = 1000))
res_esc_freeze



# base: obj_freeze ya tiene Delta_tC (acumulado 2020–2024)
calc_escenario_freeze <- function(obj_freeze, adop_frac, objetivo, B = 1000){
  base <- obj_freeze %>%
    dplyr::filter(tipo == objetivo) %>%
    dplyr::mutate(Delta_tC_year = Delta_tC/4)  # <- anualiza AQUÍ
  
  reps <- replicate(B, {
    n_take <- ceiling(nrow(base) * adop_frac)
    adop   <- dplyr::slice_sample(base, n = n_take, replace = FALSE)
    sum(adop$Delta_tC_year, na.rm = TRUE)
  })
  
  tibble::tibble(
    adopcion = paste0(as.integer(adop_frac*100), "%"),
    tC_point = mean(reps),
    tC_lo    = quantile(reps, 0.025),
    tC_med   = median(reps),
    tC_hi    = quantile(reps, 0.975)
  ) |>
    dplyr::mutate(
      tCO2e_point = tC_point * (44/12),
      tCO2e_lo    = tC_lo    * (44/12),
      tCO2e_med   = tC_med   * (44/12),
      tCO2e_hi    = tC_hi    * (44/12)
    )
}

cap_100 <- potencial_freeze |>
  dplyr::filter(tipo == "Trans_80") |>
  dplyr::mutate(cap_tC_year = Delta_total_tC)  # ya anual en tu tabla

res_esc_freeze |>
  dplyr::left_join(
    tibble::tibble(
      adopcion = c("20%","50%","80%"),
      f = c(.2,.5,.8)
    ),
    by = "adopcion"
  ) |>
  dplyr::mutate(
    bound_tC = f * cap_100$cap_tC_year,
    ok = abs(tC_med) <= abs(bound_tC) + 1e-6
  )
# 'ok' debe ser TRUE en todos los renglones




