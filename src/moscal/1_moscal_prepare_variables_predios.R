## Prepare data MoSCAL - variables - cobertura

library(tidyverse)
library(sf)
library(terra)
library(naniar)
library(patchwork)

# sf::sf_use_s2(FALSE)

# ========================== 1) Rutas ==========================
file_shp_dpto_mpio <- "data/UER/CPai2014_CCar2014_CDep2014_CMun2014_CElt2019.shp"
gdb_base <- "data/Moscal/1KPre_Hist_CAso_KPre_IGACv2.gdb/KPre_Hist_CAso_KPre_IGACv2.gdb"
gdb_moscal <- "data/Moscal/KPre_Moscal.gdb/KPre_Moscal.gdb"
gdb_testigo <- "data/Moscal/KPre_Testigo_Moscal.gdb"

c(gdb_base, gdb_moscal, gdb_testigo) %>% map(sf::st_layers)

pB_min <- 0.05
pP_min <- 0.05
area_min <- 1


# ========================== 2) Lectura ==========================
# Limites
limites_adm           <- sf::read_sf(file_shp_dpto_mpio)
area_asociacion_raw   <- sf::st_read(gdb_base,      layer = "CAso_Hist")

# capa de poligonos pertenecientes a Moscal
pred_con_raw          <- sf::st_read(gdb_moscal,      layer = "Cb_PRE") # requiere unir geometrias por cu
#group_by(cu) %>% summarise(geometry = st_union(Shape), .groups="drop") 

# capa de poligonos IGAC , sin moscal
pred_sin_raw         <- sf::st_read(gdb_testigo,      layer = "Cb_PRE") # requiere unir geometrias por cu
#group_by(cu) %>% summarise(geometry = st_union(shape), .groups="drop") 


# Coberturas
# bosque_* = contiene la cobertura de bosque (nom_variable) de cada predio (cu) en cada monitoreo (periodo)
# cobertura_* = contiene la cobertura de "Pastos","Vegetación Secundaria", "Zonas Quemadas" ($cobertura  -$nom_variable) de cada predio (cu) en cada monitoreo (periodo)
bosque_moscal         <- sf::st_read(gdb_moscal,      layer = "LB_ABosque_PRE")
cobertura_moscal         <- sf::st_read(gdb_moscal,      layer = "LB_Cob_PRE")

bosque_testigo         <- sf::st_read(gdb_testigo,      layer = "LB_ABosque_PRE")
cobertura_testigo         <- sf::st_read(gdb_testigo,      layer = "LB_Cob_PRE")



# ========================== Parche 2.0: Panel 2017–2024 ==========================
# Requiere: sf, dplyr, tidyr, stringr, purrr, readr (para parseo de periodo)
# ------------------------------------------------------------------------------
# 0) Setup
sf::sf_use_s2(FALSE)

# --- Helpers geom ---------------------------------------------------------------
# Activa geometry "Shape"/"shape", borra Z/M, transforma a 3116, valida
as_poly_3116 <- function(x) {
  x <- sf::st_as_sf(x)
  gcol <- dplyr::case_when(
    "Shape" %in% names(x) ~ "Shape",
    "shape" %in% names(x) ~ "shape",
    TRUE ~ attr(sf::st_geometry(x), "sf_column") %||% "geometry"
  )
  sf::st_geometry(x) <- x[[gcol]]
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  x <- sf::st_transform(x, 3116)
  # Mantén únicamente áreas
  gt <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
  x  <- x[gt %in% c("POLYGON","MULTIPOLYGON","CURVEPOLYGON","SURFACE",
                    "MULTISURFACE"), ]
  # Cast seguro + validación
  suppressWarnings({
    sf::st_geometry(x) <- sf::st_cast(sf::st_geometry(x), "MULTIPOLYGON", warn = FALSE)
  })
  x <- sf::st_make_valid(x)
  x
}

# Una geometría por cu (trazabilidad), conserva columnas clave
unify_by_cu <- function(x, cu = "cu", keep_cols = c("area_uer_ha","nombre_uer_padre","proyecto")) {
  x <- as_poly_3116(x)
  # si hay varias filas por cu, unir geometrías
  cols <- intersect(keep_cols, names(x))
  if (any(c("Shape", "shape") %in% names(x))) sf::st_geometry(x) <- "Shape"
  
  x |>
    dplyr::select(dplyr::all_of(unique(c(cols,"Shape")))) |>
    dplyr::group_by(.data[[cu]]) |>
    dplyr::summarise(
      dplyr::across(-Shape, ~ dplyr::first(., default = NA)),
      geometry = sf::st_union(Shape),
      .groups = "drop"
    ) |>
    dplyr::rename(cu = dplyr::all_of(cu))
}

# Centroides en 3116 para lon/lat (en metros, pero útiles como coords de control)
centroid_xy <- function(sf_predios) {
  ctd <- sf::st_centroid(sf_predios)
  xy  <- sf::st_coordinates(ctd)
  tibble::tibble(cu = sf_predios$cu, lon = xy[,1], lat = xy[,2])
}

# --- Helpers de cobertura -------------------------------------------------------
# Parche: build_panel_side() robusto y sin dependencias espaciales
# - No realiza operaciones espaciales ni joins sf
# - Acepta objetos sf en cualquier CRS; descarta geometría al inicio
# - Homologa coberturas (Pastos / Vegetacion secundaria / Zonas Quemadas)
# - Usa porc_bosq_uer y porc_cob_uer si existen; si no, cae a áreas/area_uer_ha
# - Re-cierra a 1 cuando el cierre está en [0.98, 1.02]
# - Devuelve data.frame (no-sf) con: cu, periodo (ordenable), area_uer_ha, pB,pP,pV,pO, cierre, frac_mapeada, grupo
# - pO se define estrictamente como residuo del área: pO = 1 - (pB + pP + pV + pQ) con clamp a [0,1]

`%||%` <- function(a,b) if (is.null(a)) b else a

# Normaliza etiqueta de cobertura a 3 clases (según tus capas)
normalize_cob <- function(x) {
  x <- stringr::str_trim(as.character(x))
  dplyr::case_when(
    x == "Pastos" ~ "Pastos",
    x == "Vegetación Secundaria" ~ "Vegetacion secundaria",
    x == "Vegetacion Secundaria" ~ "Vegetacion secundaria",
    x == "Zonas Quemadas" ~ "Zonas Quemadas",
    TRUE ~ "Otros"
  )
}

# Orden de periodos tipo "YYYY_MES"  con MES ∈ {ENE, ABR, JUL, OCT}
orden_periodo <- function(x) {
  x <- as.character(x)
  # mapa de meses robusto (abreviaturas ES)
  mes_map <- c(ENE="01", FEB="02", MAR="03", ABR="04", MAY="05", JUN="06",
               JUL="07", AGO="08", SEP="09", OCT="10", NOV="11", DIC="12")
  tb <- tibble::tibble(p = x)
  tb <- tidyr::separate_wider_delim(tb, "p", delim = "_",
                                    names = c("anio","mes"), cols_remove = FALSE)
  mes_num <- dplyr::recode(tb$mes, !!!mes_map, .default = "12")
  # clave ordenable AAAA-MM; usa también texto para desempatar y asegurar orden estable
  ord_key <- paste0(tb$anio, "-", mes_num, "-", tb$mes)
  lvls <- tb$p[order(ord_key, tb$p)]
  lvls <- unique(lvls)  # <- evita niveles duplicados
  factor(x, levels = lvls)
}

# Re-cierre a 1 si el cierre está entre 0.98 y 1.02
close_to_one <- function(df, cols = c("pB","pP","pV", "pQ","pO")) {
  s <- rowSums(df[, cols, drop = FALSE], na.rm = TRUE)
  ok <- !is.na(s) & s >= 0.98 & s <= 1.02 & s > 0
  if (any(ok)) df[ok, cols] <- df[ok, cols] / s[ok]
  df$cierre <- rowSums(df[, cols, drop = FALSE], na.rm = TRUE)
  df
}

# ========================= build_panel_side =========================
# bosque_sf: LB_ABosque_PRE  (sf o data.frame)
# cob_sf   : LB_Cob_PRE      (sf o data.frame)
# grupo_label: "CON_PROYECTO" | "SIN_PROYECTO"
build_panel_side <- function(bosque_sf, cob_sf, grupo_label = c("CON_PROYECTO","SIN_PROYECTO")) {
  grupo_label <- match.arg(grupo_label)
  
  # --- descartar geometría si viene como sf (evita errores de CRS/join) ---
  if (inherits(bosque_sf, "sf")) bosque_sf <- sf::st_drop_geometry(bosque_sf)
  if (inherits(cob_sf,    "sf")) cob_sf    <- sf::st_drop_geometry(cob_sf)
  
  # --- BOSQUE ---
  B <- bosque_sf |>
    dplyr::mutate(
      periodo   = as.character(.data$periodo),
      area_uer_ha = suppressWarnings(as.numeric(.data$area_uer_ha)),
      porc_bosq_uer = suppressWarnings(as.numeric(.data$porc_bosq_uer)),
      a_uer_b_ha    = suppressWarnings(as.numeric(.data$a_uer_b_ha))
    ) |>
    dplyr::group_by(cu, periodo, area_uer_ha) |>
    dplyr::summarise(pB = sum(a_uer_b_ha, na.rm = TRUE) / max(area_uer_ha, na.rm = TRUE), .groups = "drop")
  
  # --- COBERTURAS (no bosque) ---
  C <- cob_sf |>
    dplyr::mutate(
      periodo      = as.character(.data$periodo),
      cobertura    = normalize_cob(.data$cobertura),
      area_uer_ha  = suppressWarnings(as.numeric(.data$area_uer_ha)),
      porc_cob_uer = suppressWarnings(as.numeric(.data$porc_cob_uer)),
      a_cob_uer_ha = suppressWarnings(as.numeric(.data$a_cob_uer_ha))
    ) |>
    dplyr::group_by(cu, periodo, area_uer_ha, cobertura) |>
    dplyr::summarise(a_c = sum(a_cob_uer_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(prop = dplyr::if_else(area_uer_ha > 0, a_c / area_uer_ha, NA_real_)) |>
    dplyr::select(-a_c) |>
    tidyr::pivot_wider(names_from = cobertura, values_from = prop, values_fill = 0) |>
    dplyr::mutate(
      pP = .data$Pastos %||% 0,
      pV = (.data$`Vegetacion secundaria` %||% 0),
      pQ = (.data$`Zonas Quemadas` %||% 0)
    ) |>
    dplyr::select(cu, periodo, pP, pV, pQ)
  
  # --- UNION y cierre ---
  DF <- dplyr::left_join(B, C, by = c("cu","periodo")) |>
    dplyr::mutate(
      pP = tidyr::replace_na(.data$pP, 0),
      pV = tidyr::replace_na(.data$pV, 0),
      pQ = tidyr::replace_na(.data$pQ, 0)
    ) |>
    dplyr::mutate(
      # pO como residuo: (área predio - bosque - pastos - veg. secundaria - quemada) / área predio
      pO = pmax(0, 1 - (.data$pB + .data$pP + .data$pV + .data$pQ))
    )
  DF$cierre <- with(DF, pB + pP + pV + pO + pQ)
  DF <- close_to_one(DF, c("pB","pP","pV", "pQ", "pO"))
  
  # --- Fracción mapeada (áreas sumadas) ---
  A_b <- bosque_sf |>
    dplyr::select(cu, periodo, a_uer_b_ha) |>
    dplyr::group_by(cu, periodo) |>
    dplyr::summarise(a_b = sum(suppressWarnings(as.numeric(a_uer_b_ha)), na.rm = TRUE), .groups = "drop")
  
  A_c <- cob_sf |>
    dplyr::select(cu, periodo, a_cob_uer_ha) |>
    dplyr::group_by(cu, periodo) |>
    dplyr::summarise(a_c = sum(suppressWarnings(as.numeric(a_cob_uer_ha)), na.rm = TRUE), .groups = "drop")
  
  A <- dplyr::full_join(A_b, A_c, by = c("cu","periodo")) |>
    dplyr::mutate(a_b = tidyr::replace_na(.data$a_b, 0),
                  a_c = tidyr::replace_na(.data$a_c, 0),
                  a_map = .data$a_b + .data$a_c)
  
  DF <- dplyr::left_join(DF, A, by = c("cu","periodo")) |>
    dplyr::mutate(
      frac_mapeada = dplyr::if_else(.data$area_uer_ha > 0,
                                    pmin(1.02, (.data$a_map %||% 0) / .data$area_uer_ha),
                                    NA_real_),
      grupo = grupo_label
    )
  
  # --- Ordena periodo ---
  DF$periodo <- orden_periodo(DF$periodo)
  
  # Salida limpia
  dplyr::select(DF, cu, periodo, area_uer_ha, pB, pP, pV, pQ, pO, cierre, frac_mapeada, grupo)
}

# ========================= Filtros de control de calidad =========================
# 1) Cierre en [0.98, 1.02] o NA (permite re-cierre previo)
filter_cierre_ok <- function(df, tol = 0.02) {
  df |> dplyr::filter(is.na(.data$cierre) | (.data$cierre >= 1 - tol & .data$cierre <= 1 + tol))
}

# 2) Fracción mapeada mínima (p.ej., ≥ 0.95)
filter_frac_mapeada <- function(df, thr = 0.95) {
  df |> dplyr::filter(is.na(.data$frac_mapeada) | .data$frac_mapeada >= thr)
}

# 3) Área mínima de predio (ha) para evitar outliers muy pequeños
filter_area_min <- function(df, area_min_ha = 1) {
  df |> dplyr::filter(is.na(.data$area_uer_ha) | .data$area_uer_ha >= area_min_ha)
}

# 4) Enmascarar valores negativos o >1 después de residuo (sólo por seguridad)
clamp_props01 <- function(df) {
  for (v in c("pB","pP","pV", "pQ", "pO")) df[[v]] <- pmin(pmax(df[[v]], 0), 1)
  df
}



build_panel_side <- function(bosque_sf, cob_sf, grupo_label = c("CON_PROYECTO","SIN_PROYECTO")) {
  grupo_label <- match.arg(grupo_label)
  
  # --- descartar geometría si viene como sf ---
  if (inherits(bosque_sf, "sf")) bosque_sf <- sf::st_drop_geometry(bosque_sf)
  if (inherits(cob_sf,    "sf")) cob_sf    <- sf::st_drop_geometry(cob_sf)
  
  # --- BOSQUE ---
  B <- bosque_sf |>
    dplyr::mutate(
      periodo       = as.character(.data$periodo),
      asoc          = as.character(.data$nombre_uer_padre),
      area_uer_ha   = suppressWarnings(as.numeric(.data$area_uer_ha)),
      porc_bosq_uer = suppressWarnings(as.numeric(.data$porc_bosq_uer)),
      a_uer_b_ha    = suppressWarnings(as.numeric(.data$a_uer_b_ha))
    ) |>
    dplyr::group_by(cu, asoc, periodo, area_uer_ha) |>
    dplyr::summarise(
      pB = sum(.data$a_uer_b_ha, na.rm = TRUE) / max(.data$area_uer_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- COBERTURAS (no bosque) ---
  C <- cob_sf |>
    dplyr::mutate(
      periodo        = as.character(.data$periodo),
      asoc           = as.character(.data$nombre_uer_padre),
      cobertura      = normalize_cob(.data$cobertura),
      area_uer_ha    = suppressWarnings(as.numeric(.data$area_uer_ha)),
      porc_cob_uer   = suppressWarnings(as.numeric(.data$porc_cob_uer)),
      a_cob_uer_ha   = suppressWarnings(as.numeric(.data$a_cob_uer_ha))
    ) |>
    dplyr::group_by(cu, asoc, periodo, area_uer_ha, cobertura) |>
    dplyr::summarise(a_c = sum(.data$a_cob_uer_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(prop = dplyr::if_else(.data$area_uer_ha > 0, .data$a_c / .data$area_uer_ha, NA_real_)) |>
    dplyr::select(-a_c) %>%
    tidyr::pivot_wider(names_from = cobertura, values_from = prop, values_fill = 0) |>
    dplyr::mutate(
      pP = rlang::`%||%`(.data$Pastos, 0),
      pV = rlang::`%||%`(.data$`Vegetacion secundaria`, 0),
      pQ = rlang::`%||%`(.data$`Zonas Quemadas`, 0)
    ) |>
    dplyr::select(cu, asoc, periodo, pP, pV, pQ)
  
  # --- UNION y cierre ---
  DF <- dplyr::left_join(C, B, by = c("cu","asoc","periodo")) |>
    dplyr::mutate(
      pP = tidyr::replace_na(.data$pP, 0),
      pV = tidyr::replace_na(.data$pV, 0),
      pQ = tidyr::replace_na(.data$pQ, 0),
      pO = pmax(0, 1 - (.data$pB + .data$pP + .data$pV + .data$pQ))
    )
  DF$cierre <- with(DF, pB + pP + pV + pO + pQ)
  DF <- close_to_one(DF, c("pB","pP","pV", "pQ","pO"))
  
  # --- Fracción mapeada (áreas sumadas) ---
  A_b <- bosque_sf |>
    dplyr::mutate(asoc = as.character(.data$nombre_uer_padre)) |>
    dplyr::select(cu, asoc, periodo, a_uer_b_ha, area_uer_ha) |>
    dplyr::group_by(cu, asoc, periodo) |>
    dplyr::summarise(a_b = sum(suppressWarnings(as.numeric(.data$a_uer_b_ha)), na.rm = TRUE),
                     area_uer_ha_b = max(suppressWarnings(as.numeric(.data$area_uer_ha)), na.rm = TRUE),
                     .groups = "drop")
  
  A_c <- cob_sf |>
    dplyr::mutate(asoc = as.character(.data$nombre_uer_padre)) |>
    dplyr::select(cu, asoc, periodo, a_cob_uer_ha, area_uer_ha) |>
    dplyr::group_by(cu, asoc, periodo) |>
    dplyr::summarise(a_c = sum(suppressWarnings(as.numeric(.data$a_cob_uer_ha)), na.rm = TRUE),
                     area_uer_ha_c = max(suppressWarnings(as.numeric(.data$area_uer_ha)), na.rm = TRUE),
                     .groups = "drop")
  
  A <- dplyr::full_join(A_b, A_c, by = c("cu","asoc","periodo")) |>
    dplyr::mutate(
      a_b  = tidyr::replace_na(.data$a_b, 0),
      a_c  = tidyr::replace_na(.data$a_c, 0),
      a_map = .data$a_b + .data$a_c,
      # área base preferida: la de bosque; si falta, la de coberturas
      area_base = dplyr::coalesce(.data$area_uer_ha_b, .data$area_uer_ha_c)
    ) |>
    dplyr::select(cu, asoc, periodo, a_map, area_base)
  
  DF <- dplyr::left_join(DF, A, by = c("cu","asoc","periodo")) |>
    dplyr::mutate(
      # si DF$area_uer_ha existe (viene de B), úsala; si no, cae a area_base
      area_uer_ha = dplyr::coalesce(.data$area_uer_ha, .data$area_base),
      frac_mapeada = dplyr::if_else(.data$area_uer_ha > 0,
                                    pmin(1.02, (.data$a_map %||% 0) / .data$area_uer_ha),
                                    NA_real_),
      grupo = grupo_label
    ) |>
    dplyr::select(-a_map, -area_base)
  
  # --- Ordena periodo ---
  DF$periodo <- orden_periodo(DF$periodo)
  
  # Salida limpia (incluye asoc)
  dplyr::select(DF, cu, asoc, periodo, area_uer_ha, pB, pP, pV, pQ, pO, cierre, frac_mapeada, grupo)
}




# ========================= Ejemplo de uso =========================
# bosque_sf <- bosque_moscal
# cob_sf <- cobertura_moscal

panel_con <- build_panel_side(bosque_moscal,  cobertura_moscal, grupo_label =  "CON_PROYECTO")
panel_sin <- build_panel_side(bosque_testigo, cobertura_testigo, "SIN_PROYECTO")
panel_raw <- dplyr::bind_rows(panel_con, panel_sin)
panel_ok  <- panel_raw |> clamp_props01() |> 
  filter_cierre_ok(tol = 0.1) |> 
  filter_frac_mapeada(thr = 0.9) |> filter_area_min(area_min_ha = 1)
str(panel_ok)






# ========================== 1) Lectura (tus rutas ya definidas) ==================
# Usa los objetos ya cargados en tu sesión:
# limites_adm, area_asociacion_raw, pred_con_raw, pred_sin_raw,
# bosque_moscal, cobertura_moscal, bosque_testigo, cobertura_testigo

# ========================== 2) Geometrías por cu ================================
pred_con_1cu <- unify_by_cu(pred_con_raw,
                            keep_cols = c("cu","area_uer_ha","nombre_uer_padre","proyecto"))
pred_sin_1cu <- unify_by_cu(pred_sin_raw,
                            keep_cols = c("cu","area_uer_ha","nombre_uer_padre","proyecto"))

xy_con <- centroid_xy(pred_con_1cu)
xy_sin <- centroid_xy(pred_sin_1cu)

# # ========================== 3) Panel por lado (CON / SIN) ======================
# panel_con <- build_panel_side(bosque_moscal,   cobertura_moscal,   "CON_PROYECTO")
# panel_sin <- build_panel_side(bosque_testigo,  cobertura_testigo,  "SIN_PROYECTO")
# 
# ========================== 4) Unificación + coords ============================
# Área/coords finales desde la capa de predios (no desde LB_*_PRE)
meta_con <- pred_con_1cu |>
  sf::st_drop_geometry() |>
  dplyr::select(cu, area_uer_ha, asoc = nombre_uer_padre)
meta_sin <- pred_sin_1cu |>
  sf::st_drop_geometry() |>
  dplyr::select(cu, area_uer_ha, asoc = nombre_uer_padre)

panel_con <- panel_con  |> clamp_props01() |> filter_cierre_ok(tol = 0.1) |> filter_frac_mapeada(thr = 0.9) |> filter_area_min(area_min_ha = 1) |> 
  dplyr::select(cu, periodo, area_uer_ha, pB, pP, pV, pO, pQ, cierre, frac_mapeada, grupo) |>
  dplyr::left_join(meta_con, by = "cu", suffix = c("", "_pred")) |>
  dplyr::mutate(
    area_uer_ha = dplyr::coalesce(.data$area_uer_ha_pred, .data$area_uer_ha)
  ) |>
  dplyr::select(-area_uer_ha_pred) |>
  dplyr::left_join(xy_con, by = "cu")

panel_sin <- panel_sin |> clamp_props01() |> filter_cierre_ok(tol = 0.1) |> filter_frac_mapeada(thr = 0.9) |> filter_area_min(area_min_ha = 1) |>
  dplyr::select(cu, periodo, area_uer_ha, pB, pP, pV, pO, pQ, cierre, frac_mapeada, grupo) |>
  dplyr::left_join(meta_sin, by = "cu", suffix = c("", "_pred")) |>
  dplyr::mutate(
    area_uer_ha = dplyr::coalesce(.data$area_uer_ha_pred, .data$area_uer_ha)
  ) |>
  dplyr::select(-area_uer_ha_pred) |>
  dplyr::left_join(xy_sin, by = "cu")

panel_predio_periodo <- dplyr::bind_rows(panel_con, panel_sin) |>
  dplyr::mutate(
    cierre = pB + pP + pV + pO + pQ
  ) |>
  close_to_one(cols = c("pB","pP","pV","pO", "pQ"))

# ========================== 5) QA básica =======================================
# a) Cierre por asoc × periodo
qa_cierre <- panel_predio_periodo |>
  dplyr::group_by(asoc, periodo) |>
  dplyr::summarise(min = min(cierre, na.rm=TRUE),
                   max = max(cierre, na.rm=TRUE),
                   .groups="drop")

# b) Fracción mapeada (para decidir umbral p.ej. 0.95)
qa_frac <- panel_predio_periodo |>
  dplyr::group_by(grupo) |>
  dplyr::summarise(
    p25 = stats::quantile(frac_mapeada, .25, na.rm=TRUE),
    p50 = stats::quantile(frac_mapeada, .50, na.rm=TRUE),
    p75 = stats::quantile(frac_mapeada, .75, na.rm=TRUE),
    .groups="drop"
  )

print(qa_cierre); print(qa_frac)


# c) Imputación múltiple ---------------
library(mice)
# 1.1  Crea un data.frame solo con las variables a imputar
vars_to_impute <- panel_predio_periodo %>% 
  dplyr::select(pB:pQ)

# 1.2  Revisa la proporción de NA (ya sabemos que es ≤30 %)
naniar::miss_var_summary(vars_to_impute)

# 2.1  Objeto inicial (m = 0 solo como 'dry‑run')
skimr::skim(vars_to_impute)

## mice alg
ini <- mice(vars_to_impute)  # ahora sí funciona

# vis_miss(complete(ini))
impute_data <- complete(ini)

# ========================== 6) Dataset listo para análisis ======================
# Filtro principal sugerido (ajusta a 0.95 / 0.90 / 0.99 en sensibilidades)
dat_imputed <- panel_predio_periodo |>
  dplyr::select(cu, asoc, grupo, periodo, lat, lon, area_uer_ha, frac_mapeada) |>
  bind_cols(impute_data) 

table(panel_predio_periodo$grupo, panel_predio_periodo$asoc)
table(dat_imputed$grupo, dat_imputed$asoc)


dat_imputed |>
  dplyr::mutate(cierre_chk = pB + pP + pV + pO + pQ) |>
  dplyr::summarise(min = min(cierre_chk, na.rm=TRUE),
                   p25 = quantile(cierre_chk,.25,na.rm=TRUE),
                   p50 = median(cierre_chk,na.rm=TRUE),
                   p75 = quantile(cierre_chk,.75,na.rm=TRUE),
                   max = max(cierre_chk, na.rm=TRUE))


# >>> 'dat' es el insumo directo para:
# - Transversal por periodo (PERMANOVA/ilr-MANCOVA/Bootstrap pp)
# - Pool multi-año con FE de periodo y EBAL + soporte común
# - Longitudinal (DiD ilr) con FE_cu + FE_periodo (+ s(lon,lat))

# save(dat, file = "moscal_outputs/dat_predios.RData")



# dat %>%
#   pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ, pO)) %>%
#   ggplot(aes(x = grupo, y = value, fill = grupo)) +
#   geom_boxplot(outlier.alpha = 0.3) +
#   facet_wrap(~ name, scales = "free_y") +
#   theme_minimal(base_size = 13) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   ) +
#   labs(
#     x = "Grupo",
#     y = "Valor",
#     title = "Distribución de variables por grupo",
#     subtitle = "Boxplots facetados usando todos los datos"
#   )


# dat %>%
#   pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ)) %>%
#   ggplot(aes(x = value, fill = grupo)) +
#   geom_density(alpha = 0.4) +
#   facet_wrap(~ name, scales = "free") +
#   theme_minimal(base_size = 13) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     legend.position = "top"
#   ) +
#   labs(
#     x = "Valor",
#     y = "Densidad",
#     title = "Distribución de variables por grupo",
#     subtitle = "Curvas de densidad facetadas usando todos los datos"
#   )
# 
# 
# dat %>%
#   pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ)) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram(
#     bins = 30, 
#     fill = "steelblue", 
#     color = "white", 
#     alpha = 0.7
#   ) +
#   facet_wrap(name ~ grupo, scales = "free") +
#   theme_minimal(base_size = 13) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     panel.grid.minor = element_blank()
#   ) +
#   labs(
#     x = "Valor",
#     y = "Frecuencia",
#     title = "Distribución de variables por grupo",
#     subtitle = "Histogramas facetados usando todos los datos"
#   )
# 
# 
# dat %>%
#   group_by(asoc) %>%
#   filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
#   ungroup() %>%
#   pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ)) %>%
#   ggplot(aes(x = grupo, y = value, fill = grupo)) +
#   geom_violin(trim = FALSE, alpha = 0.6) +
#   geom_boxplot(width = 0.15, outlier.alpha = 0.3, fill = "white") +
#   facet_wrap(~ name, scales = "free_y") +
#   theme_minimal(base_size = 13) +
#   theme(
#     strip.text = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   ) +
#   labs(
#     x = "Grupo",
#     y = "Valor",
#     title = "Distribución de variables por grupo",
#     subtitle = "Violin plots con boxplots internos"
#   )
# 
# 
# area_total_asoc <- dat %>%
#   group_by(asoc) %>%
#   filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
#   ungroup() %>% filter(periodo == "2017_JUL") %>%
#   group_by(grupo, asoc) %>%
#   summarise(n = n(), area_sum = sum(area_uer_ha), .groups = "drop") %>%
#   mutate(asoc = factor(asoc, levels = unique(asoc))) %>%
#   ggplot(aes(x = asoc, y = area_sum, fill = grupo)) +
#   geom_col(position = position_dodge(width = 0.8), width = 0.6) +
#   coord_flip() +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_blank(),      # quita textos eje Y
#     axis.ticks.y = element_blank(),     # quita ticks eje Y
#     # legend.position = "none"            # quita leyenda
#   )  +
#   labs(x = NULL, y = "Área total (ha)", fill = "Grupo")
# 
# area_promedio_asoc <- dat %>%
#   group_by(asoc) %>%
#   filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
#   ungroup() %>% filter(periodo == "2017_JUL") %>%
#   group_by(grupo, asoc) %>%
#   summarise(n = n(), area_mean = mean(area_uer_ha), .groups = "drop") %>%
#   mutate(asoc = factor(asoc, levels = unique(asoc))) %>%
#   ggplot(aes(x = asoc, y = area_mean, fill = grupo)) +
#   geom_col(position = position_dodge(width = 0.8), width = 0.6) +
#   coord_flip() +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_blank(),      # quita textos eje Y
#     axis.ticks.y = element_blank(),     # quita ticks eje Y
#     legend.position = "none"            # quita leyenda
#   )  +
#   labs(x = NULL , y = "Área promedio - predio (ha)", fill = "Grupo") 
# 
# numero_predios_asoc <- dat %>%
#   group_by(asoc) %>%
#   filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
#   ungroup() %>% filter(periodo == "2017_JUL") %>% 
#   group_by(grupo, asoc) %>%
#   summarise(n = n(), area_mean = mean(area_uer_ha), .groups = "drop") %>%
#   mutate(asoc = factor(asoc, levels = unique(asoc))) %>%
#   ggplot(aes(x = asoc, y = n, fill = grupo)) +
#   geom_col(position = position_dodge(width = 0.8), width = 0.6) +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 8),
#         legend.position = "none"            # quita leyenda
#   ) +
#   labs(x = "Asociación", y = "Numero de predios", fill = "Grupo")
# 
# 
# library(patchwork)
# numero_predios_asoc | area_promedio_asoc | area_total_asoc


### Filtros finales ----

dat <- dat_imputed |>
  dplyr::filter(!is.na(pB), !is.na(pP), !is.na(pV), !is.na(pO), , !is.na(pQ)) |>
  dplyr::filter(frac_mapeada >= 0.9, pB > pB_min, pP > pP_min, area_uer_ha > area_min) %>%
  group_by(asoc) %>%
  filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
  ungroup()


# # --- 1) Asociaciones con datos tanto en 2017 como en 2024 ---
asocs_2017 <- dat %>% filter(periodo == "2017_JUL") %>% distinct(asoc)
asocs_2024 <- dat %>% filter(str_detect(periodo, "2024")) %>% distinct(asoc)

asocs_keep <- intersect(asocs_2017$asoc, asocs_2024$asoc)



dat <- dat %>%  
  filter(asoc %in% asocs_keep) %>%
  filter(str_detect(periodo, "2024|2025", negate = T))

dat <- dat %>% mutate(pO = pO + pQ)



table(dat %>% filter(periodo == "2017_JUL") %>% pull(grupo), dat %>% filter(periodo == "2017_JUL") %>% pull(asoc))

dat$asoc %>% unique()

dat %>% filter(periodo == "2017_JUL") %>% 
  group_by(grupo) %>%
  summarise(n()) 


density_plot_var_moscal <- dat %>%
  pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ)) %>%
  ggplot(aes(x = value, fill = grupo)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ name, scales = "free") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  ) +
  labs(
    x = "Valor",
    y = "Densidad",
    title = "Distribución de variables por grupo",
    subtitle = "Curvas de densidad usando todos los datos"
  )


histogram_plot_var_moscal <- dat %>%
  pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(
    bins = 30, 
    fill = "steelblue", 
    color = "white", 
    alpha = 0.7
  ) +
  facet_wrap(name ~ grupo, scales = "free") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Valor",
    y = "Frecuencia",
    title = "Distribución de variables por grupo",
    subtitle = "Histogramas usando todos los datos"
  )


violin_plot_var_moscal <- dat %>%
  pivot_longer(cols = c(area_uer_ha, pB, pP, pV, pQ)) %>%
  ggplot(aes(x = grupo, y = value, fill = grupo)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.3, fill = "white") +
  facet_wrap(~ name, scales = "free_y") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    x = "Grupo",
    y = "Valor",
    title = "Distribución de variables por grupo",
    subtitle = "Violin plots con boxplots internos"
  )


area_total_asoc <- dat %>%
  group_by(asoc) %>%
  filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
  ungroup() %>% filter(periodo == "2017_JUL") %>%
  group_by(grupo, asoc) %>%
  summarise(n = n(), area_sum = sum(area_uer_ha), .groups = "drop") %>%
  mutate(asoc = factor(asoc, levels = unique(asoc))) %>%
  ggplot(aes(x = asoc, y = area_sum, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),      # quita textos eje Y
    axis.ticks.y = element_blank(),     # quita ticks eje Y
    # legend.position = "none"            # quita leyenda
  )  +
  labs(x = NULL, y = "Área total (ha)", fill = "Grupo")

area_promedio_asoc <- dat %>%
  group_by(asoc) %>%
  filter(n_distinct(grupo) == 2) %>%   # solo asociaciones con los dos grupos
  ungroup() %>% filter(periodo == "2017_JUL") %>%
  group_by(grupo, asoc) %>%
  summarise(n = n(), area_mean = mean(area_uer_ha), .groups = "drop") %>%
  mutate(asoc = factor(asoc, levels = unique(asoc))) %>%
  ggplot(aes(x = asoc, y = area_mean, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),      # quita textos eje Y
    axis.ticks.y = element_blank(),     # quita ticks eje Y
    legend.position = "none"            # quita leyenda
  )  +
  labs(x = NULL , y = "Área promedio - predio (ha)", fill = "Grupo") 

numero_predios_asoc <- dat %>%
  filter(periodo == "2017_JUL") %>% 
  group_by(grupo, asoc) %>%
  summarise(n = n(), area_mean = mean(area_uer_ha), .groups = "drop") %>%
  mutate(asoc = factor(asoc, levels = unique(asoc))) %>%
  ggplot(aes(x = asoc, y = n, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "none"            # quita leyenda
  ) +
  labs(x = "Asociación", y = "Numero de predios", fill = "Grupo")



# numero_predios_asoc | area_promedio_asoc | area_total_asoc

moscal_variables_predios <- dat

save(dat, panel_raw, dat_imputed, file = "moscal_outputs/moscal_var_predios_filtered.RData")
