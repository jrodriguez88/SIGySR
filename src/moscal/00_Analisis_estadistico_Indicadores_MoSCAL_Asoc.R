# ------------------------------------------------------------
# 0) Paquetes
# ------------------------------------------------------------
pkgs <- c(
  "tidyverse","readxl","janitor","stringr","lubridate","scales",
  "glue","forcats","rstatix","broom","ggrepel","patchwork",
  "GGally","corrplot","cluster","factoextra"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ------------------------------------------------------------
# 1) Lectura y chequeos iniciales (tabla larga)
# ------------------------------------------------------------
ruta <- "1indicadores MOSCAL Asociaciones.xlsx"
hoja <- readxl::excel_sheets(ruta)[1]  # usa Hoja 1 por defecto

raw <- readxl::read_excel(ruta, sheet = hoja) |>
  janitor::clean_names()


# var_raw  <- read_csv("data/Moscal/Consolidado estadístico de variables MOSCAL_Unidad geográfica monitoreada_Pivot table.csv") %>%
#   clean_names()
# 
# ind_raw <- read_csv("data/Moscal/Consolidado estadístico de indicadores MOSCAL_Unidad geográfica monitoreada_Pivot table.csv") %>%
#   clean_names()

## Variables 
# 
# Superficie de bosque
# Superficie de Pasto
# Superficie de vegetación secundaria
# Área de cicatrices de quema


# raw %>% group_by(unidad_geografica, periodo) %>% summarise(n = n()) %>% 
#   arrange(desc(n)) %>% view()



# Diccionario de meses en español a inglés
meses_es_en <- c(
  "ENE" = "Jan", "FEB" = "Feb", "MAR" = "Mar", "ABR" = "Apr",
  "MAY" = "May", "JUN" = "Jun", "JUL" = "Jul", "AGO" = "Aug",
  "SEP" = "Sep", "OCT" = "Oct", "NOV" = "Nov", "DIC" = "Dec"
)

# Función para convertir periodo a fecha
parse_periodo <- function(x) {
  # Separar año y mes
  parts <- str_split(x, "_", simplify = TRUE)
  año <- parts[, 1]
  mes <- meses_es_en[parts[, 2]]  # traducir mes
  fecha_str <- paste0("01-", mes, "-", año)  # formato "01-Mes-Año"
  dmy(fecha_str)  # usar dmy porque ahora el formato es día-mes-año
}


# parse_periodo <- function(x){
#   str_replace(x, "_", "-") %>% paste0("-01") %>% ymd()
# }

ind <- raw %>% mutate(periodo = parse_periodo(periodo))
#var <- var_raw %>% mutate(periodo = parse_periodo(periodo))

# Esperado tras clean_names():
# unidad_geografica | linea_base_ano | nombre_indicador | und | orden | periodo | valor_indicador

stopifnot(all(c("unidad_geografica","nombre_indicador","und","periodo","valor_indicador") %in% names(raw)))

# Tipos sugeridos
dat_long <- ind |>
  mutate(
    unidad_geografica = as.factor(unidad_geografica),
    nombre_indicador  = as.factor(nombre_indicador),
    und               = as.character(und),
    periodo           = as.character(periodo),
    valor_indicador   = suppressWarnings(as.numeric(valor_indicador)),
    indicador         = str_trim(sub(".*[-–]\\s*", "", nombre_indicador))
    
  )

# ------------------------------------------------------------
# 2) Limpieza y validación antes del pivot_wider
# ------------------------------------------------------------
# Normaliza texto y unidades
norm_txt <- function(x){
  x |>
    str_squish() |>
    str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9]+","_") |>
    str_replace_all("^_|_$","")
}

dat_long_clean <- dat_long |>
  mutate(
    # Nombre final de la columna: indicador__unidad
    ind_col   = glue("{indicador}_{und}")
  )

# Detecta duplicados que impedirían un pivot_wider (misma llave con varias filas)
keys <- c("unidad_geografica","periodo","ind_col")
dups <- dat_long_clean |>
  count(across(all_of(keys))) |>
  filter(n > 1)

if (nrow(dups) > 0) {
  message("⚠️ Existen duplicados por (unidad_geografica, periodo, indicador_unidad). Se agregará por promedio.")
  dat_long_clean <- dat_long_clean |>
    group_by(across(all_of(c("unidad_geografica","periodo","ind_col")))) |>
    summarise(valor_indicador = mean(valor_indicador, na.rm = TRUE), .groups = "drop")
} else {
  dat_long_clean <- dat_long_clean |>
    select(unidad_geografica, periodo, ind_col, valor_indicador)
}

# ------------------------------------------------------------
# 3) Pivot_wider (tabla ancha por periodo y unidad)
# ------------------------------------------------------------
dat_wide <- dat_long_clean |>
  pivot_wider(
    names_from  = ind_col,
    values_from = valor_indicador
  )

# Ordena periodos si son numéricos (ej. "2017","2024")
# if (suppressWarnings(all(!is.na(as.numeric(unique(dat_wide$periodo)))))) {
#   dat_wide <- dat_wide |>
#     mutate(periodo = as.integer(periodo)) |>
#     arrange(unidad_geografica, periodo)
# } else {
#   # Si son etiquetas tipo "Línea base", "Seguimiento", mantenemos orden por aparición
#   dat_wide <- dat_wide |>
#     mutate(periodo = fct_inorder(periodo)) |>
#     arrange(unidad_geografica, periodo)
# }





# Guarda tabla ancha
dir.create("moscal_outputs", showWarnings = FALSE)
write_csv(dat_wide, "moscal_outputs/moscal_indicadores_asociacion.csv")

# Filtra asociaciones y periodo base 2017 - 2023

NDF <- setdiff(dat_wide %>% pull(unidad_geografica) %>% unique(),
               dat_wide %>% 
                 dplyr::filter(ymd(periodo) < lubridate::make_date(2024, 1,1)) %>% 
                 pull(unidad_geografica) %>% unique()) %>% as.character()

ASOC <- ind %>% 
  dplyr::filter(linea_base_ano == 2017) %>% 
  pull(unidad_geografica) %>% unique() %>% as.character()


# # --- 1) Asociaciones con datos tanto en 2017 como en 2024 ---
# asocs_2017 <- var %>% filter(year == 2017) %>% distinct(unidad_geografica)
# asocs_2024 <- var %>% filter(year == 2024) %>% distinct(unidad_geografica)
# 
# asocs_keep <- intersect(asocs_2017$unidad_geografica, asocs_2024$unidad_geografica)
# 



# --- 1. Diagnóstico de NA ----------------
ind_wide <- dat_wide %>% dplyr::filter(unidad_geografica %in% ASOC)  %>% 
  dplyr::mutate(periodo = ymd(periodo))

library(naniar)
vis_miss(ind_wide %>% arrange((periodo)) %>% filter(periodo < ymd("20240101")))       # mapa de calor de vacíos
naniar::gg_miss_var(ind_wide %>% filter(periodo < ymd("20240101")))

skimr::skim(ind_wide%>% filter(periodo < ymd("20240101")))

slice(ind_wide, 250) 



# ------------------------------------------------------------
# 4) Descriptivos (por indicador y por periodo)
# ------------------------------------------------------------
# Larga otra vez pero con columnas ya depuradas
long_from_wide <- ind_wide |> filter(periodo < ymd("20240101")) |>
  pivot_longer(
    cols = -c(unidad_geografica, periodo),
    names_to = "indicador_unidad",
    values_to = "valor"
  ) %>% dplyr::mutate(periodo = ymd(periodo))

# Descriptivos globales por indicador_unidad
desc_ind <- long_from_wide |>
  group_by(indicador_unidad) |>
  summarise(
    n        = sum(!is.na(valor)),
    missing  = sum(is.na(valor)),
    mean     = mean(valor, na.rm = TRUE),
    median   = median(valor, na.rm = TRUE),
    sd       = sd(valor, na.rm = TRUE),
    p05      = quantile(valor, 0.05, na.rm = TRUE),
    p25      = quantile(valor, 0.25, na.rm = TRUE),
    p75      = quantile(valor, 0.75, na.rm = TRUE),
    p95      = quantile(valor, 0.95, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  arrange(indicador_unidad)

write_csv(desc_ind, "moscal_outputs/descriptivos_por_indicador.csv")

# Descriptivos por periodo e indicador
desc_ind_per <- long_from_wide |>
  group_by(periodo, indicador_unidad) |>
  summarise(
    n       = sum(!is.na(valor)),
    mean    = mean(valor, na.rm = TRUE),
    median  = median(valor, na.rm = TRUE),
    sd      = sd(valor, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(desc_ind_per, "moscal_outputs/descriptivos_por_periodo_indicador.csv")

# Boxplots por indicador_unidad vs periodo
g_box <- long_from_wide |>
  ggplot(aes(x = factor(year(periodo)), y = valor)) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_wrap(~ indicador_unidad, scales = "free") +
  labs(title = "Distribución por periodo (por indicador_unidad)",
       x = "Periodo", y = "Valor") +
  theme_minimal(base_size = 11)


d_by_time <- long_from_wide |> #dplyr::filter(str_detect(indicador_unidad ,"Cb")) %>%
  ggplot(aes(x = periodo, y = valor, colour = unidad_geografica )) +
  geom_point(alpha = 0.4) +
  geom_line(se = F) +
  facet_wrap(~ indicador_unidad, scales = "free") +
  labs(title = "Dinamicas por periodo (por indicador_unidad)",
       x = "Periodo", y = "Valor") +
  theme_minimal(base_size = 11)

d_by_time

plotly::ggplotly(d_by_time)



ggsave("moscal_outputs/boxplots_por_indicador_periodo.png", g_box, width = 14, height = 9, dpi = 300)

# ------------------------------------------------------------
# 5) Cambios (Δ último – primero) por unidad geográfica e indicador
# ------------------------------------------------------------
# Requiere al menos dos periodos por unidad
calc_change <- long_from_wide |>
  group_by(unidad_geografica, indicador_unidad, year = year(periodo)) |>
  summarise(valor = median(valor, na.rm = T), .groups = "drop") |>
  group_by(unidad_geografica, indicador_unidad) |>
  arrange(year, .by_group = TRUE) |>
  summarise(
    first = first(na.omit(valor)),
    last  = nth(na.omit(valor), -1),
    change_abs = last - first,
    change_pct = ifelse(is.finite(first) & abs(first) > 0, 100*(last - first)/abs(first), 0),
    .groups = "drop"
  )

write_csv(calc_change, "moscal_outputs/cambios_first_last_por_indicador_unidad.csv")

# Heatmap de cambios (top N indicadores con más cobertura)
top_inds <- calc_change |>
  group_by(indicador_unidad) |>
  summarise(n = sum(!is.na(change_pct)), .groups = "drop") |>
  arrange(desc(n)) |>
  slice_head(n = 30) |>
  pull(indicador_unidad)

g_heat <- calc_change |>
  filter(indicador_unidad %in% top_inds) |>
  mutate(unidad_geografica = fct_reorder(unidad_geografica, change_pct, .fun = median, .na_rm = TRUE, .desc = TRUE)) |>
  ggplot(aes(x = indicador_unidad, y = unidad_geografica, fill = change_pct)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-100, 100), oob = scales::squish,
                       low="#d62728", mid="white", high="#2ca02c",
                       midpoint = 0, name = "Δ% 2018→2024") +
  
  
  # scale_fill_gradient2(low = "#d62728", mid = "white", high = "#2ca02c", midpoint = 0, na.value = "grey90") +
  labs(title = "Cambio porcentual (2018 – 2023) por unidad e indicador",
       x = NULL, y = NULL, fill = "Δ (%)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("moscal_outputs/heatmap_cambios_pct.png", g_heat, width = 14, height = 9, dpi = 300)

# ------------------------------------------------------------
# 6) Matriz para análisis multivariado (por unidad geográfica)
#    Elegimos el último periodo disponible por unidad
# ------------------------------------------------------------
wide_last <- long_from_wide |>
  # group_by(unidad_geografica, indicador_unidad) |>
  # arrange(as.character(periodo), .by_group = TRUE) |>
  # summarise(valor = dplyr::last(na.omit(valor)), .groups = "drop") |>
  pivot_wider(names_from = indicador_unidad, values_from = valor)

visdat::vis_miss(wide_last)

# Quitamos columnas con demasiados NA
na_thresh <- 0.3  # permite hasta 30% NA
keep_cols <- colMeans(is.na(wide_last)) <= na_thresh
wide_last_f <- wide_last[, keep_cols]

visdat::vis_miss(wide_last_f)

# Imputación simple de NAs con mediana (rápido y robusto para PCA/kmeans)
impute_median <- function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)
# mat_num <- wide_last_f |> dplyr::select(- periodo) %>%
#   group_by(unidad_geografica) |>
#   mutate(across(where(is.numeric), impute_median)) |>
#   ungroup() |>
#   select(-unidad_geografica) |>
#   as.matrix()


# c) Imputación múltiple ---------------
library(mice)
# 1.1  Crea un data.frame solo con las variables a imputar
vars_to_impute <- wide_last_f %>% 
  dplyr::select(-unidad_geografica, - periodo)

# 1.2  Revisa la proporción de NA (ya sabemos que es ≤30 %)
naniar::miss_var_summary(vars_to_impute)

# 2.1  Objeto inicial (m = 0 solo como 'dry‑run')
skimr::skim(vars_to_impute)

## mice alg
ini <- mice(vars_to_impute)  # ahora sí funciona

# vis_miss(complete(ini))
mat_num <- complete(ini)

wide_last_f <- wide_last_f %>% dplyr::select(unidad_geografica, periodo) %>%
  bind_cols(mat_num)


# ------------------------------------------------------------
# 7) Correlaciones
# ------------------------------------------------------------
corr_mat <- suppressWarnings(cor(scale(mat_num ), use = "pairwise.complete.obs", method = "spearman"))
png("moscal_outputs/correlaciones_spearman.png", width = 1400, height = 1000, res = 140)
corrplot::corrplot(corr_mat, method = "color", tl.cex = 0.6, number.cex = 0.5, mar = c(0,0,1,0))
mtext("Correlación Spearman entre indicadores (último periodo)", line = 1)
dev.off()

# ------------------------------------------------------------
# 8) PCA (último periodo, variables escaladas)
# ------------------------------------------------------------
pca_res <- prcomp(scale(mat_num), center = TRUE, scale. = F)

fviz_pca_biplot(pca_res, repel = TRUE, label = "var") +
  ggtitle("PCA (último periodo) — biplot")
ggsave("moscal_outputs/pca_biplot.png", width = 10, height = 8, dpi = 300)

# Cargas principales
pca_loadings <- as_tibble(pca_res$rotation, rownames = "indicador_unidad")
write_csv(pca_loadings, "moscal_outputs/pca_cargas.csv")

# ------------------------------------------------------------
# 9) Clustering (K-means) — sugerencia de K por elbow + silhouette
# ------------------------------------------------------------
set.seed(42)
wss <- map_dbl(2:8, ~{
  km <- kmeans(scale(mat_num), centers = .x, nstart = 50, iter.max = 200)
  km$tot.withinss
})

k_elbow <- tibble(k = 2:8, wss = wss)
ggplot(k_elbow, aes(k, wss)) + geom_line() + geom_point() +
  labs(title = "Elbow plot (K-means)", x = "k", y = "Total within-cluster SS") +
  theme_minimal(base_size = 11)
ggsave("moscal_outputs/kmeans_elbow.png", width = 8, height = 5, dpi = 300)

# Silhouette para k = 2..6
sil <- map_dfr(2:8, ~{
  km <- kmeans(scale(mat_num), centers = .x, nstart = 50, iter.max = 200)
  ss <- silhouette(km$cluster, dist(scale(mat_num)))
  tibble(k = .x, sil_mean = mean(ss[,3]))
})
write_csv(sil, "moscal_outputs/kmeans_silhouette.csv")

# k_best <- sil$k[which.max(sil$sil_mean)] +
# if (length(k_best) == 0 || is.na(k_best)) 
  
k_best <- 4

km_final <- kmeans(scale(mat_num), centers = k_best, nstart = 200, iter.max = 100)
clusters <- tibble(unidad_geografica = rownames(mat_num), cluster = factor(km_final$cluster))
write_csv(clusters, "moscal_outputs/kmeans_clusters.csv")
plot(km_final$cluster)

table(km_final$cluster)


# Visual de clusters en espacio PCA
fviz_pca_biplot(pca_res, habillage = as.factor(clusters$cluster), axes = c(1, 2),
                label = "var", repel = TRUE, addEllipses = F) +
  labs(title = "PCA biplot con clusters Kmeans (k = 4)") 

# fviz_pca_biplot(
#   pca_res,
#   habillage = as.factor(clusters$cluster),
#   axes = c(1, 2),
#   label = "var",
#   repel = TRUE,
#   addEllipses = FALSE
# ) +
#   labs(title = "PCA biplot con clusters Kmeans (k = 4)") +
#   scale_color_viridis_d(option = "E") +
#   scale_fill_viridis_d(option = "E")



wide_last_f %>%
  mutate(cluster = clusters$cluster) %>%
  select(unidad_geografica, cluster, periodo) %>%
  distinct() %>%
  count(unidad_geografica, cluster) %>%
  group_by(unidad_geografica) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = unidad_geografica, y = prop, fill = cluster)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  # scale_fill_viridis_d(option = "E", begin = 0.1, end = 0.9) +
  labs(y = "Proporción", x = "Unidad Geográfica", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Tabla de perfiles

profile <- wide_last_f %>%
  mutate(cluster = clusters$cluster) %>%
  group_by(cluster) %>%
  summarise(n = n(),
            across(where(is.numeric), \(x) mean(x, na.rm = TRUE), .names = "mean_{.col}"))

data.table::data.table(profile)


wide_last_f %>%
  mutate(cluster = clusters$cluster) %>% filter(cluster == 4) %>% arrange(periodo)

pivot_longer(profile, -cluster) %>%
  pivot_wider(names_from = cluster,
              values_from = value,
              names_prefix = "cluster_")


factoextra::fviz_pca_var(pca_res, col.var = "contrib",
                         gradient.cols = c("white", "blue", "red"),
                         ggtheme = theme_minimal())

factoextra::fviz_contrib(pca_res, choice = "var", , axes = 1:7, top = 10)


# ------------------------------------------------------------http://127.0.0.1:17164/graphics/15638dbd-dea9-45da-8c17-b33cd13fefd8.png
# 10) Reporte rápido de hallazgos (texto)
# ------------------------------------------------------------
sink("moscal_outputs/resumen_hallazgos.txt")
cat("Resumen de hallazgos (auto):\n\n")
cat("- Se normalizaron nombres de indicadores y unidades, y se creó una matriz wide por (unidad_geografica, periodo).\n")
cat("- Se agregaron duplicados exactos por promedio cuando existieron.\n")
cat("- Se calcularon descriptivos globales y por periodo; ver CSVs en moscal_outputs/.\n")
cat("- Cambios Δ% (último – primero) por unidad e indicador; heatmap en heatmap_cambios_pct.png.\n")
cat("- Correlaciones (Spearman) en correlaciones_spearman.png.\n")
cat("- PCA (biplot) en pca_biplot.png; cargas en pca_cargas.csv.\n")
cat(glue("- K-means: mejor k por silhouette ≈ {k_best}; clusters en kmeans_clusters.csv y gráfico kmeans_pca.png.\n"))
sink()




# ------------------------------------------------------------
# X) Tendencias por indicador y asociación (2018–2023)
#     – pendiente lineal + Theil–Sen + Mann–Kendall
# ------------------------------------------------------------

# Paquete nuevo
if (!requireNamespace("Kendall", quietly = TRUE)) {
  install.packages("Kendall")
}
library(Kendall)

# --- 1) Pasar wide_last_f a formato largo con año numérico ----
trend_long <- wide_last_f %>%
  dplyr::mutate(
    year = lubridate::year(periodo)
  ) %>%
  tidyr::pivot_longer(
    cols = -c(unidad_geografica, periodo, year),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  dplyr::arrange(unidad_geografica, indicador, year) %>%
  dplyr::filter(!is.na(valor))

# --- 2) Función Theil–Sen simple (pendiente mediana) ----------
theil_sen_slope <- function(x, t) {
  ok <- is.finite(x) & is.finite(t)
  x <- x[ok]; t <- t[ok]
  n <- length(x)
  if (n < 3L || length(unique(t)) < 2L) return(NA_real_)
  
  # todas las pendientes (y_j - y_i) / (t_j - t_i)
  slopes <- c()
  k <- 1L
  for (i in 1:(n - 1L)) {
    for (j in (i + 1L):n) {
      if (t[j] != t[i]) {
        slopes[k] <- (x[j] - x[i]) / (t[j] - t[i])
        k <- k + 1L
      }
    }
  }
  stats::median(slopes)
}

# --- 3) Calcular tendencias por (unidad_geografica, indicador) -
tendencias <- trend_long %>%
  dplyr::group_by(unidad_geografica, indicador) %>%
  dplyr::group_modify(~{
    df <- .x
    n  <- nrow(df)
    años_unicos <- length(unique(df$year))
    
    # si hay muy pocos puntos o serie constante, no se estima tendencia
    if (n < 4L || años_unicos < 3L || stats::sd(df$valor, na.rm = TRUE) == 0) {
      return(tibble::tibble(
        n         = n,
        year_ini  = min(df$year),
        year_fin  = max(df$year),
        slope_lm  = NA_real_,
        p_lm      = NA_real_,
        slope_ts  = NA_real_,
        tau_mk    = NA_real_,
        p_mk      = NA_real_
      ))
    }
    
    # --- Regresión lineal clásica ---
    mod <- stats::lm(valor ~ year, data = df)
    slope_lm <- unname(stats::coef(mod)["year"])
    p_lm     <- summary(mod)$coefficients["year", "Pr(>|t|)"]
    
    # --- Pendiente Theil–Sen (no paramétrica) ---
    slope_ts <- theil_sen_slope(df$valor, df$year)
    
    # --- Mann–Kendall (Kendall::MannKendall) ---
    mk <- Kendall::MannKendall(df$valor)
    tau_mk <- unname(mk$tau)
    p_mk   <- mk$sl
    
    tibble::tibble(
      n        = n,
      year_ini = min(df$year),
      year_fin = max(df$year),
      slope_lm = slope_lm,
      p_lm     = p_lm,
      slope_ts = slope_ts,
      tau_mk   = tau_mk,
      p_mk     = p_mk
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # clasificación sencilla de tendencia según MK
    trend_mk = dplyr::case_when(
      is.na(p_mk) ~ "sin_test",
      p_mk < 0.05 & tau_mk > 0 ~ "creciente",
      p_mk < 0.05 & tau_mk < 0 ~ "decreciente",
      TRUE ~ "no_signif"
    ),
    # y según pendiente lineal (por si quieres comparar)
    trend_lm = dplyr::case_when(
      is.na(p_lm) ~ "sin_test",
      p_lm < 0.05 & slope_lm > 0 ~ "creciente",
      p_lm < 0.05 & slope_lm < 0 ~ "decreciente",
      TRUE ~ "no_signif"
    )
  )

# Guardar tabla para usarla en el informe
readr::write_csv(tendencias, "moscal_outputs/tendencias_por_indicador_asociacion.csv")

# ------------------------------------------------------------
# X.1) Heatmap de pendientes (Theil–Sen) por indicador/asociación
# ------------------------------------------------------------
g_trend_heat <- tendencias %>%
  dplyr::mutate(
    unidad_geografica = forcats::fct_reorder(
      unidad_geografica, slope_ts,
      .fun = mean, na.rm = TRUE, .desc = TRUE
    )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = indicador, y = unidad_geografica, fill = slope_ts)
  ) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient2(
    low      = "#d62728",  # pendiente negativa
    mid      = "white",
    high     = "#2ca02c",  # pendiente positiva
    midpoint = 0,
    na.value = "grey90",
    name     = "Pendiente\nTheil–Sen\n(unidad/año)"
  ) +
  ggplot2::labs(
    title = "Pendiente temporal por indicador y asociación (Theil–Sen)",
    x = NULL, y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
  )

ggplot2::ggsave(
  "moscal_outputs/heatmap_tendencias_theilsen.png",
  g_trend_heat, width = 14, height = 9, dpi = 300
)

# ------------------------------------------------------------
# X.2) Ejemplo de gráfico de series + tendencia por indicador
#      (puedes repetir cambiando 'ind_to_plot')
# ------------------------------------------------------------

ind_to_plot <- "Cb_%"   # cámbialo por cualquier indicador

g_trend_ts <- trend_long %>%
  dplyr::filter(indicador == ind_to_plot) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = valor, group = unidad_geografica, colour = unidad_geografica)
  ) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_line(alpha = 0.5) +
  ggplot2::geom_smooth(
    method = "lm", se = FALSE, colour = "black", linetype = "dashed"
  ) +
  ggplot2::facet_wrap(~unidad_geografica, scales = "free_y") +
  ggplot2::labs(
    title = paste("Tendencias anuales del indicador", ind_to_plot),
    x = "Año", y = "Valor"
  ) +
  ggplot2::theme_minimal(base_size = 9) +
  ggplot2::theme(legend.position = "none")

ggplot2::ggsave(
  paste0("moscal_outputs/series_tendencia_", gsub("%", "pct", ind_to_plot), ".png"),
  g_trend_ts, width = 14, height = 9, dpi = 300
)




# ------------------------------------------------------------
# Tendencias por asociación (Theil–Sen + Mann–Kendall)
#   → resumen % de asociaciones ↑ sig., ↓ sig., ns
# ------------------------------------------------------------

if (!requireNamespace("Kendall", quietly = TRUE)) {
  install.packages("Kendall")
}
library(Kendall)

# 1) Pasar wide_last_f a largo (indicador, año)
trend_long <- wide_last_f %>%
  dplyr::mutate(year = lubridate::year(periodo)) %>%
  tidyr::pivot_longer(
    cols = -c(unidad_geografica, periodo, year),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  dplyr::filter(!is.na(valor)) %>%
  dplyr::arrange(unidad_geografica, indicador, year)

# 2) Theil–Sen casero (pendiente mediana)
theil_sen_slope <- function(x, t) {
  ok <- is.finite(x) & is.finite(t)
  x <- x[ok]; t <- t[ok]
  n <- length(x)
  if (n < 3L || length(unique(t)) < 2L) return(NA_real_)
  slopes <- c()
  k <- 1L
  for (i in 1:(n-1L)) {
    for (j in (i+1L):n) {
      if (t[j] != t[i]) {
        slopes[k] <- (x[j] - x[i]) / (t[j] - t[i])
        k <- k + 1L
      }
    }
  }
  stats::median(slopes)
}

# 3) MK + Theil–Sen por indicador y asociación  ----------------
calc_mk_moscal <- function(df, varname) {
  dd <- df %>%
    dplyr::filter(indicador == varname) %>%
    dplyr::group_by(unidad_geografica, year) %>%
    dplyr::summarise(v = mean(valor, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(unidad_geografica, year)
  
  if (nrow(dd) == 0) return(NULL)
  
  dd %>%
    dplyr::group_by(unidad_geografica) %>%
    dplyr::group_modify(~{
      v <- .x$v
      t <- .x$year
      ok <- is.finite(v) & is.finite(t)
      v <- v[ok]; t <- t[ok]
      n <- length(v)
      
      # defaults
      sen  <- NA_real_
      pval <- NA_real_
      
      if (n >= 3L && stats::sd(v) > 0) {
        sen  <- theil_sen_slope(v, t)
        mk   <- Kendall::MannKendall(v)
        pval <- as.numeric(mk$sl)
      }
      
      tibble::tibble(sen = sen, mk_p = pval)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      variable = varname,
      dir = dplyr::case_when(
        !is.na(mk_p) & mk_p < 0.05 & sen > 0 ~ "↑ sig.",
        !is.na(mk_p) & mk_p < 0.05 & sen < 0 ~ "↓ sig.",
        TRUE ~ "ns"
      )
    )
}

vars_ind <- sort(unique(trend_long$indicador))

mk_tbl <- purrr::map_dfr(vars_ind, ~calc_mk_moscal(trend_long, .x))

# 4) Resumen % asociaciones por dirección de tendencia ---------
mk_sum <- mk_tbl %>%
  dplyr::count(variable, dir) %>%
  dplyr::group_by(variable) %>%
  dplyr::mutate(pct = 100 * n / sum(n)) %>%
  dplyr::ungroup()

# (opcional) renombrar variables para el gráfico
lab_ind <- c(
  "PFC_%"    = "Cantidad de puntos de calor",
  "Vp_ha"    = "Superficie de Pasto",
  "VCq_ha"   = "Área de cicatrices de quema",
  "VLv_ha"   = "Longitud Vial",
  "Vcco_ha"  = "Área de cultivos de coca",
  "Cb_%"     = "Superficie de Bosque",
  "VC_%"     = "Índice de Conectividad",
  "Vvs_ha"   = "Superficie de Vegetación secundaria",
  "VIFrag_ha"= "Grado de Fragmentación",
  "Cbp_%"    = "Bosque → Pasto",
  "Cpvs_%"   = "Pasto → Veg. secundaria",
  "Cvsb_%"   = "Veg. secundaria → Bosque",
  "Vmi_ha"   = "Área minera",
  "VTHC_ha"  = "Área hidrocarburos",
  "PbRF_%"   = "% pérdida de bosque en ZRF"
)

mk_sum <- mk_sum %>%
  dplyr::mutate(
    variable = dplyr::recode(variable, !!!lab_ind)
  )

message("\n[Mann–Kendall] % asociaciones por dirección de tendencia:")
print(mk_sum %>% dplyr::arrange(variable, desc(dir)), n = nrow(mk_sum))

readr::write_csv(mk_sum, "moscal_outputs/moscal_mk_summary_tendencias.csv")

# 5) Gráfico tipo barras apiladas (% asociaciones) -------------
g_mk <- ggplot2::ggplot(
  mk_sum,
  ggplot2::aes(
    x   = pct,
    y   = forcats::fct_reorder(variable, pct, .fun = max),
    fill = dir
  )
) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Tendencias por asociación (Theil–Sen + Mann–Kendall)",
    x = "% de asociaciones",
    y = NULL,
    fill = NULL
  ) +
  ggplot2::scale_fill_manual(
    values = c("↑ sig." = "#2ca02c", "↓ sig." = "#d62728", "ns" = "#bbbbbb")
  ) +
  ggplot2::theme_minimal(base_size = 12)

g_mk

ggplot2::ggsave(
  "moscal_outputs/mk_barras_tendencias.png",
  g_mk, width = 9, height = 7, dpi = 300
)

