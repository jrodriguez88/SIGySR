# ===============================================
# MoSCAL — análisis final con prints y plots
# ===============================================

# ---- Paquetes ----
req <- c("tidyverse","lubridate","broom","forcats","glue",
         "lme4","lmerTest","broom.mixed",
         "Kendall","trend","scales","patchwork",
         "FactoMineR","factoextra","naniar")
for(p in req){ if(!requireNamespace(p, quietly=TRUE)) install.packages(p) }
library(tidyverse); library(lubridate); library(broom); library(forcats); library(glue)
library(lme4); library(lmerTest); library(broom.mixed)
library(Kendall); library(trend); library(scales); library(patchwork)
library(FactoMineR); library(factoextra); library(naniar)

# # ---- Cargar datos ----
# if(!exists("var")){
#   message("Leyendo var_moscal.csv ...")
#   var <- readr::read_csv("var_moscal.csv", show_col_types = FALSE)
# }
# stopifnot(all(c("unidad_geografica","nombre_variable","unidad","orden","periodo","valor_variable") %in% names(var)))



df <- readr::read_csv("moscal_imputed.csv", show_col_types = FALSE) #|> clean_names()



stopifnot(all(c("unidad_geografica","periodo") %in% names(df)))

# columnas id/tiempo que NO pivotear
id_cols  <- intersect(c("unidad_geografica","periodo","orden"), names(df))
num_cols <- setdiff(names(df)[sapply(df, is.numeric)], id_cols)

var <- df %>%
  pivot_longer(all_of(num_cols),
               names_to  = "nombre_variable",
               values_to = "valor_variable") %>%
  mutate(
    periodo  = as_date(periodo),
    year     = year(periodo),
    mes      = month(periodo),
    semestre = if_else(mes <= 6, 1L, 2L),
    # opcional, si luego graficas unidades:
    unidad = case_when(
      nombre_variable %in% c("Superficie de Bosque","Superficie de Pasto",
                             "Superficie de Vegetación secundaria","Área de no agropecuario",
                             "Área de Paisaje Agropecuario continuo","Área de cicatrices de quema",
                             "Área de cultivos de coca") ~ "ha",
      nombre_variable %in% c("Longitud Vial") ~ "km",
      nombre_variable %in% c("Cantidad de puntos de calor") ~ "conteos",
      nombre_variable %in% c("Índice de Conectividad","Grado de Fragmentación") ~ "índice",
      nombre_variable == "Promedio del tamaño de los predios con firma de acuerdo" ~ "ha",
      TRUE ~ NA_character_
    )
  )

# ---- Definiciones ----
vars_obj <- c(
  "Cantidad de puntos de calor","Grado de Fragmentación","Longitud Vial",
  "Promedio del tamaño de los predios con firma de acuerdo",
  "Superficie de Bosque","Superficie de Pasto","Superficie de Vegetación secundaria",
  "Área de Paisaje Agropecuario continuo","Área de cicatrices de quema",
  "Área de cultivos de coca","Área de no agropecuario","Índice de Conectividad"
)
# vars_area_den      <- c("Superficie de Bosque","Superficie de Pasto","Superficie de Vegetación secundaria","Área de no agropecuario")
# vars_as_pct        <- c(vars_area_den,"Área de Paisaje Agropecuario continuo","Área de cicatrices de quema","Área de cultivos de coca")
# vars_density_1000ha<- c("Cantidad de puntos de calor","Longitud Vial","Índice de Conectividad", "Grado de Fragmentación")
# vars_unitless      <- c("")
# var_predio         <- "Promedio del tamaño de los predios con firma de acuerdo"


# conjuntos 
vars_area_den <- c("Superficie de Bosque","Superficie de Pasto",
                   "Superficie de Vegetación secundaria", "Área de cicatrices de quema" 
                   )

vars_as_pct <- c(vars_area_den,
                 "Área de Paisaje Agropecuario continuo",
                 "Área de no agropecuario",
                 "Área de cultivos de coca")

vars_per_1000ha <- c("Cantidad de puntos de calor","Longitud Vial")  # densidad -> por 1000 ha

vars_unitless <- c("Índice de Conectividad","Grado de Fragmentación")
var_predio    <- "Promedio del tamaño de los predios con firma de acuerdo"

dir.create("moscal_outputs", showWarnings = FALSE)


# --- 1) Asociaciones con datos tanto en 2017 como en 2024 ---
asocs_2017 <- var %>% filter(year == 2017) %>% distinct(unidad_geografica)
asocs_2024 <- var %>% filter(year == 2024) %>% distinct(unidad_geografica)

asocs_keep <- intersect(asocs_2017$unidad_geografica, asocs_2024$unidad_geografica)


# ---- QC rápido: cobertura y NA ----
message("\n[QC] Cobertura temporal (n observaciones por año, todas las variables):")
coverage_tbl <- var %>% dplyr::filter(unidad_geografica %in% asocs_keep) %>%
  count(year) %>%
  arrange(year)
print(coverage_tbl)

# Mapa de NA (en el largo)
# print(naniar::gg_miss_var(var) + ggtitle("Porcentaje de NA por variable (largo)"))

# --- 1) Denominador de área por asociación–año (ha) ---
area_total <- var %>%
  dplyr::filter(unidad_geografica %in% asocs_keep,
                year %in% 2017:2023) %>% 
  filter(nombre_variable %in% vars_area_den) %>%
  group_by(unidad_geografica, year, nombre_variable) %>%
  summarise(v = mean(valor_variable, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = nombre_variable, values_from = v, values_fill = 0) %>%
  mutate(area_total_ha = rowSums(across(all_of(vars_area_den)), na.rm = TRUE)) %>%
  dplyr::select(unidad_geografica, year, area_total_ha)

# ---- Normalización ----
# normalización analítica (para tablas/modelos)
datos_norm <- var %>%
  dplyr::filter(unidad_geografica %in% asocs_keep,
                year %in% 2017:2023) %>% 
  left_join(area_total, by = c("unidad_geografica","year")) %>%
  mutate(
    valor_norm = case_when(
      nombre_variable %in% vars_as_pct      & area_total_ha > 0 ~ 100  * valor_variable / area_total_ha,   # %
      nombre_variable %in% vars_per_1000ha  & area_total_ha > 0 ~ 1000 * valor_variable / area_total_ha,   # por 1000 ha
      nombre_variable %in% c(vars_unitless, var_predio)          ~ valor_variable,                          # crudo (analítico)
      TRUE ~ NA_real_
    )
  )

# ---- Panel anual (media del año) ----
panel_anual <- datos_norm %>% dplyr::filter(nombre_variable != "mes", nombre_variable != "year") %>%
  group_by(unidad_geografica, nombre_variable, year) %>%
  summarise(mean_value = mean(valor_norm, na.rm = TRUE),
            n_obs = sum(!is.na(valor_norm)),
            area_total_ha = mean(area_total_ha, na.rm = TRUE), .groups = "drop")

message(glue("\nPanel anual listo: {n_distinct(panel_anual$unidad_geografica)} asociaciones; {n_distinct(panel_anual$year)} años."))

# ---- PRE/POST 2018→2024 (pareado) ----
calc_prepost <- function(df, varname){
  g <- df %>% 
    dplyr::filter(nombre_variable == varname,
                     year %in% c(2018, 2023)) %>%
    dplyr::select(unidad_geografica, year, mean_value) %>%
    pivot_wider(names_from = year, values_from = mean_value, names_prefix = "y_") %>%
    drop_na(y_2018, y_2024)
  
  if(nrow(g) < 3) return(NULL)
  
  if (varname %in% vars_unitless){
    d <- g$y_2024 - g$y_2018
    tt <- t.test(g$y_2024, g$y_2018, paired = TRUE)
    tibble(variable=varname, n_pairs=nrow(g),
           metric="diff", estimate=mean(d), ci_low=unname(tt$conf.int[1]),
           ci_high=unname(tt$conf.int[2]), p_value=tt$p.value,
           effect_dz=mean(d)/sd(d))
  } else {
    # log-ratio
    g <- g %>% filter(y_2018>0, y_2024>0)
    if(nrow(g) < 3) return(NULL)
    dlog <- log(g$y_2024) - log(g$y_2018)
    tt <- t.test(dlog, mu = 0)
    se <- sd(dlog)/sqrt(length(dlog))
    ci <- exp(mean(dlog) + c(-1,1) * qt(0.975, df = length(dlog)-1) * se)
    tibble(variable=varname, n_pairs=nrow(g),
           metric="ratio", estimate=exp(mean(dlog)),
           ci_low=ci[1], ci_high=ci[2], p_value=tt$p.value,
           effect_dz=mean(dlog)/sd(dlog))
  }
}


calc_prepost <- function(df, varname){
  g <- df %>%
    dplyr::filter(nombre_variable == varname) %>%
    group_by(unidad_geografica) %>%
    filter(!is.na(year)) %>%
    summarise(y0 = first(year[order(year)]),
              y1 = last(year[order(year)]),
              v0 = mean_value[which.min(year)],
              v1 = mean_value[which.max(year)],
              .groups = "drop") %>%
    drop_na(v0, v1)
  
  if(nrow(g) < 3) return(NULL)
  
  if (varname %in% vars_unitless || varname == var_predio){
    d <- g$v1 - g$v0
    tt <- t.test(d, mu = 0)
    tibble(variable=varname, n_pairs=nrow(g),
           metric="diff", estimate=mean(d), 
           ci_low=unname(tt$conf.int[1]), ci_high=unname(tt$conf.int[2]),
           p_value=tt$p.value)
  } else {
    g <- g %>% filter(v0>0, v1>0)
    if(nrow(g) < 3) return(NULL)
    dlog <- log(g$v1) - log(g$v0)
    tt <- t.test(dlog, mu = 0)
    se <- sd(dlog)/sqrt(length(dlog))
    ci <- exp(mean(dlog) + c(-1,1) * qt(0.975, df = length(dlog)-1) * se)
    tibble(variable=varname, n_pairs=nrow(g),
           metric="ratio", estimate=exp(mean(dlog)),
           ci_low=ci[1], ci_high=ci[2], p_value=tt$p.value)
  }
}



prepost_tbl <- map_dfr(setdiff(vars_obj, var_predio), ~calc_prepost(panel_anual, .x)) %>%
  mutate(fdr = p.adjust(p_value, method = "BH")) %>%
  arrange(fdr, p_value)

message("\n[PRE/POST] Tabla de cambios 2018→2024 (normalizado) con FDR:")
print(prepost_tbl, n = nrow(prepost_tbl))
readr::write_csv(prepost_tbl, "moscal_outputs/moscal_prepost_summary_normalized.csv")

# ---- Plots forest (pre/post) ----
forest_ratio <- prepost_tbl %>% filter(metric=="ratio") %>%
  mutate(pct_change = 100*(estimate-1),
         pct_low = 100*(ci_low-1), pct_high = 100*(ci_high-1),
         variable = fct_reorder(variable, pct_change)) %>%
  ggplot(aes(pct_change, variable, xmin=pct_low, xmax=pct_high)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(height=0) + geom_point(size=2) +
  labs(title="Cambio 2018-2024 (razon geometrica - %)", x="% cambio (IC95%)", y=NULL) +
  theme_minimal(base_size = 12)

forest_diff <- prepost_tbl %>% filter(metric=="diff") %>%
  mutate(variable = fct_reorder(variable, estimate)) %>%
  ggplot(aes(estimate, variable, xmin=ci_low, xmax=ci_high)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(height=0) + geom_point(size=2) +
  labs(title="Cambio 2018→2024 (diferencia absoluta)", x="Δ (IC95%)", y=NULL) +
  theme_minimal(base_size = 12)

ggsave("moscal_outputs/forest_prepost_ratio.png", forest_ratio, width=7, height=5, dpi=300)
ggsave("moscal_outputs/forest_prepost_diff.png",  forest_diff,  width=7, height=5, dpi=300)
print(forest_ratio)#; print(forest_diff)

# ---- LMM: pendiente anual (serie completa) ----
# covariable: tamaño de predio (log1p) por asociación–año
predio_by_year <- datos_norm %>%
  filter(nombre_variable == var_predio) %>%
  group_by(unidad_geografica, year) %>%
  summarise(predio_mean = mean(valor_norm, na.rm = TRUE), .groups = "drop")

df_lmm <- datos_norm %>%
  filter(nombre_variable != var_predio) %>%
  left_join(predio_by_year, by = c("unidad_geografica","year")) %>%
  mutate(year_c = year - 2017,
         log_pos = if_else(nombre_variable %in% vars_unitless, NA_real_, log1p(pmax(valor_norm,0))),
         resp    = if_else(nombre_variable %in% vars_unitless, valor_norm, log_pos))

fit_lmm_one <- function(df, varname){
  dd <- df %>% filter(nombre_variable==varname, !is.na(resp))
  if(n_distinct(dd$unidad_geografica) < 3) return(NULL)
  m <- lmer(resp ~ year_c + factor(semestre) + scale(log1p(predio_mean)) +
              (1|unidad_geografica),
            data=dd,
            control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
  fx <- broom.mixed::tidy(m, effects="fixed") %>% filter(term=="year_c")
  est <- fx$estimate; se <- fx$std.error
  if(!(varname %in% vars_unitless)){
    tibble(variable=varname, slope_type="% anual (aprox.)",
           slope=(exp(est)-1)*100,
           ci_low=(exp(est-1.96*se)-1)*100,
           ci_high=(exp(est+1.96*se)-1)*100,
           p_value=fx$p.value)
  } else {
    tibble(variable=varname, slope_type="unidades/año",
           slope=est, ci_low=est-1.96*se, ci_high=est+1.96*se,
           p_value=fx$p.value)
  }
}
lmm_tbl <- map_dfr(setdiff(vars_obj, var_predio), ~fit_lmm_one(df_lmm, .x)) %>%
  mutate(fdr=p.adjust(p_value, method="BH")) %>%
  arrange(fdr, p_value)


# df_test <- df_lmm %>% filter(nombre_variable == "Superficie de Bosque")
# 
# 
# m <- lmer(resp ~ year_c + factor(semestre) + scale(log1p(predio_mean)) +
#             (1|unidad_geografica),
#           data=df_test,
#           control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
# 
# broom.mixed::tidy(m, effects="fixed") 
# 
# m
# 
# plot(m)


# 1) Línea base 2018 por variable (mediana sobre asociaciones)
baseline_2018 <- panel_anual %>%
  filter(year == 2018) %>%
  group_by(nombre_variable) %>%
  summarise(base2018 = median(mean_value, na.rm = TRUE), .groups = "drop")

# 2) Reexpresar pendientes a %/año cuando estaban en unidades/año
lmm_plot <- lmm_tbl %>%
  left_join(baseline_2018, by = c("variable" = "nombre_variable")) %>%
  mutate(
    slope_pct  = dplyr::case_when(
      slope_type == "% anual (aprox.)"                ~ slope,                        # ya está en %/año
      slope_type == "unidades/año" & base2018 > 0     ~ 100 * slope  / base2018,
      TRUE                                            ~ NA_real_
    ),
    ci_low_pct = dplyr::case_when(
      slope_type == "% anual (aprox.)"                ~ ci_low,
      slope_type == "unidades/año" & base2018 > 0     ~ 100 * ci_low / base2018,
      TRUE                                            ~ NA_real_
    ),
    ci_high_pct = dplyr::case_when(
      slope_type == "% anual (aprox.)"                ~ ci_high,
      slope_type == "unidades/año" & base2018 > 0     ~ 100 * ci_high / base2018,
      TRUE                                            ~ NA_real_
    )
  )

# 3) Forest plot en una sola escala (%/año) — ahora Índice de Conectividad no aplasta el eje
g_lmm_pct <- lmm_plot %>%
  filter(!is.na(slope_pct)) %>%
  mutate(variable = forcats::fct_reorder(variable, slope_pct)) %>%
  ggplot(aes(slope_pct, variable, xmin = ci_low_pct, xmax = ci_high_pct)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_errorbarh(height = 0) +
  geom_point(size = 2) +
  labs(title = "Tendencia anual (LMM 2018–2024) — todo en %/año",
       x = "%/año (IC95%)", y = NULL) +
  theme_minimal(base_size = 12)

# Guarda si quieres
# ggsave("moscal_outputs/forest_lmm_slopes_pct.png", g_lmm_pct, width=7, height=6, dpi=300)


message("\n[LMM] Pendientes anuales promedio (ajustado por semestre y tamaño de predio):")
print(lmm_tbl, n = nrow(lmm_tbl))
readr::write_csv(lmm_tbl, "moscal_outputs/moscal_lmm_slopes_normalized.csv")

# g_lmm <- lmm_tbl %>%
#   mutate(variable = fct_reorder(variable, slope)) %>%
#   ggplot(aes(slope, variable, xmin=ci_low, xmax=ci_high)) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   geom_errorbarh(height=0) + geom_point(size=2) +
#   labs(title="Tendencia anual (LMM 2018–2024)",
#        x="Pendiente (IC95%) — %/año o unidades/año", y=NULL) +
#   theme_minimal(base_size = 12)
# ggsave("moscal_outputs/forest_lmm_slopes.png", g_lmm, width=7, height=6, dpi=300)
# print(g_lmm)



# ---- Tendencias Theil–Sen + Mann–Kendall por asociación ----
# --------- PARCHE ROBUSTO PARA MK/THIEL–SEN ----------
# --- Calc MK/Thiel–Sen robusto (sin rowwise, sin list-cols frágiles) ---
# MK + Theil–Sen robusto por asociación
calc_mk <- function(df, varname){
  dd <- df %>%
    dplyr::filter(nombre_variable == varname) %>%
    dplyr::group_by(unidad_geografica, year) %>%
    dplyr::summarise(v = mean(mean_value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(unidad_geografica, year)
  
  if (nrow(dd) == 0) return(NULL)
  
  dd %>%
    dplyr::group_by(unidad_geografica) %>%
    dplyr::group_modify(~{
      v <- .x$v
      v <- v[is.finite(v)]
      n <- length(v)
      
      # Defaults seguros
      sen  <- 0
      pval <- 1
      
      # Theil–Sen solo si hay >= 2 puntos y varianza > 0
      if (n >= 2 && stats::sd(v) > 0) {
        sen <- tryCatch(
          {
            est <- trend::sens.slope(v)$estimates
            if (length(est) > 0 && is.finite(est[1])) as.numeric(est[1]) else 0
          },
          error = function(e) 0
        )
      }
      
      # Mann–Kendall solo si hay >= 3 puntos y varianza > 0
      if (n >= 3 && stats::sd(v) > 0) {
        pval <- tryCatch(
          {
            mk <- Kendall::MannKendall(v)
            # 'sl' es numérico (p-value)
            as.numeric(mk$sl)
          },
          error = function(e) 1
        )
        if (!is.finite(pval)) pval <- 1
      }
      
      tibble::tibble(sen = sen, mk_p = pval)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      variable = varname,
      dir = dplyr::case_when(
        mk_p < 0.05 & sen > 0 ~ "↑ sig.",
        mk_p < 0.05 & sen < 0 ~ "↓ sig.",
        TRUE ~ "ns"
      )
    )
}


# datos_norm 

mk_tbl <- purrr::map_dfr(setdiff(vars_obj, var_predio), ~calc_mk(panel_anual, .x))


mk_tbl <- map_dfr(setdiff(vars_obj, var_predio), ~calc_mk(panel_anual, .x))
mk_sum <- mk_tbl %>% count(variable, dir) %>% group_by(variable) %>%
  mutate(pct = 100*n/sum(n)) %>% ungroup()
message("\n[Mann–Kendall] % asociaciones por dirección de tendencia:")
print(mk_sum %>% arrange(variable, desc(dir)), n = nrow(mk_sum))
readr::write_csv(mk_sum, "moscal_outputs/moscal_mk_summary_normalized.csv")

g_mk <- ggplot(mk_sum, aes(pct, fct_reorder(variable, pct, .fun=max), fill=dir)) +
  geom_col() + labs(title="Tendencias por asociación (Theil–Sen + Mann–Kendall (p<0.05)",
                    x="% de asociaciones", y=NULL, fill=NULL) +
  scale_fill_manual(values=c("↑ sig."="#2ca02c","↓ sig."="#d62728","ns"="#bbbbbb")) +
  theme_minimal(base_size = 12)
ggsave("moscal_outputs/mk_bars.png", g_mk, width=7, height=6, dpi=300)
print(g_mk)

# ---- Heatmap cambio 2018→2024 por asociación×variable ----
prepost_assoc_tbl <- function(df, varname){
  g <- df %>% filter(nombre_variable==varname, year %in% c(2018, 2024)) %>%
    dplyr::select(unidad_geografica, year, mean_value) %>%
    pivot_wider(names_from=year, values_from=mean_value, names_prefix="y_") %>%
    drop_na(y_2018, y_2024)
  if(nrow(g)==0) return(NULL)
  if(varname %in% vars_unitless){
    g %>% mutate(change = y_2024 - y_2018, metric = "Δ unidades")
  } else {
    g %>% filter(y_2018>0, y_2024>0) %>%
      mutate(change = 100*(y_2024/y_2018 - 1), metric = "Δ %")
  }
}

# epsilon por variable: 1% de la mediana en 2018 (evita dividir por ~0)

BASE <- 2018; TARGET <- 2024
eps_tbl <- panel_anual %>% filter(year == BASE) %>%
  group_by(nombre_variable) %>%
  summarise(eps = pmax(1e-6, 0.01 * median(mean_value, na.rm = TRUE)), .groups="drop")


hm_df <- purrr::map_dfr(setdiff(vars_obj, var_predio), function(v){
  e <- eps_tbl %>% filter(nombre_variable == v) %>% pull(eps); if(length(e)==0) e <- 1e-6
  d <- panel_anual %>% filter(nombre_variable == v, year %in% c(BASE, TARGET)) %>%
    dplyr::select(unidad_geografica, year, mean_value) %>%
    tidyr::pivot_wider(names_from = year, values_from = mean_value, names_prefix = "y_") %>%
    tidyr::drop_na(y_2018, y_2024) %>%
    mutate(variable = v,
           change_pct = 100 * (exp(log(pmax(y_2024,0)+e) - log(pmax(y_2018,0)+e)) - 1))
})


g_heat <- hm_df %>%
  mutate(unidad_geografica = forcats::fct_reorder(unidad_geografica, change_pct, .fun = median, .desc = TRUE)) %>%
  ggplot(aes(variable, unidad_geografica, fill = change_pct)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-100, 100), oob = scales::squish,
                       low="#d62728", mid="white", high="#2ca02c",
                       midpoint = 0, name = "Δ% 2018→2024") +
  labs(title = "Cambio 2018→2024 por asociación y variable (19 asocs)",
       x = NULL, y = NULL) +
  theme_minimal(11) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("moscal_outputs/heatmap_prepost_assoc.png", g_heat, width=9, height=8, dpi=300)
print(g_heat)

# ---- Dumbbell ejemplo (% Bosque) ----
c("Superficie de Bosque","Superficie de Pasto", "Superficie de Vegetación secundaria")
sup_bosque <- "Superficie de Bosque"
sup_pastos <- "Superficie de Pasto"
sup_vegsec <- "Superficie de Vegetación secundaria"
sup_ciq <- "Área de cicatrices de quema"

db_df_bosque <- prepost_assoc_tbl(panel_anual, sup_bosque)
db_df_pastos <- prepost_assoc_tbl(panel_anual, sup_pastos)
db_df_vegsec <- prepost_assoc_tbl(panel_anual, sup_vegsec)
db_df_ciq <- prepost_assoc_tbl(panel_anual, sup_ciq)

# if(!is.null(db_df)){
  
  g_dumbbell_bosque <- db_df_bosque %>%
    pivot_longer(cols = c(y_2018, y_2024), names_to = "year", values_to = "value") %>%
    mutate(year = recode(year, y_2018 = "2018", y_2024 = "2024")) %>%
    ggplot(aes(y = fct_reorder(unidad_geografica, change))) +
    # Flecha que indica dirección del cambio
    geom_segment(data = db_df_bosque,
                 aes(x = y_2018, xend = y_2024, y = unidad_geografica, yend = unidad_geografica),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "black", alpha = 0.8) +
    # Puntos con color por año
    geom_point(aes(x = value, color = year), size = 3) +
    # Escala de color manual
    scale_color_manual(values = c("2018" = "forestgreen", "2024" = "firebrick"),
                       name = "Año") +
    labs(title = glue("{sup_bosque} (normalizado)"),
         x = "Valor anual medio", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  g_dumbbell_pastos <- db_df_pastos %>%
    pivot_longer(cols = c(y_2018, y_2024), names_to = "year", values_to = "value") %>%
    mutate(year = recode(year, y_2018 = "2018", y_2024 = "2024")) %>%
    ggplot(aes(y = fct_reorder(unidad_geografica, change))) +
    # Flecha que indica dirección del cambio
    geom_segment(data = db_df_pastos,
                 aes(x = y_2018, xend = y_2024, y = unidad_geografica, yend = unidad_geografica),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "black", alpha = 0.8) +
    # Puntos con color por año
    geom_point(aes(x = value, color = year), size = 3) +
    # Escala de color manual
    scale_color_manual(values = c("2018" = "forestgreen", "2024" = "firebrick"),
                       name = "Año") +
    labs(title = glue("{sup_pastos} (normalizado)"),
         x = "Valor anual medio", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  
  g_dumbbell_vegsec <- db_df_vegsec %>%
    pivot_longer(cols = c(y_2018, y_2024), names_to = "year", values_to = "value") %>%
    mutate(year = recode(year, y_2018 = "2018", y_2024 = "2024")) %>%
    ggplot(aes(y = fct_reorder(unidad_geografica, change))) +
    # Flecha que indica dirección del cambio
    geom_segment(data = db_df_vegsec,
                 aes(x = y_2018, xend = y_2024, y = unidad_geografica, yend = unidad_geografica),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "black", alpha = 0.8) +
    # Puntos con color por año
    geom_point(aes(x = value, color = year), size = 3) +
    # Escala de color manual
    scale_color_manual(values = c("2018" = "forestgreen", "2024" = "firebrick"),
                       name = "Año") +
    labs(title = glue("{sup_vegsec} (normalizado)"),
         x = "Valor anual medio", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  
  g_dumbbell_ciq <- db_df_ciq %>%
    pivot_longer(cols = c(y_2018, y_2024), names_to = "year", values_to = "value") %>%
    mutate(year = recode(year, y_2018 = "2018", y_2024 = "2024")) %>%
    ggplot(aes(y = fct_reorder(unidad_geografica, change))) +
    # Flecha que indica dirección del cambio
    geom_segment(data = db_df_ciq,
                 aes(x = y_2018, xend = y_2024, y = unidad_geografica, yend = unidad_geografica),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "black", alpha = 0.8) +
    # Puntos con color por año
    geom_point(aes(x = value, color = year), size = 3) +
    # Escala de color manual
    scale_color_manual(values = c("2018" = "forestgreen", "2024" = "firebrick"),
                       name = "Año") +
    labs(title = glue("{sup_ciq} (normalizado)"),
         x = "Valor anual medio", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  
  ggsave("moscal_outputs/dumbbell_bosque.png", g_dumbbell_bosque, width=7, height=8, dpi=300)
  
  
  g_dumbbell_bosque / g_dumbbell_pastos 
  
  g_dumbbell_vegsec / g_dumbbell_ciq
  
# }

# ---- PCA sobre DELTAS (2024–2018) ----
deltas <- panel_anual %>%
  filter(nombre_variable != var_predio, year %in% c(2018, 2023)) %>%
  dplyr::select(unidad_geografica, nombre_variable, year, mean_value) %>%
  pivot_wider(names_from=year, values_from=mean_value, names_prefix="y_") %>%
  drop_na(y_2018, y_2023) %>%
  mutate(delta = y_2023 - y_2018) %>%
    dplyr::select(unidad_geografica, nombre_variable, delta) %>%
  pivot_wider(names_from=nombre_variable, values_from=delta) %>%
  column_to_rownames("unidad_geografica") %>%
  mutate(across(everything(), ~as.numeric(scale(.x))))

if(nrow(deltas) >= 3){
  pca_delta <- FactoMineR::PCA(deltas, scale.unit = FALSE, graph = FALSE)
  g_pca <- factoextra::fviz_pca_biplot(pca_delta, repel=TRUE, habillage = ) +
    labs(title="PCA sobre cambios (Δ=2024–2018) — variables normalizadas")
  ggsave("moscal_outputs/pca_deltas.png", g_pca, width=8, height=6, dpi=300)
  print(g_pca)
}
  
  factoextra::fviz_pca_var(pca_delta, col.var = "contrib", repel = TRUE,
                           gradient.cols = c("white", "blue", "red"),
                           ggtheme = theme_minimal())

message("\nListo. Archivos guardados en 'moscal_outputs/'. Usa las tablas y gráficos para redactar el cierre: ")
message(" • Variables con FDR<0.05 en pre/post y signos concordantes con LMM → cambios robustos.")
message(" • % de asociaciones ↑/↓ (MK) respalda la dirección global.")


# # --- 8) Índices compuestos (z-scores) y pre/post ---
# make_index <- function(df_year_wide){
#   df_year_wide %>%
#     mutate(
#       idx_fuego = scale(`Cantidad de puntos de calor`) + scale(`Área de cicatrices de quema`),
#       idx_integridad = scale(`Superficie de Bosque`) + scale(`Índice de Conectividad`) -
#         scale(`Grado de Fragmentación`) - scale(`Área de Paisaje Agropecuario continuo`)
#     )
# }
# 
# wide_prepost <- panel_anual %>%
#   filter(nombre_variable != var_predio, year %in% c(2018, 2024)) %>%
#   dplyr::select(unidad_geografica, nombre_variable, year, mean_value) %>%
#   pivot_wider(names_from = nombre_variable, values_from = mean_value) %>%
#   group_by(year) %>%
#   group_modify(~ make_index(.x)) %>%
#   ungroup()
# 
# idx_prepost <- wide_prepost %>%
#   dplyr::select(unidad_geografica, year, idx_fuego, idx_integridad) %>%
#   pivot_wider(names_from = year, values_from = c(idx_fuego, idx_integridad), names_prefix = "y_") %>%
#   drop_na()
# 
# # prueba pareada sobre índices
# t_idx_fuego <- t.test(idx_prepost$idx_fuego_y_2024, idx_prepost$idx_fuego_y_2018, paired = TRUE)
# t_idx_int   <- t.test(idx_prepost$idx_integridad_y_2024, idx_prepost$idx_integridad_y_2018, paired = TRUE)
# 
# tbl_indices <- tibble(
#   indice = c("Presión de Fuego","Integridad del Paisaje"),
#   diff = c(mean(idx_prepost$idx_fuego_y_2024 - idx_prepost$idx_fuego_y_2018),
#            mean(idx_prepost$idx_integridad_y_2024 - idx_prepost$idx_integridad_y_2018)),
#   ci_low = c(unname(t_idx_fuego$conf.int[1]), unname(t_idx_int$conf.int[1])),
#   ci_high = c(unname(t_idx_fuego$conf.int[2]), unname(t_idx_int$conf.int[2])),
#   p_value = c(t_idx_fuego$p.value, t_idx_int$p.value)
# )
# 
# g_idx <- ggplot(tbl_indices, aes(x = diff, y = indice, xmin = ci_low, xmax = ci_high)) +
#   geom_vline(xintercept = 0, linetype = 2) +
#   geom_errorbarh(height = 0) +
#   geom_point(size = 2) +
#   labs(title = "Pre/Post 2018→2024 — Índices compuestos (z-score)",
#        x = "Diferencia (IC95%)", y = NULL) +
#   theme_minimal(base_size = 12)

# --- 9) Exportables (tablas) ---
prepost_tbl %>% arrange(fdr, p_value) %>% readr::write_csv("moscal_prepost_summary_normalized.csv")
lmm_tbl      %>% arrange(fdr, p_value) %>% readr::write_csv("moscal_lmm_slopes_normalized.csv")
mk_sum       %>% arrange(variable, desc(dir)) %>% readr::write_csv("moscal_mk_summary_normalized.csv")

# --- 10) Mostrar gráficos clave ---
(forest_ratio | g_lmm_pct)
g_heat | g_mk
g_dumbbell_bosque / g_dumbbell_pastos | g_dumbbell_vegsec / g_dumbbell_ciq
g_pca


