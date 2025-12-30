## Prepare data MoSCAL - Indicadores - cobertura

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
Cb_moscal         <- sf::st_read(gdb_moscal,      layer = "Cb_PRE")
Cb_testigo         <- sf::st_read(gdb_testigo,      layer = "Cb_PRE")

Cpvs_moscal         <- sf::st_read(gdb_moscal,      layer = "Cpvs_PRE")
Cpvs_testigo         <- sf::st_read(gdb_testigo,      layer = "Cpvs_PRE")

Cvsb_moscal         <- sf::st_read(gdb_moscal,      layer = "Cvsb_PRE")
Cvsb_testigo         <- sf::st_read(gdb_testigo,      layer = "Cvsb_PRE")

PbRF_moscal         <- sf::st_read(gdb_moscal,      layer = "PbRF_PRE")
PbRF_testigo         <- sf::st_read(gdb_testigo,      layer = "PbRF_PRE")

PCbp_moscal         <- sf::st_read(gdb_moscal,      layer = "PCbp_PRE")
PCbp_testigo         <- sf::st_read(gdb_testigo,      layer = "PCbp_PRE")

Vp_moscal         <- sf::st_read(gdb_moscal,      layer = "Vp_PRE")
Vp_testigo         <- sf::st_read(gdb_testigo,      layer = "Vp_PRE")

Vvs_moscal         <- sf::st_read(gdb_moscal,      layer = "Vvs_PRE")
Vvs_testigo         <- sf::st_read(gdb_testigo,      layer = "Vvs_PRE")


# ========================== Unir con dat Panel 2017–2024 ==========================

# 1. Asegurar que los periodos de dat estén ordenados con tu función
panel_raw <- panel_raw %>%
  mutate(periodo = orden_periodo(periodo))

# 2. Crear columna con el siguiente periodo
dat2 <- panel_raw %>%
  mutate(periodo = orden_periodo(periodo)) %>%
  group_by(cu) %>%
  arrange(periodo, .by_group = TRUE) %>%
  mutate(periodo_siguiente = lead(periodo)) %>%
  ungroup() %>%
  # 3. Construir la clave "periodo_periodo_siguiente"
  mutate(periodos = ifelse(periodo == "2017_JUL",
                           paste(periodo, "2018_ENE", sep = "_"),
                           paste("2017_JUL", periodo, sep = "_"))) %>%
  mutate(periodos2 = paste(periodo, periodo_siguiente, sep = "_"))

# 4. Hacer el left_join con Cb_moscal

# Función genérica para unir un indicador
join_indicador <- function(dat2, moscal, testigo, nombre_indicador, key = "periodos") {
  
  if(key != "periodos"){
    
    dat2 <- dat2 %>% mutate(periodos = periodos2)
    
  }
  
  dat_con <- dat2 %>%
    filter(grupo == "CON_PROYECTO") %>%
    left_join(moscal %>%
                st_drop_geometry() %>%
                select(cu, periodos, !!nombre_indicador := valor_indicador),
              by = c("cu", "periodos"))
  
  dat_sin <- dat2 %>%
    filter(grupo == "SIN_PROYECTO") %>%
    left_join(testigo %>%
                st_drop_geometry() %>%
                select(cu, periodos, !!nombre_indicador := valor_indicador),
              by = c("cu", "periodos"))
  
  bind_rows(dat_con, dat_sin)
}

# --- Uso de la función para cada indicador ---
dat_join <- dat2 %>%
  # Cb
  join_indicador(Cb_moscal, Cb_testigo, "Cb", "periodos") %>%
  # Cpvs
  join_indicador(Cpvs_moscal, Cpvs_testigo, "Cpvs", "periodos2") %>%
  # Cvsb
  join_indicador(Cvsb_moscal, Cvsb_testigo, "Cvsb", "periodos2") %>%
  # PbRF
  join_indicador(PbRF_moscal, PbRF_testigo, "PbRF", "periodos2") %>%
  # PCbp
  join_indicador(PCbp_moscal, PCbp_testigo, "PCbp", "periodos2") %>%
  # Vp
  join_indicador(Vp_moscal, Vp_testigo, "Vp", "periodos2") %>%
  # Vvs
  join_indicador(Vvs_moscal, Vvs_testigo, "Vvs", "periodos2") %>%
  # Filtrar como en tu script original
  filter(asoc %in% unique(dat$asoc),
         cu %in% unique(dat$cu),
         periodo %in% unique(dat$periodo))



# c) Imputación múltiple ---------------
library(mice)
# 1.1  Crea un data.frame solo con las variables a imputar
vars_to_impute <- dat_join %>% 
  dplyr::select(Cb:Vvs)

# 1.2  Revisa la proporción de NA (ya sabemos que es ≤30 %)
naniar::miss_var_summary(vars_to_impute)

# 2.1  Objeto inicial (m = 0 solo como 'dry‑run')
skimr::skim(vars_to_impute)

## mice alg
ini <- mice(vars_to_impute)  # ahora sí funciona

# vis_miss(complete(ini))
impute_data <- complete(ini)


# dat_join$Cb %>% hist




dat_join <- dat_join |>
  dplyr::select(cu, asoc, grupo, periodo) |>
  bind_cols(impute_data) 




density_plot_ind_moscal <- dat_join %>% dplyr::select(grupo, Cb:Vvs) %>%
  pivot_longer(cols = c(Cb:Vvs)) %>%
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
    title = "Distribución de Indicadores por grupo",
    subtitle = "Curvas de densidad usando todos los datos"
  )

violin_plot_ind_moscal <- dat_join %>% dplyr::select(grupo, Cb:Vvs) %>%
  pivot_longer(cols = c(Cb:Vvs)) %>%
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
    title = "Distribución de Indicadores por grupo",
    subtitle = "Violin plots con boxplots internos"
  )

histogram_plot_ind_moscal <- dat_join %>% dplyr::select(grupo, Cb:Vvs) %>%
  pivot_longer(cols = c(Cb:Vvs)) %>%
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
    title = "Distribución de Indicadores por grupo",
    subtitle = "Histogramas usando todos los datos"
  )



moscal_indicadores_predios <- dat_join 

