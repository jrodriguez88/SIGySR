### Analisis encuestas MoSCAL
## github.com/jrodriguez88
## Nov-2025

## Paquetes

library(tidyverse)
library(readxl)
library(naniar)
library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(janitor)


## params and data 

data_rw <- read_excel("data/Moscal/3 Kpre_CAso_Encuestas_v1.xlsx", sheet = "Kpre_ConEncuestas")


names(data_rw)


data_rw$`Año de línea base` %>% table



# 1) Filtrar solo línea base 2017
data_lb <- data_rw %>%
  filter(`Año de línea base` == 2017)

# 2) Seleccionar columnas de interés
variables_seleccionadas <- c(
  "r483_grupoetnico",
  "r5_genero",
  "P7_relacionpredio",
  "r140_distancia",
  "P18_Interconexion",
  "P46_TechoMadera",
  "P47_TechoPaja",
  "P76_ParedesGuadua",
  "P77_ParedesMadera",
  "P83_Madera",
  "P50_Numerohabitaciones",
  "P51_Personasporhabitacion",
  "P60_Alcantarillado",
  "P65_Acceso_educacion",
  "P64_saludeducacionmodalidadsalud",
  "P101_p1_sexo",
  "P287_$Unidad",
  "P291_$Unidad",
  "p445_obseringresoanualpredio",
  "P451_Seguridadalimenaria",
  "P866_No_ASOCIACIONES",
  "p867_cualesasociaciones",
  "r758_ultimogradoescopropietario"
)

data_encuesta <- data_lb %>%
  select(
    objectid,
    codigo_uer        = `Código UER`,
    codigo_unico      = `Código único`,
    nombre_predio     = `Nombre predio`,
    area_predio       = `Área (ha)`,
    nombre_asociacion = `Nombre UER padre`,
    Vereda,
    departamento      = `Departamento espacial`,
    municipio         = `Municipio espacial`,
    car               = `Corporación Autonoma Regional Espacial`,
    all_of(
      variables_seleccionadas %>%
        str_replace_all(fixed("$"), "_") %>%
        tolower()
    )
  )

# 3) Limpieza / recodificación básica
data_encuesta <- data_encuesta %>%
  mutate(
    # estandarizar textos
    across(
      where(is.character),
      ~ .x %>%
        str_squish()
    ),
    # grupo étnico simplificado
    r483_grupoetnico = str_to_lower(r483_grupoetnico),
    r483_grupoetnico = case_when(
      str_detect(r483_grupoetnico, "afro|negro")            ~ "afro",
      str_detect(r483_grupoetnico, "indig")                 ~ "indigena",
      str_detect(r483_grupoetnico, "raizal")                ~ "raizal",
      str_detect(r483_grupoetnico, "ninguno|ningun|ningun ") ~ "ninguno",
      str_detect(r483_grupoetnico, "^no$|^0$|no aplica")    ~ "ninguno",
      TRUE                                                  ~ NA_character_
    ),
    # nivel educativo propietario
    r758_ultimogradoescopropietario = str_to_lower(r758_ultimogradoescopropietario),
    r758_ultimogradoescopropietario = case_when(
      str_detect(r758_ultimogradoescopropietario, "bachiller") ~ "secundaria",
      str_detect(r758_ultimogradoescopropietario, "primaria")  ~ "basica",
      str_detect(r758_ultimogradoescopropietario, "preescolar|pre-escolar") ~ "ninguno",
      str_detect(r758_ultimogradoescopropietario, "tecnic|tecno|tecnól|vocacion") ~ "tecnico_tecnologo",
      str_detect(r758_ultimogradoescopropietario, "profesional|pregrado|especialista|maestr") ~ "superior",
      str_detect(r758_ultimogradoescopropietario, "ninguno|^0$|no aplica") ~ "ninguno",
      TRUE ~ NA_character_
    ),
    # materiales / servicios: normalizar SI/NO
    p77_paredesmadera = case_when(
      str_detect(str_to_lower(p77_paredesmadera), "si|true") ~ "si",
      str_detect(str_to_lower(p77_paredesmadera), "no|false") ~ "no",
      TRUE ~ NA_character_
    ),
    p83_madera = case_when(
      str_detect(str_to_lower(p83_madera), "si|true") ~ "si",
      str_detect(str_to_lower(p83_madera), "no|false") ~ "no",
      TRUE ~ NA_character_
    ),
    r5_genero = case_when(
      str_detect(str_to_lower(r5_genero), "^0$") ~ NA_character_,
      TRUE ~ r5_genero
    ),
    p7_relacionpredio = case_when(
      str_detect(str_to_lower(p7_relacionpredio), "^0$") ~ NA_character_,
      TRUE ~ p7_relacionpredio
    ),
    p65_acceso_educacion = case_when(
      str_detect(str_to_lower(p65_acceso_educacion), "si|true") ~ "si",
      str_detect(str_to_lower(p65_acceso_educacion), "no|false|^0$|no tiene") ~ "no",
      str_detect(r758_ultimogradoescopropietario, "ninguno|^0$|no aplica") ~ NA_character_,
      TRUE ~ NA_character_
    ),
    # seguridad alimentaria → escala 0–5
    p451_seguridadalimenaria = str_squish(p451_seguridadalimenaria),
    p451_seguridadalimenaria = case_when(
      str_detect(p451_seguridadalimenaria, "^0")                         ~ "menos_10",
      str_detect(p451_seguridadalimenaria, "^1")                         ~ "menos_10",
      str_detect(p451_seguridadalimenaria, "^2")                         ~ "10_20",
      str_detect(p451_seguridadalimenaria, "^3")                         ~ "20_50",
      str_detect(p451_seguridadalimenaria, "^4")                         ~ "50_70",
      str_detect(p451_seguridadalimenaria, "^5")                         ~ "mas_70",
      TRUE ~ NA_character_
    )
  )

# 4) Definir factores clave
factores <- c(
  "nombre_asociacion" , "departamento", "municipio", "car",
  "r483_grupoetnico" , "r5_genero", "p7_relacionpredio",
  "p77_paredesmadera", "p65_acceso_educacion",
  "p64_saludeducacionmodalidadsalud", "p101_p1_sexo",
  "p451_seguridadalimenaria", "p867_cualesasociaciones",
  "r758_ultimogradoescopropietario"
)

data_encuesta <- data_encuesta %>%
  mutate(
    across(all_of(factores), ~ as.factor(.x)),
    p451_seguridadalimenaria = fct_relevel(p451_seguridadalimenaria, c("menos_10","10_20","20_50","50_70","mas_70")) #,
   # p445_obseringresoanualpredio = case_when(p445_obseringresoanualpredio < 6000000 ~ NA_real_,
    #                                         TRUE ~p445_obseringresoanualpredio)
  )


###GLOBAL
skimr::skim(data_encuesta)
data_encuesta %>% group_by(nombre_asociacion) %>% skimr::skim()

# NUMÉRICAS
resumen_num <- data_encuesta %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    n         = n(),
    n_miss    = sum(is.na(valor)),
    pct_miss  = 100 * mean(is.na(valor)),
    mean      = mean(valor, na.rm = TRUE),
    sd        = sd(valor, na.rm = TRUE),
    min       = min(valor, na.rm = TRUE),
    p25       = quantile(valor, 0.25, na.rm = TRUE),
    p50       = quantile(valor, 0.50, na.rm = TRUE),
    p75       = quantile(valor, 0.75, na.rm = TRUE),
    max       = max(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_miss))

# CATEGÓRICAS
resumen_cat <- data_encuesta %>%
  select(where(is.factor), where(is.character)) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  group_by(variable) %>%
  summarise(
    n         = n(),
    n_miss    = sum(is.na(categoria)),
    pct_miss  = 100 * mean(is.na(categoria)),
    n_levels  = n_distinct(categoria, na.rm = TRUE),
    top_cat   = fct_infreq(categoria) %>% fct_explicit_na("NA") %>% levels() %>% .[1],
    .groups = "drop"
  ) %>%
  arrange(desc(pct_miss))

resumen_cat %>% print(n= 22)



data_encuesta %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    -objectid,  # si quieres excluir el ID, si no quita esto
    names_to = "variable",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(x = NULL, y = "Frecuencia", title = "Distribución de variables numéricas (línea base 2017)")




vars_cat_clave <- c(
  "r483_grupoetnico",
  "r5_genero",
#  "p7_relacionpredio",
  "p65_acceso_educacion",
  "r758_ultimogradoescopropietario",
  "p451_seguridadalimenaria",
  "p64_saludeducacionmodalidadsalud"
)

data_encuesta %>%
  select(nombre_asociacion, all_of(vars_cat_clave)) %>%
  pivot_longer(-nombre_asociacion ,
    names_to = "variable",
    values_to = "categoria"
  ) %>%
  filter(!is.na(categoria)) %>%
  group_by(nombre_asociacion, variable, categoria) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(categoria, prop), y = prop)) +
  geom_col() +
  coord_flip() +
  facet_grid(variable ~ nombre_asociacion , scales = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(x = NULL, y = "% de predios", title = "Distribución de variables categóricas clave")



# Considerando el anterior grafico y las tablas resumen, 
# podemos descartar ASOES y CUACHEROS, y variables como p64_saludeducacionmodalidadsalud

data_encuesta %>% dplyr::select(-c(p7_relacionpredio)) %>% 
  select(nombre_asociacion, all_of(vars_cat_clave)) %>% 
#  dplyr::filter(str_detect(nombre_asociacion, "ASOES|CAUCHEROS", negate = T)) %>%
  dplyr::select(-p64_saludeducacionmodalidadsalud) %>%
  pivot_longer(-nombre_asociacion ,
               names_to = "variable",
               values_to = "categoria"
  ) %>%
  filter(!is.na(categoria)) %>%
  group_by(nombre_asociacion, variable, categoria) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(categoria, prop), y = prop)) +
  geom_col() +
  coord_flip() +
  facet_grid(variable ~ nombre_asociacion , scales = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(x = NULL, y = "% de predios", title = "Distribución de variables categóricas clave")




#data_encuesta  %>% skimr::skim()



# data_encuesta %>% group_by(nombre_asociacion) %>%
#   summarise(n = n(), across(where(is.numeric), .fns = median, na.rm = T))



data_encuesta %>% 
  filter(nombre_asociacion ==  "ASECADY") %>% 
  pull(p50_numerohabitaciones) %>% head(20)




vars_num <- c(
  "area_predio",
  "r140_distancia",
  "p50_numerohabitaciones",
  "p51_personasporhabitacion",
  "p287__unidad",
  "p291__unidad",
  "p445_obseringresoanualpredio",
  "p866_no_asociaciones"
)

data_encuesta %>%
  select(all_of(vars_num)) %>%
  pivot_longer(
    everything(),
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_wrap(~ variable, scale = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Panel de boxplots de variables numéricas (vertical)"
  )

  


data_encuesta_num <- data_encuesta %>%
  mutate(
    prop_no_na_num = rowMeans(!is.na(across(all_of(vars_num))))
  ) %>%
  filter(prop_no_na_num >= 0.50)



boot_stats_var <- function(x,
                           B = 2000,
                           umbral_area = 100,
                           umbral_ingreso = 20000000,
                           var_name = "") {
  x <- x[!is.na(x)]
  n_eff <- length(x)
  if (n_eff < 5) {
    return(
      tibble(
        n = n_eff,
        mean = NA_real_, mean_l = NA_real_, mean_u = NA_real_,
        median = NA_real_, median_l = NA_real_, median_u = NA_real_,
        sd = NA_real_,
        p25 = NA_real_, #p25_l = NA_real_, p25_u = NA_real_,
        p75 = NA_real_#, p75_l = NA_real_, p75_u = NA_real_,
        
      )
    )
  }
  
  # Remuestras bootstrap
  boot_sample <- function(fun) {
    replicate(B, {
      xb <- sample(x, size = n_eff, replace = TRUE)
      fun(xb)
    })
  }
  
  # Estadísticos en la muestra original
  est_mean   <- mean(x, na.rm = TRUE)
  est_median <- median(x, na.rm = TRUE)
  est_sd     <- sd(x, na.rm = TRUE)
#  est_p10    <- quantile(x, 0.10, na.rm = TRUE)
  est_p25    <- quantile(x, 0.25, na.rm = TRUE)
  est_p75    <- quantile(x, 0.75, na.rm = TRUE)
#  est_p90    <- quantile(x, 0.90, na.rm = TRUE)
  

  # 
  # Bootstrap distribuciones
  boot_mean   <- boot_sample(mean)
  boot_median <- boot_sample(median)
#  boot_p10    <- boot_sample(function(z) quantile(z, 0.10, na.rm = TRUE))
  boot_p25    <- boot_sample(function(z) quantile(z, 0.25, na.rm = TRUE))
  boot_p75    <- boot_sample(function(z) quantile(z, 0.75, na.rm = TRUE))
 # boot_p90    <- boot_sample(function(z) quantile(z, 0.90, na.rm = TRUE))
  
  # IC percentil 95%
  ci <- function(v) quantile(v, c(0.025, 0.975), na.rm = TRUE)
  
  ci_mean   <- ci(boot_mean)
  ci_median <- ci(boot_median)
 # ci_p10    <- ci(boot_p10)
  ci_p25    <- ci(boot_p25)
  ci_p75    <- ci(boot_p75)
#  ci_p90    <- ci(boot_p90)

  

  tibble(
    n       = n_eff,
    mean    = est_mean,
#    mean_boot = mean(boot_mean, na.rm = T),
    mean_l  = ci_mean[1],
    mean_u  = ci_mean[2],
    median  = est_median,
#    median_boot = mean(boot_median, na.rm = T),
    median_l = ci_median[1],
    median_u = ci_median[2],
    sd = est_sd,
    p25     = est_p25,
    p75     = est_p75
  )
}



set.seed(123)  # para reproducibilidad

resumen_global <- data_encuesta_num %>%
  select(all_of(vars_num)) %>%
  pivot_longer(
    everything(),
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  group_modify(~ {
    boot_stats_var(
      x        = .x$valor,
      B        = 2000,
      var_name = unique(.y$variable)
    )
  }) %>%
  ungroup()



set.seed(123)

resumen_asoc <- data_encuesta_num %>%
  select(nombre_asociacion, all_of(vars_num)) %>%
  pivot_longer(
    cols      = all_of(vars_num),
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  group_by(nombre_asociacion, variable) %>%
  group_modify(~ {
    boot_stats_var(
      x        = .x$valor,
      B        = 2000,
      var_name = unique(.y$variable)
    )
  }) %>%
  ungroup()


# resumen_asoc %>%
#   filter(variable == "area_predio")
# 
# resumen_asoc %>%
#   filter(variable == "p445_obseringresoanualpredio")


# estos graficos deben ir en el rmardown



# vars_num %>% map(~resumen_asoc %>%
#   filter(variable == .x) %>%
#   ggplot(aes(
#     x = reorder(nombre_asociacion, median),
#     y = median
#   )) +
#   geom_point() +
#   geom_errorbar(aes(ymin = median_l, ymax = median_u), width = 0) +
#   coord_flip() +
#   theme_minimal() +
#   labs(
#     x = NULL,
#     y = paste("Mediana de ", .x),
#     title = paste0(.x, "(mediana e IC95% bootstrap)")
#   ))


resumen_global


resumen_asoc %>%
  ggplot(aes(
    x = reorder(nombre_asociacion, median),
    y = median
  )) +
  geom_point() +
  geom_errorbar(aes(ymin = median_l, ymax = median_u), width = 0) +
  coord_flip() +
  facet_grid(~variable, scale ="free") +
                   theme_bw() +
                   labs(
                     x = NULL,
                     y = paste("Mediana"),
                     title = paste0("(mediana e IC95% bootstrap)")
                   )


resumen_asoc %>%
  ggplot(aes(
    x = reorder(nombre_asociacion, median),
    y = mean
  )) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_l, ymax = mean_u), width = 0) +
  coord_flip() +
  facet_grid(~variable, scale ="free") +
  theme_bw() +
  labs(
    x = NULL,
    y = paste("Media"),
    title = paste0("(Media e IC95% bootstrap)")
  )

write_csv(resumen_global, "resumen_global.csv")
write_csv(resumen_asoc, "resumen_asoc.csv")



# tag <- c("Area_predio_(ha)", "Valor_Leche_($/lt)", "Ingreso_anual_($)", "No_habitaciones", "Personas/habitacion", "No_asociaciones", "Distancia_Municipio_(km)" )
# 
# 
# resumen_asoc %>% filter(variable!= "p291__unidad", nombre_asociacion != "ASOES") %>% 
#   ggplot(aes(
#     x = reorder(nombre_asociacion, mean),
#     y = mean
#   )) +
#   # Banda de IC global (fondo rojo suave)
#   geom_rect(
#     data = resumen_global %>% filter(variable!= "p291__unidad"),
#     aes(
#       xmin = -Inf, xmax = Inf,
#       ymin = mean_l, ymax = mean_u
#     ),
#     inherit.aes = FALSE,
#     fill = "red", alpha = 0.08
#   ) +
#   # Línea de media global (roja punteada)
#   geom_hline(
#     data = resumen_global%>% filter(variable!= "p291__unidad"),
#     aes(yintercept = mean),
#     inherit.aes = FALSE,
#     color = "red",
#     linetype = "dashed",
#     linewidth = 0.4
#   ) +
#   # Puntos y barras por asociación
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_l, ymax = mean_u), width = 0) +
#   coord_flip() +
#   facet_grid(~ variable, scales = "free") +
#   theme_bw() +
#   labs(
#     x = NULL,
#     y = "Media",
#     title = "Media por asociación e IC95% bootstrap\n(banda y línea = media global)"
#   )




tag <- c(
  "area_predio"                = "Area_predio_(ha)",
  "p287__unidad"              = "Valor_Leche_($/lt)",
  "p445_obseringresoanualpredio" = "Ingreso_anual_($)",
  "p50_numerohabitaciones"    = "No_habitaciones",
  "p51_personasporhabitacion" = "Personas/habitacion",
  "p866_no_asociaciones"      = "No_asociaciones",
  "r140_distancia"            = "Distancia_Municipio_(km)"
)

resumen_asoc %>% 
  filter(variable != "p291__unidad",
         nombre_asociacion != "ASOES") %>% 
  ggplot(aes(
    x = reorder(nombre_asociacion, mean),
    y = mean
  )) +
  # Banda de IC global (fondo rojo suave)
  geom_rect(
    data = resumen_global %>% filter(variable != "p291__unidad"),
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = mean_l, ymax = mean_u
    ),
    inherit.aes = FALSE,
    fill = "red", alpha = 0.08
  ) +
  # Línea de media global (roja punteada)
  geom_hline(
    data = resumen_global %>% filter(variable != "p291__unidad"),
    aes(yintercept = mean),
    inherit.aes = FALSE,
    color = "red",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  # Puntos y barras por asociación
  geom_point() +
  geom_errorbar(aes(ymin = mean_l, ymax = mean_u), width = 0) +
  coord_flip() +
  facet_grid(
    ~ variable,
    scales   = "free",
    labeller = labeller(variable = tag)
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text       = element_text(face = "bold"),
    axis.title.x     = element_text(face = "bold"),
    axis.title.y     = element_text(face = "bold"),
    axis.text.x      = element_text(face = "bold", angle = 45, hjust = 1),
    axis.text.y      = element_text(face = "bold"),
    plot.title       = element_text(face = "bold")
  ) +
  labs(
    x     = NULL,
    y     = "Media",
    title = "Media por asociación e IC95% bootstrap\n(banda y línea = media global)"
  )





tags = c("Segur_Aliment_prod(%)", "Acceso_Educ_(%)", "Grupo_etnico", "Genero", "Escolaridad")


tags <- c(
  "p451_seguridadalimenaria"        = "Segur_Aliment_prod(%)",
  "p65_acceso_educacion"            = "Acceso_Educ_(%)",
  "r483_grupoetnico"                = "Grupo_etnico",
  "r5_genero"                       = "Genero",
  "r758_ultimogradoescopropietario" = "Escolaridad"
)

data_encuesta %>% 
  dplyr::select(-c(p7_relacionpredio)) %>% 
  select(nombre_asociacion, all_of(vars_cat_clave)) %>% 
  dplyr::filter(str_detect(nombre_asociacion, "ASOES|CAUCHEROS", negate = TRUE)) %>%
  dplyr::select(-p64_saludeducacionmodalidadsalud) %>%
  pivot_longer(
    -nombre_asociacion,
    names_to  = "variable",
    values_to = "categoria"
  ) %>%
  filter(!is.na(categoria)) %>%
  group_by(nombre_asociacion, variable, categoria) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(nombre_asociacion, variable) %>%      # <-- cambio clave
  mutate(prop = n / sum(n)) %>%                  # ahora suma a 1 por asoc+var
  ungroup() %>%
  ggplot(aes(x = fct_reorder(categoria, prop), y = prop)) +
  geom_col() +
  coord_flip() +
  facet_grid(
    variable ~ nombre_asociacion,
    scales   = "free",
    labeller = labeller(variable = tags)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x     = NULL,
    y     = "% de predios",
    title = "Distribución de variables categóricas clave"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text       = element_text(face = "bold"),
    axis.title.x     = element_text(face = "bold"),
    axis.title.y     = element_text(face = "bold"),
    axis.text.x      = element_text(face = "bold", angle = 45, hjust = 1),
    axis.text.y      = element_text(face = "bold"),
    plot.title       = element_text(face = "bold")
  )



library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

# Opcional: sacar IDs que no quieres en el heatmap
vars_na <- data_encuesta %>%
  select(-objectid, -codigo_uer, -codigo_unico, -nombre_predio) 

heat_na <- vars_na %>%
  pivot_longer(
    cols      = -nombre_asociacion,
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  group_by(nombre_asociacion, variable) %>%
  summarise(
    pct_na = mean(is.na(valor)) * 100,
    .groups = "drop"
  )

ggplot(heat_na, aes(
  x    = nombre_asociacion,
  y    = fct_rev(variable),
  fill = pct_na
)) +
  geom_tile(color = "grey80") +
  scale_fill_gradient(
    name   = "% NA",
    limits = c(0, 100),
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    x     = "Asociación",
    y     = "Variable",
    title = "% de valores NA por asociación y variable"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title   = element_text(face = "bold")
  )


library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

# 1) Opcional: sacar columnas que no quieres en el heatmap
vars_na <- data_encuesta %>%
  select(
    -objectid,
    -codigo_uer,
    -codigo_unico,
    -nombre_predio
    # deja nombre_asociacion y el resto
  )

# 2) %NA por variable dentro de cada asociación
ggplot(heat_na, aes(
  x    = nombre_asociacion,
  y    = fct_rev(variable),
  fill = pct_na
)) +
  geom_tile(color = "grey80") +
  scale_fill_gradientn(
    name    = "% NA",
    limits  = c(0, 100),
    labels  = function(x) paste0(round(x), "%"),
    colours = c("blue", "yellow", "red")
  ) +
  labs(
    x     = "Asociación",
    y     = "Variable",
    title = "% de valores NA por asociación y variable"
  ) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title   = element_text(face = "bold")
  )



