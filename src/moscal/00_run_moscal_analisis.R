# MoSCAL run control ----

## Dependencias - Requerimientos  - Config ----



## 



## Preparar data asociaciones ----


### Variables


### Indicadores

plotly::ggplotly(d_by_time)


### Encuestas 



### Panel final



## Preparar data predios ----
source("src/moscal/moscal_prepare_variables_predios.R")

### Variables

numero_predios_asoc | area_promedio_asoc | area_total_asoc
density_plot_var_moscal
histogram_plot_var_moscal
violin_plot_var_moscal


panel_raw
dat_imputed
dat

dat %>% dplyr::select(-pQ , -pO) %>%
  pivot_longer(cols = c(area_uer_ha, pB, pP, pV),
               names_to = "variable",
               values_to = "valor") %>%
ggplot(aes(x = asoc, y = valor, color = grupo)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange",
               position = position_dodge(width = 0.5)) +
  coord_flip() + 
  facet_grid(~ variable, scales = "free") +
  labs(title = "Media y dispersión por grupo y asociación",
       x = "Grupo", y = "Valor") +
  theme_bw()




source("src/moscal/moscal_coda_did_variables_predios.R")

fig_did_pp
fig_es
fig_asoc

plot_did_heatmap(did_2018_2024)
plot_did_slope(did_2018_2024)
plotly::ggplotly(plot_did_slope(did_2018_2024))
rank_did(did_2018_2024)
resume_did(did_2018_2024)

did_2018_2024 %>%
  group_by(asoc, Componente) %>%
  summarise(Delta_mediana = stats::median(DiD_2018_2024, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(abs(Delta_mediana))) %>%
  pivot_wider(names_from = Componente, values_from = Delta_mediana)


### Indicadores

moscal_indicadores_predios
density_plot_ind_moscal


### Encuestas 



### Panel final

data_encuesta_to <- data_encuesta %>% 
  # mutate(cu = str_sub(codigo_uer, end = -1, start = -6)) %>%
  # mutate(cu = codigo_unico) %>%
  dplyr::select(cu = codigo_unico, 
                genero = r5_genero, 
                seg_aliment  = p451_seguridadalimenaria,
                escolaridad = r758_ultimogradoescopropietario,
                acceso_edu = p65_acceso_educacion,
                ingreso_anual = p445_obseringresoanualpredio,
                distancia_municipio = r140_distancia) 



panel_predios_moscal <- moscal_variables_predios %>% 
  left_join(moscal_indicadores_predios) %>% 
  dplyr::select(-frac_mapeada) %>%
  mutate(asoc = factor(asoc),
         grupo = factor(grupo)) 




to_test <- panel_predios_moscal %>% 
  dplyr::filter(grupo == "CON_PROYECTO", periodo == "2023_JUL"
                ) %>%
  left_join(data_encuesta_to)


to_pca <- to_test %>% drop_na() %>%
  dplyr::select(where(is.numeric), -lat, -lon) %>% distinct() %>%
  as.matrix()


pca_res <- prcomp(scale(to_pca), center = TRUE, scale. = TRUE)



k_best <- 4

km_final <- kmeans(scale(to_pca), centers = k_best, nstart = 200, iter.max = 100)
clusters <- tibble(unidad_geografica = rownames(to_pca), cluster = factor(km_final$cluster))
# plot(km_final$cluster)
# 
# table(km_final$cluster)


# Visual de clusters en espacio PCA
fviz_pca_biplot(pca_res, habillage = as.factor(clusters$cluster), axes = c(1, 2),
                label = "var", repel = TRUE, addEllipses = F) +
  labs(title = "PCA biplot con clusters Kmeans (k = 4)") 



# Tabla de perfiles

profile <- to_test %>% drop_na() %>%
  dplyr::select(where(is.numeric), -lat, -lon) %>% distinct() %>%
  mutate(cluster = clusters) %>%
  group_by(cluster) %>%
  summarise(n = n(),
            across(where(is.numeric), \(x) mean(x, na.rm = TRUE), .names = "mean_{.col}"))

data.table::data.table(profile)


factoextra::fviz_pca_var(pca_res, col.var = "contrib",
                         gradient.cols = c("white", "blue", "red"),
                         ggtheme = theme_minimal())

factoextra::fviz_contrib(pca_res, choice = "var", , axes = 1:5, top = 10)



ind_to_plot <- "Cb"   # cámbialo por cualquier indicador

moscal_indicadores_predios %>%
  dplyr::select(cu:Cb) %>% 
  mutate(year = as.numeric(str_sub(periodo, 1, 4))) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = Cb, color = grupo)
  ) +
  ggplot2::geom_jitter(width = 0.2, alpha = 0.3) +
#  ggplot2::geom_line(alpha = 0.5) +
  ggplot2::geom_smooth( aes(color = grupo),
    method = "lm", se = FALSE, linetype = "dashed"
  ) +
  ggplot2::facet_wrap(~asoc) +
  ggplot2::labs(
    title = paste("Tendencias anuales del indicador", ind_to_plot),
    x = "Año", y = "Valor", color  = "MoSCAL: "
  ) +
  ggplot2::theme_bw(base_size = 9) +
  ggplot2::theme(legend.position = "bottom")



moscal_indicadores_predios %>%
  dplyr::select(cu:Cb) %>% 
  mutate(year = as.numeric(str_sub(periodo, 1, 4))) %>%
  ggplot2::ggplot(
    ggplot2::aes(x = year, y = Cb)
  ) +
  #ggplot2::geom_jitter(alpha = 0.8) +
  #  ggplot2::geom_line(alpha = 0.5) +
  ggplot2::geom_smooth( aes(color = grupo),
                        method = "lm", se = FALSE, linetype = "dashed"
  ) +
  ggplot2::facet_wrap(asoc~.) +
  ggplot2::labs(
    title = paste("Tendencias anuales del indicador", ind_to_plot),
    x = "Año", y = "Valor", color  = "MoSCAL: "
  ) +
  ggplot2::theme_bw(base_size = 9) +
  ggplot2::theme(legend.position = "bottom")

