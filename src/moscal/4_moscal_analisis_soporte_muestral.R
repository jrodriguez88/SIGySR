moscal_variables_predios %>% group_by(asoc, grupo, periodo) %>%
  summarise(n = n()) %>% ungroup() %>%
  pivot_wider(names_from = periodo, values_from = n)



library(dplyr)
library(tidyr)

# A) Soporte a nivel predio (deduplicado por cu)
support_predio <- moscal_variables_predios %>%
  distinct(cu, asoc, grupo, area_uer_ha) %>%
  group_by(asoc, grupo) %>%
  summarise(
    n_predios       = n(),
    area_total_ha   = sum(area_uer_ha, na.rm = TRUE),
    area_mediana_ha = median(area_uer_ha, na.rm = TRUE),
    area_p25_ha     = quantile(area_uer_ha, 0.25, na.rm = TRUE),
    area_p75_ha     = quantile(area_uer_ha, 0.75, na.rm = TRUE),
    area_max_ha     = max(area_uer_ha, na.rm = TRUE),
    .groups = "drop"
  )

# B) Soporte a nivel panel (obs/periodos)
support_panel <- moscal_variables_predios %>%
  group_by(asoc, grupo) %>%
  summarise(
    n_obs      = n(),
    n_periodos = n_distinct(periodo),
    .groups = "drop"
  )

# C) Tabla final para anexar al informe
tabla_soporte_asoc <- support_predio %>%
  left_join(support_panel, by = c("asoc", "grupo")) %>%
  arrange(asoc, grupo)

tabla_soporte_asoc


tabla_soporte_periodo <- moscal_variables_predios %>%
  group_by(asoc, grupo, periodo) %>%
  summarise(
    n_obs    = n(),
    n_predios = n_distinct(cu),
    .groups = "drop"
  ) %>%
  arrange(asoc, periodo, grupo)

tabla_soporte_periodo




# Línea base por asoc y grupo
baseline_asoc <- moscal_variables_predios %>%
  filter(periodo == "2017_JUL") %>%
  mutate(pO_mod = pO + pQ) %>%
  group_by(asoc, grupo) %>%
  summarise(
    n_predios = n_distinct(cu),
    area_med_ha = median(area_uer_ha, na.rm = TRUE),
    pB_med = median(pB, na.rm = TRUE),
    pP_med = median(pP, na.rm = TRUE),
    pV_med = median(pV, na.rm = TRUE),
    pO_med = median(pO_mod, na.rm = TRUE),
    .groups = "drop"
  )

baseline_asoc

# Diferencia CON - SIN por asociación (para una tabla rápida)
baseline_diff_asoc <- baseline_asoc %>%
  pivot_wider(names_from = grupo,
              values_from = c(n_predios, area_med_ha, pB_med, pP_med, pV_med, pO_med)) %>%
  mutate(
    d_area_med_ha = area_med_ha_CON_PROYECTO - area_med_ha_SIN_PROYECTO,
    d_pB = pB_med_CON_PROYECTO - pB_med_SIN_PROYECTO,
    d_pP = pP_med_CON_PROYECTO - pP_med_SIN_PROYECTO,
    d_pV = pV_med_CON_PROYECTO - pV_med_SIN_PROYECTO,
    d_pO = pO_med_CON_PROYECTO - pO_med_SIN_PROYECTO
  ) %>%
  select(asoc, starts_with("n_predios_"),
         d_area_med_ha, d_pB, d_pP, d_pV, d_pO)

baseline_diff_asoc


baseline_global <- moscal_variables_predios %>%
  filter(periodo == "2017_JUL") %>%
  mutate(pO_mod = pO + pQ) %>%
  group_by(grupo) %>%
  summarise(
    n_predios = n_distinct(cu),
    area_med_ha = median(area_uer_ha, na.rm = TRUE),
    pB_med = median(pB, na.rm = TRUE),
    pP_med = median(pP, na.rm = TRUE),
    pV_med = median(pV, na.rm = TRUE),
    pO_med = median(pO_mod, na.rm = TRUE),
    .groups = "drop"
  )

baseline_global


# Top asociaciones por magnitud (mediana) del DiD agregado
top_did <- did_2018_2024 %>%
  group_by(asoc, Componente) %>%
  summarise(
    did_med = median(DiD_2018_2024, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(abs_did = abs(did_med)) %>%
  arrange(desc(abs_did)) %>%
  slice_head(n = 12)

# Añadir soporte (predios y área total por grupo) para contextualizar
top_did_con_soporte <- top_did %>%
  left_join(
    tabla_soporte_asoc %>% select(asoc, grupo, n_predios, area_total_ha),
    by = "asoc"
  ) %>%
  arrange(desc(abs_did), Componente)

top_did_con_soporte

soporte_wide <- tabla_soporte_asoc %>%
  select(asoc, grupo, n_predios, area_total_ha) %>%
  pivot_wider(names_from = grupo, values_from = c(n_predios, area_total_ha))

top_did_tabla <- top_did %>%
  left_join(soporte_wide, by = "asoc")

top_did_tabla


# t0 por predio tratado (asumiendo grupo fijo por cu)
t0_tratados <- moscal_variables_predios %>%
  filter(grupo == "CON_PROYECTO") %>%
  group_by(cu, asoc) %>%
  summarise(t0 = min(levels(periodo)), .groups = "drop")

# Distribución de t0
dist_t0 <- t0_tratados %>%
  count(t0, sort = TRUE)

dist_t0



baseline_global %>% print(n = 50)
vis_miss(panel_raw, warn_large_data = FALSE, facet = grupo)

tabla_soporte_asoc %>% print(n = 50)
baseline_asoc %>% print(n = 50)
baseline_diff_asoc %>% print(n = 50)
top_did_con_soporte %>% print(n = 50)
top_did_tabla %>% print(n = 50)


write.csv(tabla_soporte_asoc,   "tabla_soporte_asoc.csv", row.names = FALSE)
write.csv(baseline_diff_asoc,   "baseline_diff_asoc_2017JUL.csv", row.names = FALSE)
write.csv(top_did_tabla,        "top_asoc_did_con_soporte.csv", row.names = FALSE)
write.csv(dist_t0,              "dist_t0_tratados.csv", row.names = FALSE)



