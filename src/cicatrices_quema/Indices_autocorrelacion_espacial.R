library(sf)
library(dplyr)
library(terra)



# 1.1. Lee tus capas
cic <- cicatrices_sp[[1]]        # sf MULTIPOLYGON con id_ciq
cov <- coberturas_sp[[1]]                # sf POLYGON con code_num

# 1.2. Asegura mismo CRS
cov <- st_transform(cov, st_crs(cic))

# 1.3. Intersección espacial: asigna scars a cobertura
int <- st_intersection(
  st_make_valid(cic),            # en caso de geometrías inválidas
  st_make_valid(cov)
)

int <- st_intersection(
  cic,            # en caso de geometrías inválidas
  cov
)

# 1.4. Cuenta cicatrices por polígono de cobertura
cov_stats <- int %>%
  st_set_geometry(NULL) %>%      # descarta geometría para agrupar
  group_by(code_num) %>%
  summarize(n_scars = n(),       # número de polígonos de cicatriz
            area_scars = sum(st_area(.)), .groups="drop")

# 1.5. Une de vuelta a 'cov'
cov <- cov %>%
  left_join(cov_stats, by="code_num") %>%
  mutate(
    n_scars     = replace_na(n_scars, 0),
    area_scars  = replace_na(area_scars, 0)
  )
