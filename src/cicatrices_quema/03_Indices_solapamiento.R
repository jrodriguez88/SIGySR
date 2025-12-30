## Analisis Areas quemadas - coberturas


adm <- departamentos_sinchi           # lecturas vectoriales :contentReference[oaicite:4]{index=4}
adm_proj <- st_transform(adm, crs(cq))  
adm_vec <- vect(adm_proj)                           # de sf a SpatVector (terra) :contentReference[oaicite:6]{index=6}

## Rasteriza ADM a partir de adm_vec
adm_rast <- rasterize(adm_vec, cq, field = "departamen")

# 1. Cargar raster prueba

# Lista
cq_raster_list <- list.files("output/raster/", pattern = "id_ciq.tif", full.names = T) %>%
  map(rast)

coberturas_raster_list <- list.files("output/raster/", pattern = "cobertura_code", full.names = T) %>%
  map(rast)

# Raster binario
cq_raster_bin_list <- cq_raster_list %>% map(~.x >0) %>% setNames(cq_names)

c(
BE = "Bosque Estable",
NI = "Sin Información",
NB = "No Bosque Estable",
DF = "Deforestación",
RG = "Regeneración") %>% names %>% as.factor() %>% as.numeric()

deforest_raster_list <- coberturas_raster_list %>% map(~.x == 1) %>% setNames(cq_names)

cq_raster_bin <- rast("output/cic_quema_rbin_crs9377.tif")
deforest_raster <- rast("output/deforest_rbin_crs9377.tif")

r <- coberturas_raster_list[[1]]               # tu SpatRaster
r_fac <- as.factor(r)                          # convierte a factor
terra::levels(r_fac)[[1]]   


plot(cq_raster_bin)
plot(deforest_raster)
plot(coberturas_raster)


# 2. Asegurarse de que ambos sean lógicos (TRUE/FALSE o 1/0)
cq  <- as.numeric(cq_raster_bin_list[[2]])    # convierte TRUE/FALSE a 1/0
def <- as.numeric(deforest_raster_list[[2]])

# 3. Overlay: intersección y unión
intersect_raster <- (cq == 1 & def == 1)
union_raster     <- (cq == 1 | def == 1)

# 4. Contar celdas verdaderas
n_intersect <- global(intersect_raster, "sum", na.rm=TRUE)[1]
n_union     <- global(union_raster,     "sum", na.rm=TRUE)[1]
n_cq        <- global(cq,              "sum", na.rm=TRUE)[1]
n_def       <- global(def,             "sum", na.rm=TRUE)[1]

# 5. Índice de Jaccard
#    J = |A ∩ B| / |A ∪ B|
jaccard <- n_intersect / n_union

# 6. Índice de Sørensen–Dice
#    D = 2 |A ∩ B| / (|A| + |B|)
sorensen_dice <- 2 * n_intersect / (n_cq + n_def)

# 7. Mostrar resultados
cat("Número de celdas (intersección):", n_intersect[[1]], "\n")
cat("Número de celdas (unión):",     n_union[[1]],     "\n\n")
cat("Índice de Jaccard:     ", round(jaccard[[1]], 4), "\n")
cat("Índice Sørensen–Dice:  ", round(sorensen_dice[[1]], 4), "\n")


## Solapamiento por departamento



# 2.1 Conteo de quemas por departamento
n_cq <- zonal(cq, adm_rast, fun="sum", na.rm=TRUE)  
# devuelve data.frame con columnas "ID_departamento" y "sum" :contentReference[oaicite:3]{index=3}
names(n_cq) <- c("ID_departamento","n_cq")

# 2.2 Conteo de deforestación por departamento
n_def <- zonal(def,      adm_rast, fun="sum", na.rm=TRUE)
names(n_def) <- c("ID_departamento","n_def")

# 2.3 Conteo de intersección (cq & def)
int_rast <- (cq == 1 & def == 1)
n_int  <- zonal(int_rast, adm_rast, fun="sum", na.rm=TRUE)
names(n_int) <- c("ID_departamento","n_int")

# 2.4 Conteo de unión (cq | def)
union_rast <- ((cq + def) > 0)
n_union    <- zonal(union_rast, adm_rast, fun="sum", na.rm=TRUE)
names(n_union) <- c("ID_departamento","n_union")

# 3.1 Unir tablas por el ID de departamento
stats_raw <- Reduce(function(x,y) merge(x,y, by="ID_departamento"),
                    list(n_cq, n_def, n_int, n_union))

# 3.2 Calcular índice de Jaccard y Sørensen–Dice
stats_raw$jaccard <- with(stats_raw, n_int / n_union)  
stats_raw$dice     <- with(stats_raw, 2 * n_int / (n_cq + n_def))

# 3.3 Formatear resultados
stats_indice <- stats_raw[, c("ID_departamento","jaccard","dice")]



stats_raw %>% mutate(area_cq = n_cq * (30 * 30) / 10000)


