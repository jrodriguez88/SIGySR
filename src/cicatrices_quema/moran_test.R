# library(terra)
# # SpatRaster con valores 0/1
# cq_pts  <- as.data.frame(cq,  xy=TRUE, na.rm=FALSE)
# def_pts <- as.data.frame(def, xy=TRUE, na.rm=FALSE)
# # Unimos ambos en un solo data.frame
# df <- merge(cq_pts, def_pts, by=c("x","y"), suffixes=c("_cq","_def"))
library(spdep)


## Estadísticas Globales 

# Global Moran’s I

cq_num <- as.numeric(cq)

# Calcular Moran's I global (3×3 matriz de pesos)
global_moran <- autocor(cq_num, 
                        w = matrix(c(1,1,1,
                                     1,0,1,
                                     1,1,1),3),
                        method="moran", global=TRUE)
print(global_moran)



# Join Count Statistic

# Factorizamos el vector binario
cq_factor <- factor(df$value_cq, levels=c(0,1))

# Test BB (1–1 joins)
jc_bb <- joincount.test(cq_factor, listw=lw, zero.policy=TRUE)
print(jc_bb)







library(terra)
library(spdep)
library(sf)

# 1. Agregar
fact <- 10
cq_agg  <- aggregate(
  ,  fact=fact, fun=mean, na.rm=TRUE)
def_agg <- aggregate(def, fact=fact, fun=mean, na.rm=TRUE)

# 2. Convertir agregados a puntos
pts_cq  <- as.data.frame(cq_agg,  xy=TRUE, na.rm=FALSE)
pts_def <- as.data.frame(def_agg, xy=TRUE, na.rm=FALSE)
df_agg  <- merge(pts_cq, pts_def, by=c("x","y"), suffixes=c("_cq","_def"))
names(df_agg)[3:4] <- c("cq","def")

# 3. Crear objeto sf
pts_sf  <- st_as_sf(df_agg, coords=c("x","y"), crs=crs(cq))

# 4. Vecindad basado en K vecinos (e.g., k=8)
coords <- st_coordinates(pts_sf)
knn_nb <- knearneigh(coords, k=8)
nb      <- knn2nb(knn_nb)
lw      <- nb2listw(nb, style="W", zero.policy=TRUE)

# 5. Local Moran’s I univariado para quemas
local_mi_cq <- localmoran(pts_sf$cq, lw, zero.policy=TRUE)
pts_sf$Ii_cq <- local_mi_cq[,1]   # estadístico Ii
pts_sf$Z_Ii  <- local_mi_cq[,"Z.Ii"]

# 6. Local Bivariate Moran’s I (cq vs def)
local_bv <- localmoran_bv(pts_sf$cq, pts_sf$def, lw, zero.policy=TRUE)
pts_sf$Ib <- local_bv[,1]         # estadístico bivariante

# Ahora 'pts_sf' contiene columnas Ii_cq, Z_Ii y Ib para cada punto agregado
