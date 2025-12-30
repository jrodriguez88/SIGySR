# Opcional: aumentar tiempo máximo de descarga (segundos)
options(timeout = 600)

# Años a descargar
years <- 1985:2023

# Carpeta de salida
out_dir <- "data/mapbiomas_colombia_cov3"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# URL base (solo cambia el año al final)
base_url <- "https://storage.googleapis.com/mapbiomas-public/initiatives/colombia/collection_3/coverage/colombia_coverage_"

for (yr in years) {
  url  <- paste0(base_url, yr, ".tif")
  dest <- file.path(out_dir, paste0("colombia_coverage_", yr, ".tif"))
  
  if (file.exists(dest)) {
    message("Saltando ", yr, " (archivo ya existe en disco).")
    next
  }
  
  message("Descargando ", yr, " desde: ", url)
  
  res <- try(
    download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
    silent = TRUE
  )
  
  if (inherits(res, "try-error") || !file.exists(dest)) {
    warning("No se pudo descargar el año ", yr, " desde: ", url)
  }
}

message("Proceso terminado.")


map_biomas <- rast(list.files(out_dir, full.names = T))




# --- Librerías ---
library(terra)
library(magick)
library(dplyr)

# --- Configuración ---
setwd("data/mapbiomas_colombia_cov3")  # Ajusta si tus TIFF están en otra ruta
out_gif <- "mapbiomas_colombia_1985_2024.gif"

# --- Leyenda (basada en el PDF de MapBiomas Colombia Colección 3.0) ---
leyenda <- data.frame(
  code = c(3, 4, 12, 15, 18, 19, 20, 21, 23, 24, 25, 33, 29, 30, 31, 32, 39, 40, 41, 46),
  color = c("#1f8d49", "#b6f199", "#45c2a5", "#b8af4f", "#f1c232",
            "#ffeb3b", "#ffd966", "#d6b656", "#f6b26b", "#e974ed",
            "#a64d79", "#8a2be2", "#2986cc", "#0b5394", "#76a5af",
            "#c27ba0", "#e6b8af", "#cccccc", "#999999", "#000000"),
  label = c("Bosque", "Formaciones Arbustivas", "Mosaico de agricultura y vegetación secundaria",
            "Pastos", "Agricultura", "Mosaico de agricultura y pastos",
            "Cultivos permanentes", "Plantaciones forestales", "Mosaico urbano/rural",
            "Infraestructura urbana", "Minas y otras áreas artificiales", "Agua",
            "Humedales naturales", "Manglares", "Otros cuerpos de agua",
            "Nieve o glaciar", "Arena o playa", "Roca expuesta", "Otros no vegetados", "Sin datos")
)

# --- Cargar y leer los rasters ---
years <- 1985:2024
r_list <- list()

for (yr in years) {
  f <- sprintf("colombia_coverage_%d.tif", yr)
  if (!file.exists(f)) next
  r <- rast(f)
  r_list[[as.character(yr)]] <- r
}

# --- Crear imágenes temporales ---
frames <- list()
for (yr in names(r_list)) {
  r <- r_list[[yr]]
  
  # Asignar paleta y plot temporal
  pngfile <- tempfile(fileext = ".png")
  png(pngfile, width = 1000, height = 1000, res = 100)
  plot(r, col = leyenda$color, breaks = c(leyenda$code - 0.5, max(leyenda$code) + 0.5),
       main = paste("MapBiomas Colombia - Cobertura", yr), legend = FALSE, axes = FALSE)
  dev.off()
  
  frames[[yr]] <- image_read(pngfile)
}

# --- Crear GIF ---
animation <- image_animate(image_join(frames), fps = 2)
image_write(animation, path = out_gif)

message("GIF generado: ", out_gif)




# --- Librerías ---
library(terra)
library(magick)

# --- Configuración ---
work_dir <- "data/mapbiomas_colombia_cov3"  # carpeta donde están los .tif
setwd(work_dir)

years   <- 1985:2024
out_gif <- file.path(work_dir, "mapbiomas_colombia_1985_2024_legend.gif")

# --- Leer leyenda desde CSV (a partir de tu PDF) ---
# # CSV con columnas: code, label, color
# leyenda <- read.csv("legend_mapbiomas_col3.csv",
#                     stringsAsFactors = FALSE)

# Asegura orden por código
leyenda <- leyenda[order(leyenda$code), ]

# --- Función para hacer un frame (PNG) por año ---
make_frame <- function(year, leyenda) {
  tif_file <- sprintf("colombia_coverage_%d.tif", year)
  if (!file.exists(tif_file)) {
    message("No existe: ", tif_file, " → se omite este año.")
    return(NULL)
  }
  
  r <- rast(tif_file)
  
  # archivo temporal PNG
  pngfile <- tempfile(fileext = ".png")
  png(pngfile, width = 1200, height = 900, res = 120)
  
  ## margen grande a la derecha para que quepa la leyenda
  par(mar = c(3, 3, 4, 9))
  
  plot(
    r,
    col    = leyenda$color,
    breaks = c(leyenda$code - 0.5, max(leyenda$code) + 0.5),
    axes   = FALSE,
    legend = FALSE,
    main   = paste0("MapBiomas Colombia - Cobertura y Uso del Suelo ", yr)
  )
  
  legend(
    "right",                 # mismo lado: right
    legend = leyenda$label,
    fill   = leyenda$color,
    cex    = 0.7,
    bg     = "white",
    ncol   = 1,              # <-- fuerza UNA sola columna
    xpd    = TRUE,
    inset  = 0.01
  )
  
  dev.off()
  
  image_read(pngfile)
}

# --- Generar todos los frames ---
frames <- list()

for (yr in years) {
  message("Generando frame para ", yr)
  im <- make_frame(yr, leyenda)
  if (!is.null(im)) {
    frames[[length(frames) + 1]] <- im
  }
}

# Si no hubo frames, salir
if (length(frames) == 0) {
  stop("No se generó ningún frame: revisa ruta y nombres de los .tif")
}

# --- Crear GIF ---
animation <- image_animate(image_join(frames), fps = 2)
image_write(animation, path = "Map_biomas_gif")

message("GIF generado en: ", out_gif)




