# ==============================================================================#
# SCRIPT: aux_maps.R
# PROYECTO: Pulso Social - Las Antioquias
# DESCRIPCIÓN: Genera shapefile de municipios de Antioquia con correcciones
#              topológicas para eliminar gaps entre límites municipales
# ==============================================================================#

# ==============================================================================#
# CONFIGURACIÓN INICIAL ----
# ==============================================================================#
rm(list = ls())

# ---- librerías ---- #
library(sf)
library(tidyverse)
library(stringr)
library(lwgeom)

# ==============================================================================#
# CARGA Y PREPARACIÓN DE DATOS ----
# ==============================================================================#
# ---- cargar shapefiles de Colombia ---- #
map_mpios <- st_read("01_Data/00_Inputs/maps/mapa_municipios_colombia.shp")

# ---- filtrar municipios de Antioquia ---- #
mpios_ant <- map_mpios |>
  mutate(
    code5   = str_pad(as.character(nivl_vl), width = 5, pad = "0"),
    depcode = substr(code5, 1, 2)
  ) |>
  filter(depcode == "05")

# ---- agregar variables derivadas ---- #
mpios_ant <- mpios_ant |>
  mutate(
    nivl_vl_num = as.integer(nivl_vl),
    nivl_vl     = str_pad(as.character(nivl_vl), 5, pad = "0"),
    nvl_lbl_2   = str_sub(nvl_lbl, 1, 3)
  )

# ==============================================================================#
# CORRECCIONES TOPOLÓGICAS ----
# ==============================================================================#
# ---- aplicar buffer + snap para cerrar gaps entre municipios ---- #
crs_original <- st_crs(mpios_ant)

mpios_ant <- mpios_ant |>
  st_transform(3857) |>           # CRS proyectado Web Mercator (metros)
  st_make_valid() |>              # Validar geometrías
  st_buffer(500) |>               # Buffer 300m para cerrar gaps
  st_make_valid() |>              # Re-validar después del buffer
  lwgeom::st_snap_to_grid(1) |>   # Snap a grilla 1m para regularizar vértices
  st_transform(crs_original)      # Volver a CRS original

# ==============================================================================#
# EXPORTAR SHAPEFILE ----
# ==============================================================================#
# ---- guardar shapefile corregido ---- #
st_write(
  mpios_ant,
  dsn = "01_Data/01_Derived/maps/municipios_antioquia.shp",
  delete_layer = TRUE
)


