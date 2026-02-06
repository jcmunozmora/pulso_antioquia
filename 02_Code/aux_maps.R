# ==============================================================================#
# SCRIPT: aux_maps.R
# PROYECTO: Pulso Social - Las Antioquias
# DESCRIPCIÓN: Reemplaza geometrías del shapefile anterior con geometrías
#              de alta calidad, manteniendo todas las variables originales
# ==============================================================================#

# ==============================================================================#
# CONFIGURACIÓN INICIAL ----
# ==============================================================================#
rm(list = ls())

# ---- librerías ---- #
library(sf)
library(tidyverse)
library(stringr)

# ==============================================================================#
# CARGA DE SHAPEFILES ----
# ==============================================================================#

# ---- cargar shapefile NUEVO (mejor calidad topológica) ---- #
# Este shapefile ya viene filtrado solo para Antioquia
map_nuevo <- st_read("01_Data/00_Inputs/maps/MGN_MPIO_POLITICO.shp")

# ---- cargar shapefile ANTERIOR (tiene las variables que necesitamos) ---- #
map_anterior <- st_read("01_Data/00_Inputs/maps/mapa_municipios_colombia.shp")

# ==============================================================================#
# PREPARAR SHAPEFILE ANTERIOR ----
# ==============================================================================#

# ---- filtrar Antioquia y preparar variables en shapefile anterior ---- #
mpios_anterior_ant <- map_anterior |>
  mutate(
    # Estandarizar código a 5 dígitos para el join
    nivl_vl = str_pad(as.character(nivl_vl), width = 5, pad = "0")
  ) |>
  filter(substr(nivl_vl, 1, 2) == "05") |> # Filtrar solo Antioquia
  mutate( nvl_lbl_2 = str_sub(nvl_lbl, 1, 3))     


# ==============================================================================#
# UNIR GEOMETRÍAS NUEVAS CON VARIABLES ANTERIORES ----
# ==============================================================================#

# ---- extraer solo la geometría del nuevo shapefile ---- #
# Nos quedamos solo con código y geometría del nuevo
geom_nueva <- map_nuevo |>
  select(MPIO_CCDGO, geometry)

# ---- remover geometría del anterior y mantener todas sus variables ---- #
# Quitamos geometría para poder hacer join sin conflictos
vars_anteriores <- mpios_anterior_ant |>
  st_drop_geometry()

# ---- hacer join usando código de municipio ---- #
# Unimos las variables del anterior con la geometría del nuevo
mpios_ant <- geom_nueva |>
  left_join(
    vars_anteriores,
    by = c("MPIO_CCDGO" = "nivl_vl")  # La llave de unión
  )

# ---- remover columna del nuevo shapefile y mantener solo las del anterior ---- #
# Eliminamos MPIO_CCDGO porque ya tenemos nivl_vl del shapefile anterior
mpios_ant <- mpios_ant |>
  rename(nivl_vl = MPIO_CCDGO)

# ==============================================================================#
# EXPORTAR SHAPEFILE ----
# ==============================================================================#

# ---- guardar shapefile con geometrías nuevas y variables anteriores ---- #
st_write(
  mpios_ant,
  dsn = "01_Data/01_Derived/maps/municipios_antioquia.shp",
  delete_layer = TRUE  # Sobreescribe si ya existe
)



