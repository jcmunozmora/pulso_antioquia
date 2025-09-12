# Aux Code - Maps
rm(list = ls())

# Librerias
library(fs)
library(sf)
library(dplyr)
library(ggplot2)
library(dplyr)
library(stringr)

map_mpios <- st_read("01_Data/00_Inputs/maps/mapa_municipios_colombia.shp")
map_depto <- st_read("01_Data/00_Inputs/maps/mapa_departamentos_colombia.shp")
# prueba
ggplot(map_depto) +
  geom_sf(fill = "grey90", color = "white", linewidth = 0.1) +
  theme_void()
# Antioquia
mpios_ant <- map_mpios %>%
  mutate(code5   = str_pad(as.character(nivl_vl), width = 5, pad = "0"),
         depcode = substr(code5, 1, 2)) %>%
  filter(depcode == "05")
# prueba 
ggplot(mpios_ant) +
  geom_sf(fill = "grey90", color = "white", linewidth = 0.1) +
  theme_void()

# mpios con 0
mpios_ant <-mpios_ant %>%
  mutate(
    nivl_vl_num = as.integer(nivl_vl),                               
    nivl_vl     = str_pad(as.character(nivl_vl), 5, pad = "0")       
  )

mpios_ant <- mpios_ant %>%
  mutate( nvl_lbl_2 = str_sub(nvl_lbl, 1, 3))                  

# guardar 
st_write(mpios_ant,
         dsn = file.path("01_Data/01_Derived/maps/municipios_antioquia.shp"),
         delete_layer = TRUE)

