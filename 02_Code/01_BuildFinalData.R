rm(list = ls())
# Reshape data
#install.packages("haven")
# Packages
library(haven)
library(tidyr)
library(dplyr)
library(stringi)
library(writexl)
library(readxl)
# LOAD DATA
datos <- read_dta("01_Data/01_Derived/01_final_data.dta")
excel_sheets("01_Data/00_Inputs/INVENTARIO_VARIABLES.xlsx")
labels <- read_excel("01_Data/00_Inputs/INVENTARIO_VARIABLES.xlsx", sheet = "Diccionario_Base") 
# Reshape
datos_long <- datos %>%
  pivot_longer(
    cols = -c(ind_mpio, nvl_label),  
    names_to = "var0",                
    values_to = "valor"             
  )
# Cleaning data 
datos_long$nvl_label <- datos_long$nvl_label |>
  stri_trans_general(id = "Latin-ASCII") |>  
  gsub(" ", "_", x = _) |>                   
  tolower()                                  
datos_long$var0 <- datos_long$var0 |>
  stri_trans_general(id = "Latin-ASCII") |>
  gsub(" ", "_", x = _) |>
  tolower()
# Var0 To Numeric
datos_long$valor <- as.numeric(datos_long$valor)
datos_long <- datos_long[!grepl("^time_", datos_long$var0),]
write_xlsx(datos_long, "01_Data/01_Derived/01_final_data_long.xlsx")

# Merege
labels <- labels %>%
  rename( var0 = var_id  )  
datos_long2 <- left_join(datos_long, labels, by = "var0")

# Rev Acueducto Urbano y Acueducto Rural
datos_long2 <- datos_long2 %>% select(-`Unidad de medida_descriptiva`)
write_xlsx(datos_long2, "01_Data/01_Derived/01_final_data_long.xlsx")

# Creamos los datos iniciales
ds_ind <- read_excel("01_Data/01_Derived/01_final_data_long.xlsx")

ds_ind <- ds_ind %>%
  rename(
    value = valor)

ds_ind <- ds_ind %>%
  mutate(var_id = as.integer(factor(var0)))

#
label <- ds_ind %>%
  select(var0, variable) %>%
  distinct(var0, .keep_all = TRUE) %>%
  rename(terms = var0,
         lab = variable)
ds_ind3 <- ds_ind %>%
  select(ind_mpio, nvl_label, var0, value, var_id, Subdimensión, Sentido)%>%
  rename(Subdimension = Subdimensión)
ds_ind2 <- ds_ind %>%
  select(ind_mpio, nvl_label, var0, value, var_id, Subdimensión)%>%
  rename(Subdimension = Subdimensión)
ds_ind <- ds_ind %>%
  mutate(nvl_label = toupper(nvl_label)) %>%
  select(ind_mpio, nvl_label, var0, value, var_id)

write_xlsx(ds_ind, "01_Data/01_Derived/01_DATOS_INICIALES.xlsx")
write_xlsx(ds_ind2,"01_Data/01_Derived/01_DATOS_INICIALES_dim.xlsx")
write_xlsx(ds_ind3,"01_Data/01_Derived/01_DATOS_INICIALES_dim_signo.xlsx")
write_xlsx(label,"01_Data/01_Derived/label.xlsx")




