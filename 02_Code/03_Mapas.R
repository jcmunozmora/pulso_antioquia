# ==============================================================================#
# ANTIOQUIAS - 03_Mapas.R
# GENERACIÓN DE MAPA FINAL CONSOLIDADO
#
# OBJETIVO:
# Consolidar resultados de subclustering en un único mapa sintético que muestre
# la clasificación de 9 categorías (3 grupos territoriales × 3 niveles de desempeño)
# con overlays de provincias/zonas administrativas.
#
# INPUTS:
#   - 04_Cluster_Outputs/Sub_Consolidada/datos_map.csv
#   - 04_Cluster_Outputs/Sub_Transicion/datos_map.csv
#   - 04_Cluster_Outputs/Sub_Vulnerable/datos_map.csv
#   - 01_Data/01_Derived/maps/municipios_antioquia.shp
#   - 01_Data/00_Inputs/maps/EAT_DAP_26082025/EAT_PROPUESTA_26082025.shp
#
# OUTPUTS:
#   - final_map.pdf → Mapa de 9 categorías sin overlays
#   - final_map_prov.pdf → Mapa con límites de provincias/zonas
#
# ETAPA DEL PIPELINE: 4 (Visualización espacial)
# ==============================================================================#

# ---- setup ----
rm(list = ls())
source("02_Code/AUX_Functions_Clusters.R")
set.seed(1601)

# ---- packages ----
pacman::p_load(
  readxl,
  dplyr,
  purrr,
  tibble,
  stringr,
  readr,
  sf,
  ggplot2
)

# ==============================================================================#
# PASO 1: Lectura de datos espaciales y de clustering ----
# ==============================================================================#

# ---- shapefiles ----
map_mpios <- st_read("01_Data/01_Derived/maps/municipios_antioquia.shp")
map_prov <- st_read("01_Data/00_Inputs/maps/EAT_DAP_26082025/EAT_PROPUESTA_26082025.shp")

# ---- limpiar topología de municipios ----
# Corregir geometrías inválidas y solapamientos
map_mpios <- st_make_valid(map_mpios)

# Simplificar preservando topología (elimina vértices redundantes)
map_mpios <- st_simplify(map_mpios, preserveTopology = TRUE, dTolerance = 0.0001)

# ---- resultados de subclustering ----
# Cada archivo contiene la asignación de 3 niveles (Alto/Medio/Bajo) dentro de su grupo
df_consolidado <- read_csv("04_Cluster_Outputs/Sub_Consolidada/datos_map.csv")
df_transicion <- read_csv("04_Cluster_Outputs/Sub_Transicion/datos_map.csv")
df_vulnerable <- read_csv("04_Cluster_Outputs/Sub_Vulnerable/datos_map.csv")

# ==============================================================================#
# PASO 2: Preparación y combinación de datasets ----
# ==============================================================================#

# ---- seleccionar columnas clave ----
df_consolidado <- df_consolidado[c("nivl_vl", "sub_grp")]
df_transicion <- df_transicion[c("nivl_vl", "sub_grp")]
df_vulnerable <- df_vulnerable[c("nivl_vl", "sub_grp")]

# ---- agregar identificador de grupo territorial ----
df_consolidado$cat <- "Consolidada"
df_transicion$cat <- "Transición"
df_vulnerable$cat <- "Vulnerable"

# ---- filtrar municipios sin asignación ----
df_consolidado <- df_consolidado %>% filter(!is.na(sub_grp))
df_transicion <- df_transicion %>% filter(!is.na(sub_grp))
df_vulnerable <- df_vulnerable %>% filter(!is.na(sub_grp))

# ---- consolidar en un solo dataset ----
data <- bind_rows(
  list(Consolidada = df_consolidado, "Transición" = df_transicion, Vulnerable = df_vulnerable),
  .id = "grupo_org"
)

# ==============================================================================#
# PASO 3: Generación de IDs únicos para 9 categorías ----
# ==============================================================================#
# Lógica: 3 grupos territoriales × 3 niveles = 9 combinaciones únicas
# Cada combinación (sub_grp, cat) recibe un ID único (combo_id)

data2 <- data %>%                              
  group_by(sub_grp, cat) %>%                   # Agrupar por nivel y categoría territorial
  arrange(nivl_vl, .by_group = TRUE) %>%       # Ordenar municipios dentro del grupo
  mutate(
    combo_id     = cur_group_id(),             # ID único de la combinación (1-9)
    id_en_combo  = row_number()                # Posición del municipio en su combo
  ) %>%
  ungroup()


# ---- unir con geometría espacial ----
data_map <- merge(
  map_mpios, data2,
  by.x = "nivl_vl", by.y = "nivl_vl",
  all.x = TRUE,      # Mantener todos los municipios del shapefile
  sort = FALSE
)

# ==============================================================================#
# PASO 4: Configuración de visualización ----
# ==============================================================================#

# ---- tema para mapas ----
map_theme <- list(
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = rel(1)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = -0.2, colour = "black"),
    axis.text = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ))

# ---- paleta de colores (9 tonos - DEBE COINCIDIR con AUX_Functions_Clusters.R) ----
# Paleta extendida para subclusters (3 grupos × 3 niveles = 9 combinaciones)
col_palette <- c(
  "#88B08D", "#9EBA88", "#B3C383",  # Consolidada: verde - Alto/Medio/Bajo
  "#C8CE7D", "#DDD878", "#D9B76B",  # Transición: amarillo - Alto/Medio/Bajo
  "#D6555F", "#D27452", "#CE5244"   # Vulnerable: rojo - Alto/Medio/Bajo
)

# ---- mapeo de combo_id a etiquetas descriptivas ----
# Reordenamiento: 1,3,2 (Consolidada), 4,6,5 (Transición), 7,9,8 (Vulnerable)
# IMPORTANTE: Etiquetas deben coincidir con AUX_Functions_Clusters.R (Alto/Medio/Bajo)
data_map$sub_grp <- factor(
  data_map$combo_id,
  levels = c(1, 3, 2, 4, 6, 5, 7, 9, 8),
  labels = c(
    "Consolidada - Alto", "Consolidada - Medio", "Consolidada - Bajo",
    "Transición - Alto", "Transición - Medio", "Transición - Bajo",
    "Vulnerable - Alto", "Vulnerable - Medio", "Vulnerable - Bajo"
  )
)

# ==============================================================================#
# PASO 5: Generación de mapa final ----
# ==============================================================================#

# ---- mapa base con 9 categorías ----
g1 <- ggplot(data_map) +
  geom_sf(aes(fill = factor(sub_grp)), color = "#505050", linewidth = 0.4, alpha = 0.95) +
  geom_text(aes(X, Y, label = nvl_lbl), 
            vjust = 1.5, 
            color = "black",
            position = position_dodge(0.9), 
            size = 2, 
            alpha = 1) +
  scale_fill_manual(values = rep(col_palette, 40), 
                    na.value = "#ededed", 
                    na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 1)) +
  xlab("") +
  labs(fill = "") +
  map_theme +
  theme_map(base_size = 12) +
  theme(legend.position = c(0.8, 0.1),
        plot.caption = element_text(color = "blue", face = "italic", size = 5))

g1

# ---- exportar mapa sin overlays ----
ggsave(
  file = "final_map.pdf", 
  plot = g1,
  height = h, 
  width = w * 0.8, 
  dpi = d
)

# ==============================================================================#
# PASO 6: Mapa con overlays de provincias/zonas ----
# ==============================================================================#

# ---- preparar shapefile de provincias ----
map_prov <- st_make_valid(map_prov)                   # Corregir geometrías inválidas
map_prov <- st_transform(map_prov, st_crs(map_mpios)) # Alinear CRS con mapa de municipios

prov <- "ZONA"  # Campo que identifica las zonas/provincias

# ---- disolver límites internos por zona ----
redes_diss <- map_prov %>%
  filter(!is.na(.data[[prov]])) %>%
  group_by(grupo_red = .data[[prov]]) %>%
  summarise(.groups = "drop")

# ---- superponer límites de provincias ----
g2 <- g1 +
  geom_sf(
    data = redes_diss, 
    fill = NA, 
    color = "#838B8B", 
    linewidth = 0.6,  # Grosor de líneas de provincias
    inherit.aes = FALSE
  )

# ---- exportar mapa con overlays ----
ggsave(
  file = "final_map_prov.pdf", 
  plot = g2,
  height = h, 
  width = w * 0.8, 
  dpi = d
)
