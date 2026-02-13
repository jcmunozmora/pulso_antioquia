# ==============================================================================#
# ANTIOQUIAS - 02_ClusterAnalisys.R
# ANÁLISIS DE CLUSTERING TERRITORIAL JERÁRQUICO
#
# OBJETIVO:
# Realizar clustering de municipios de Antioquia en dos niveles:
# 1. Clúster global: clasificación en 3 grupos (Consolidada, Transición, Vulnerable)
# 2. Subclústers: análisis dentro de cada grupo con dos tipos de referencia
#
# INPUTS:  01_Data/01_Derived/pca_ds_Total.rds (31 componentes principales)
#          02_Code/AUX_Functions_Clusters.R (funciones auxiliares)
# OUTPUTS: 03_Outputs/all/04_Cluster/ds_cluster.rds (clúster base)
#          03_Outputs/{Grupo}_ref_subset/04_Cluster/ (referencia interna)
#          03_Outputs/{Grupo}_ref_dept/04_Cluster/ (referencia departamental)
#
# METODOLOGÍA:
# Doble estandarización permite dos tipos de comparación:
# - ref="dept": desviaciones respecto a media de Antioquia (comparación global)
# - ref="subset": desviaciones respecto a media del grupo (análisis intra-grupo)
#
# ETAPA DEL PIPELINE: 3 - Clustering territorial
# ==============================================================================#

# ---- packages ----
rm(list = ls())

# Semilla para replicabilidad de algoritmos de clustering
set.seed(1601)

pacman::p_load(
  tidyverse,
  readr,
  tidyr,
  dplyr,
  sf,
  ggrepel
)

# ---- paths ----
path_in <- "01_Data/01_Derived"
path_code <- "02_Code"

# ---- funciones auxiliares ----
source(file.path(path_code, "AUX_Functions_Clusters.R"))

# ==============================================================================#
# FUNCIÓN LOCAL: standardize_df
# ==============================================================================#
# Estandariza un subconjunto de datos usando diferentes referencias y escalas.
# Esta función permite comparaciones flexibles tanto dentro de grupos como
# respecto al departamento completo.
#
# PARÁMETROS:
#   df_subset_raw - Data frame del subgrupo a estandarizar (solo variables numéricas)
#   df_dept_raw   - Data frame del departamento completo (referencia global)
#   ref           - Centro de estandarización: "subset" (media del grupo) o "dept" (media departamental)
#   scale_sd      - Escala: "dept" (desv. est. departamental) o "subset" (desv. est. del grupo)
#
# RETORNA:
#   Data frame estandarizado: (valor - centro) / desviación_estándar
#
# INTERPRETACIÓN:
#   - ref="dept", scale_sd="dept": Desviaciones estándar respecto a Antioquia (comparación global)
#   - ref="subset", scale_sd="subset": Desviaciones estándar respecto al grupo (análisis intra-grupo)
# ==============================================================================#
# ---- standardize_df ----
standardize_df <- function(df_subset_raw, df_dept_raw,
                           ref = c("subset", "dept"),
                           scale_sd = c("dept", "subset")) {
  
  ref <- match.arg(ref)
  scale_sd <- match.arg(scale_sd)
  
  # Calcular desviación estándar para escalar
  sds <- if (scale_sd == "dept") {
    apply(df_dept_raw, 2, sd, na.rm = TRUE)
  } else {
    apply(df_subset_raw, 2, sd, na.rm = TRUE)
  }
  
  # Calcular centro (media de referencia)
  center <- switch(
    ref,
    subset = colMeans(df_subset_raw, na.rm = TRUE),
    dept   = colMeans(df_dept_raw, na.rm = TRUE)
  )
  
  # Estandarizar: (x - centro) / sd
  out <- sweep(df_subset_raw, 2, center, "-")
  out <- sweep(out, 2, sds, "/")
  
  as.data.frame(out)
}

# ==============================================================================#
# LECTURA DE DATOS Y SELECCIÓN DE VARIABLES
# ==============================================================================#
# ---- lectura de datos ----
pca_ds <- read_rds(file.path(path_in, "pca_ds_Total.rds"))

# ---- selección de componentes principales ----
# 31 componentes de los 15 índices PCA (algunos índices tienen múltiples componentes)
vars <- c(
  "idx1_servicio1",
  "idx2_Adultez1", "idx2_Adultez2", "idx2_Adultez3",
  "idx3_climatico1", "idx3_climatico2",
  "idx4_Capacidad1", "idx4_Capacidad2",
  "idx5_viviendas1", "idx5_viviendas2",
  "idx6_crecimiento1", "idx6_crecimiento2", "idx6_crecimiento3",
  "idx7_gini1",
  "idx8_demografica1",
  "idx9_infnin1", "idx9_infnin2", "idx9_infnin3", "idx9_infnin4",
  "idx10_juventud1", "idx10_juventud2", "idx10_juventud3", "idx10_juventud4",
  "idx11_pobreza1", "idx11_pobreza2",
  "idx12_salud1", "idx12_salud2",
  "idx13_saludmental1", "idx13_saludmental2",
  "idx14_seguridad1", "idx14_seguridad2",
  "idx15_vejez1"
)

# ==============================================================================#
# CLUSTERING GLOBAL (NIVEL 1)
# ==============================================================================#
# ---- preparación datos clustering global ----
# Seleccionar variables y eliminar casos con valores faltantes
pca_ds_all <- pca_ds %>% 
  dplyr::select(ind_mpio, nvl_label, dplyr::all_of(vars)) %>% 
  tidyr::drop_na()

# Dataset solo con variables numéricas para cálculos
df_dept_raw <- pca_ds_all %>% 
  dplyr::select(dplyr::all_of(vars))

# ---- clustering con referencia departamental ----
# Estandarizar usando media y desviación estándar de Antioquia
df_all_dept <- standardize_df(
  df_dept_raw, 
  df_dept_raw, 
  ref = "dept", 
  scale_sd = "dept"
)

# Asignar nombres de municipios como rownames para identificación
rownames(df_all_dept) <- pca_ds_all$nvl_label

# Ejecutar clustering y generar outputs (función de AUX_Functions_Clusters.R)
ps_cluster(
  pca_ds_all, 
  df_all_dept, 
  c("Consolidada", "Transición", "Vulnerable"), 
  "all", 
  "Desviaciones de la Media Departamental (std. desv.)"
)

# ==============================================================================#
# SUBCLUSTERING (NIVEL 2)
# ==============================================================================#
# ---- lectura de clústeres base ----
# Cargar resultado del clustering global
base_clusters <- read_rds("03_Outputs/all/04_Cluster/ds_cluster.rds") %>%
  dplyr::mutate(ind_mpio = nivl_vl) 

# Dataset departamental completo para referencia
dept_ref <- base_clusters %>% 
  tidyr::drop_na() %>% 
  dplyr::select(dplyr::all_of(vars))

# ---- análisis por cada grupo ----
# Iterar sobre los 3 grupos: Consolidada, Transición, Vulnerable
for (i in c("Consolidada", "Transición", "Vulnerable")) {
  
  # Filtrar municipios del grupo específico
  pca_sub <- base_clusters %>%
    dplyr::filter(sub_grp == i) %>%
    tidyr::drop_na()
  
  # Extraer solo variables numéricas del subgrupo
  df_subset_raw <- pca_sub %>% 
    dplyr::select(dplyr::all_of(vars))
  
  # Etiquetas para los 3 sub-clústeres dentro del grupo
  lab_g <- c(
    paste0(i, " - Alto"), 
    paste0(i, " - Medio"), 
    paste0(i, " - Bajo")
  )
  
  # ---- opción 1: referencia intra-grupo ----
  # Compara cada municipio con la media de su grupo (Consolidada, Transición o Vulnerable)
  df_sub_subset <- standardize_df(
    df_subset_raw, 
    dept_ref, 
    ref = "subset", 
    scale_sd = "subset"
  )
  rownames(df_sub_subset) <- pca_sub$nvl_label
  
  ps_cluster(
    pca_sub, 
    df_sub_subset, 
    lab_g, 
    paste0(i, "_ref_subset"), 
    paste0("Desviaciones de la Media de Antioquia ", i, " (std. desv.)")
  )
  
  # ---- opción 2: referencia departamental ----
  # Compara cada municipio con la media de TODO Antioquia
  df_sub_dept <- standardize_df(
    df_subset_raw, 
    dept_ref, 
    ref = "dept", 
    scale_sd = "dept"
  )
  rownames(df_sub_dept) <- pca_sub$nvl_label
  
  ps_cluster(
    pca_sub, 
    df_sub_dept, 
    lab_g, 
    paste0(i, "_ref_dept"), 
    "Desviaciones de la Media Departamental (std. desv.)"
  )
}

# ==============================================================================#
# MAPA INTEGRADO DE TODOS LOS SUBCLUSTERS
# ==============================================================================#
# ---- lectura de resultados de subclustering ----
# Leer los resultados generados en el paso anterior (referencia interna)
df_consolidada <- read_rds("03_Outputs/Consolidada_ref_subset/04_Cluster/ds_cluster.rds")
df_transicion <- read_rds("03_Outputs/Transición_ref_subset/04_Cluster/ds_cluster.rds")
df_vulnerable <- read_rds("03_Outputs/Vulnerable_ref_subset/04_Cluster/ds_cluster.rds")

# ---- seleccionar columnas relevantes ----
df_consolidada <- df_consolidada %>% 
  select(nivl_vl, nvl_label, sub_grp) %>%
  mutate(grupo_principal = "Consolidada")

df_transicion <- df_transicion %>% 
  select(nivl_vl, nvl_label, sub_grp) %>%
  mutate(grupo_principal = "Transición")

df_vulnerable <- df_vulnerable %>% 
  select(nivl_vl, nvl_label, sub_grp) %>%
  mutate(grupo_principal = "Vulnerable")

# ---- combinar todos los subclusters ----
data_integrada <- bind_rows(
  df_consolidada,
  df_transicion,
  df_vulnerable
) %>%
  filter(!is.na(sub_grp))  # Eliminar municipios sin asignación

# ---- cargar shapefile de municipios ----
map_mpios <- st_read("01_Data/01_Derived/maps/municipios_antioquia.shp", quiet = TRUE)

# ---- unir datos con geometría ----
data_map_integrado <- merge(
  map_mpios, 
  data_integrada,
  by.x = "nivl_vl", 
  by.y = "nivl_vl",
  all.x = TRUE,
  sort = FALSE
)

# ---- configurar factor con las 9 categorías ----
# Usar la paleta de colores definida en AUX_Functions_Clusters.R
data_map_integrado$categoria <- factor(
  data_map_integrado$sub_grp,
  levels = c(
    "Consolidada - Alto", "Consolidada - Medio", "Consolidada - Bajo",
    "Transición - Alto", "Transición - Medio", "Transición - Bajo",
    "Vulnerable - Alto", "Vulnerable - Medio", "Vulnerable - Bajo"
  )
)

# ---- calcular centroides para etiquetas ----
data_map_integrado <- data_map_integrado %>%
  mutate(
    centroid = st_centroid(geometry),
    x_centroid = st_coordinates(centroid)[,1],
    y_centroid = st_coordinates(centroid)[,2]
  )

# ---- generar mapa integrado ----
mapa_integrado <- ggplot(data_map_integrado) +
  geom_sf(aes(fill = categoria), color = "#606060", size = 0.05) +
  geom_text_repel(aes(x_centroid, y_centroid, label = nvl_lbl),
                  size = 2.5,
                  color = "black",
                  alpha = 0.9,
                  max.overlaps = Inf,
                  min.segment.length = 0,
                  force = 0.1,
                  force_pull = 5,
                  box.padding = 0.05,
                  point.padding = 0.1) +
  scale_fill_manual(
    values = col_palette,  # Paleta definida en AUX_Functions_Clusters.R
    na.value = "#ededed", 
    na.translate = FALSE
  ) +
  guides(fill = guide_legend(ncol = 1)) +
  xlab("") +
  labs(fill="")+
  map_theme +
  theme(
    legend.position = c(0.9, 0.15)
  )

# ---- exportar mapa ----
ggsave(
  filename = "03_Outputs/all/04_Cluster/subcluster_map.png",
  plot = mapa_integrado,
  height = h * 1.3,
  width = w * 0.85,
  dpi = d
)

# ---- mostrar mapa ----
print(mapa_integrado)

