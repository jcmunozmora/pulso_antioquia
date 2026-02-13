# ==============================================================================#
# ANTIOQUIAS - 04_Graficos_Brechas.R
# GRÁFICOS DE BRECHAS TERRITORIALES EN CONTEXTO
#
# OBJETIVO:
# Generar gráficos dot plot comparativos de las principales brechas de contexto
# entre grupos territoriales (Consolidada, Transición, Vulnerable) respecto a 
# la media departamental en componentes principales clave.
#
# INPUTS:  01_Data/01_Derived/pca_ds_Total.rds (scores de PCA)
#          03_Outputs/all/04_Cluster/ds_cluster.rds (asignación de clusters)
# OUTPUTS: 03_Outputs/all/05_Brechas/
#          - brecha_01_alcantarillado_pobreza.png
#          - brecha_02_desarrollo_economico.png
#          - brecha_03_salud.png
#          - brecha_04_seguridad.png
#          - brecha_05_capacidad_fiscal.png
#
# METODOLOGÍA:
# Se calculan promedios por grupo territorial para cada componente principal,
# luego se expresan como desviaciones estandarizadas respecto a la media 
# departamental (en unidades de desviación estándar).
#
# ETAPA DEL PIPELINE: 4 - Análisis de brechas y visualización
# ==============================================================================#

# ---- packages ----
rm(list = ls())

# Semilla para replicabilidad
set.seed(1601)

pacman::p_load(
  tidyverse,
  readr,
  dplyr,
  tidyr,
  ggplot2,
  scales
)

# ---- paths ----
path_in <- "01_Data/01_Derived"
path_cluster <- "03_Outputs/all/04_Cluster"
path_out <- "03_Outputs/all/05_Brechas"

# Crear directorio de salida si no existe
if (!dir.exists(path_out)) {
  dir.create(path_out, recursive = TRUE)
}

# ---- parámetros gráficos ----
# Paleta de colores por grupo territorial
col_grupos <- c(
  "Consolidada" = "#2E7D32",  # Verde oscuro
  "Transición" = "#F9A825",    # Amarillo/ámbar
  "Vulnerable" = "#C62828"     # Rojo oscuro
)

# Tema base para gráficos institucionales
theme_brechas <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0, color = "gray30"),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

# Dimensiones de exportación
w <- 10
h <- 8
d <- 300

# ==============================================================================#
# LECTURA Y PREPARACIÓN DE DATOS
# ==============================================================================#

# ---- lectura de datos ----
# Dataset completo de PCA con scores
pca_ds <- read_rds(file.path(path_in, "pca_ds_Total.rds"))

# Asignaciones de cluster global
cluster_ds <- read_rds(file.path(path_cluster, "ds_cluster.rds")) %>%
  dplyr::select(ind_mpio = nivl_vl, nvl_label, sub_grp)

# ---- unión de datasets ----
# Combinar scores de PCA con asignaciones de grupo territorial
ds_completo <- pca_ds %>%
  dplyr::inner_join(cluster_ds, by = c("ind_mpio", "nvl_label")) %>%
  tidyr::drop_na()

# ---- selección de componentes principales clave ----
# Definir componentes por categoría de análisis en una lista estructurada
categorias_config <- list(
  vivienda = list(
    nombre = "Características de la Vivienda",
    vars = c("idx5_viviendas1", "idx5_viviendas2"),
    archivo = "brecha_01_vivienda_pobreza.png",
    altura = h
  ),
  pobreza = list(
    nombre = "Pobreza",
    vars = c("idx11_pobreza1", "idx11_pobreza2"),
    archivo = "brecha_01_vivienda_pobreza.png",  # Comparte gráfico con vivienda
    altura = h
  ),
  economia = list(
    nombre = "Desarrollo Económico",
    vars = c("idx6_crecimiento1", "idx6_crecimiento2"),
    archivo = "brecha_02_desarrollo_economico.png",
    altura = h * 0.6
  ),
  salud = list(
    nombre = "Salud",
    vars = c("idx12_salud1", "idx12_salud2"),
    archivo = "brecha_03_salud.png",
    altura = h * 0.6
  ),
  seguridad = list(
    nombre = "Seguridad",
    vars = c("idx14_seguridad1", "idx14_seguridad2"),
    archivo = "brecha_04_seguridad.png",
    altura = h * 0.6
  ),
  capacidad = list(
    nombre = "Capacidad Fiscal",
    vars = c("idx4_Capacidad1"),
    archivo = "brecha_05_capacidad_fiscal.png",
    altura = h * 0.4
  ),
  adultez = list(
    nombre = "Adultez",
    vars = c("idx2_Adultez1", "idx2_Adultez2"),
    archivo = "brecha_06_adultez.png",
    altura = h * 0.6
  ),
  infancia = list(
    nombre = "Infancia y Niñez",
    vars = c("idx9_infnin1", "idx9_infnin2"),
    archivo = "brecha_07_infancia_ninez.png",
    altura = h * 0.6
  ),
  juventud = list(
    nombre = "Juventud",
    vars = c("idx10_juventud1", "idx10_juventud2"),
    archivo = "brecha_08_juventud.png",
    altura = h * 0.6
  ),
  vejez = list(
    nombre = "Vejez",
    vars = c("idx15_vejez1", "idx15_vejez2"),
    archivo = "brecha_09_vejez.png",
    altura = h * 0.6
  )
)

# ==============================================================================#
# CÁLCULO DE BRECHAS ESTANDARIZADAS
# ==============================================================================#

# ---- función para calcular brechas ----
calcular_brechas <- function(data, vars_interes, categoria) {
  # Calcular media y desviación estándar departamental (global)
  stats_dept <- data %>%
    dplyr::select(dplyr::all_of(vars_interes)) %>%
    summarise(across(everything(), list(
      media = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    )))
  
  # Calcular media por grupo territorial
  medias_grupo <- data %>%
    dplyr::select(sub_grp, dplyr::all_of(vars_interes)) %>%
    group_by(sub_grp) %>%
    summarise(across(dplyr::all_of(vars_interes), 
                     ~mean(.x, na.rm = TRUE), 
                     .names = "{.col}"))
  
  # Calcular desviaciones estandarizadas respecto a media departamental
  brechas <- medias_grupo %>%
    pivot_longer(cols = -sub_grp, names_to = "componente", values_to = "media_grupo") %>%
    dplyr::mutate(
      media_dept = map_dbl(componente, ~stats_dept[[paste0(.x, "_media")]]),
      sd_dept = map_dbl(componente, ~stats_dept[[paste0(.x, "_sd")]]),
      brecha_std = (media_grupo - media_dept) / sd_dept,
      categoria = categoria
    )
  
  return(brechas)
}

# ---- calcular todas las brechas ----
# Calcular brechas para todas las categorías de forma iterativa
lista_brechas <- map2(
  categorias_config,
  names(categorias_config),
  ~calcular_brechas(ds_completo, .x$vars, .x$nombre)
)
names(lista_brechas) <- names(categorias_config)

# ==============================================================================#
# DICCIONARIO DE ETIQUETAS LEGIBLES
# ==============================================================================#
# Etiquetas basadas en las variables con mayor carga (loading) en cada
# componente principal. Fuente: archivos *_loadings_y_tops.xlsx en 03_Outputs/all/

# Crear etiquetas descriptivas para cada componente
etiquetas_componentes <- c(
  # Características de la Vivienda
  # PC1: Sin alcantarillado urbano, déficit cuantitativo, hacinamiento
  "idx5_viviendas1" = "Suficiencia de infraestructura y espacio habitacional",
  # PC2: Déficit cualitativo, sin alcantarillado rural
  "idx5_viviendas2" = "Calidad de vivienda en zonas rurales",
  
  # Pobreza
  # PC1: Inseguridad alimentaria, NBI, línea de pobreza
  "idx11_pobreza1" = "Vulnerabilidad estructural y NBI",
  # PC2: Inseguridad alimentaria moderada en hogares con menores, IPM (-)
  "idx11_pobreza2" = "Inseguridad alimentaria en hogares con niños",
  
  # Desarrollo Económico
  # PC1: Vías (total, terciaria, primaria), internet
  "idx6_crecimiento1" = "Infraestructura vial y conectividad",
  # PC2: Densidad empresarial, internet, vías primarias (+) vs terciarias (-)
  "idx6_crecimiento2" = "Dinamismo empresarial",
  # PC3: Corresponsales bancarios, microcréditos
  #"idx6_crecimiento3" = "Inclusión financiera",
  
  # Salud
  # PC1: Dengue, malaria, leishmaniasis
  "idx12_salud1" = "Enfermedades transmitidas por vectores",
  # PC2: Camas hospitalarias, leishmaniasis rural
  "idx12_salud2" = "Infraestructura hospitalaria",
  
  # Seguridad
  # PC1: IRV, tasa de homicidios, percepción de seguridad
  "idx14_seguridad1" = "Violencia letal y riesgo de victimización",
  # PC2: Tasa de delitos (-), VCM/VIF/VSX (-), percepción (-)
  "idx14_seguridad2" = "Delitos y violencias de género e intrafamiliares",
  
  # Capacidad Fiscal
  # PC1: Medición desempeño municipal, índice desempeño fiscal
  "idx4_Capacidad1" = "Desempeño fiscal municipal",
  # PC2: Categoría municipal (+), desempeño fiscal (-)
  # "idx4_Capacidad2" = "Categoría y tamaño municipal",
  
  # Adultez
  # PC1: Tasa desocupación mujeres, brecha M-H, desocupación hombres
  "idx2_Adultez1" = "Desempleo y brechas laborales de género",
  # PC2: Empleo informal, afiliación salud, desocupación
  "idx2_Adultez2" = "Informalidad y protección social",
  
  # Infancia y Niñez
  # PC1: Mortalidad desnutrición, desnutrición aguda, cobertura primaria
  "idx9_infnin1" = "Mortalidad y desnutrición infantil",
  # PC2: Cobertura transición/primaria, repitencia primaria
  "idx9_infnin2" = "Acceso y permanencia en educación inicial",
  
  # Juventud
  # PC1: Fecundidad adolescente, calidad educativa
  "idx10_juventud1" = "Fecundidad adolescente y vulnerabilidad juvenil",
  # PC2: Repitencia media, cobertura secundaria
  "idx10_juventud2" = "Cobertura y permanencia en educación media",
  
  # Vejez
  # PC1: Índice de envejecimiento poblacional
  "idx15_vejez1" = "Envejecimiento demográfico y mortalidad asociada",
  "idx15_vejez2" = "Cobertura pensional y protección en la vejez"
)

# Aplicar etiquetas a todos los datasets de brechas
aplicar_etiquetas <- function(data) {
  data %>%
    dplyr::mutate(
      componente_label = factor(componente, 
                                levels = names(etiquetas_componentes),
                                labels = etiquetas_componentes),
      sub_grp = factor(sub_grp, 
                      levels = c("Consolidada", "Transición", "Vulnerable"))
    )
}

# Aplicar etiquetas a todas las brechas
lista_brechas <- map(lista_brechas, aplicar_etiquetas)

# ==============================================================================#
# GENERACIÓN DE GRÁFICOS DE FORMA ITERATIVA
# ==============================================================================#

# ---- función genérica para crear gráficos ----
crear_grafico_brecha <- function(data, add_hline = NULL) {
  pos_dodge <- position_dodge(width = 0.7)
  
  g <- ggplot(data, aes(x = brecha_std, y = fct_rev(componente_label), 
                        color = sub_grp, group = sub_grp)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_point(aes(size = abs(brecha_std)), alpha = 0.7, position = pos_dodge) +
    geom_text(aes(label = sprintf("%.1f", brecha_std)), 
              color = "white", size = 3.5, fontface = "bold",
              position = pos_dodge) +
    scale_color_manual(values = col_grupos) +
    scale_size_continuous(range = c(8, 18), guide = "none") +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_x_continuous(
      breaks = seq(-2, 2, 0.5),
      labels = label_number(accuracy = 0.1),
      limits = c(NA, NA)
    ) +
    facet_wrap(~categoria, ncol = 1, scales = "free_y") +
    labs(
      x = "Desviación estándar respecto a la media de Antioquia",
      y = NULL
    ) +
    theme_brechas
  
  # Agregar líneas horizontales si se especifican
  if(!is.null(add_hline)) {
    g <- g + geom_hline(yintercept = add_hline, color = "gray40", linewidth = 0.4)
  }
  
  return(g)
}

# ---- generar y exportar todos los gráficos ----
# Gráfico combinado: Vivienda y Pobreza (línea separadora entre componentes en cada panel)
brechas_panel1 <- bind_rows(lista_brechas$vivienda, lista_brechas$pobreza)
g1 <- crear_grafico_brecha(brechas_panel1, add_hline = 1.5)  # Línea entre componentes dentro de cada facet
ggsave(
  filename = file.path(path_out, "brecha_01_vivienda_pobreza.png"),
  plot = g1, width = w, height = h, dpi = d, bg = "white"
)

# Resto de gráficos individuales con líneas horizontales apropiadas
# Economía: 2 componentes, línea en 1.5
g_economia <- crear_grafico_brecha(lista_brechas$economia, add_hline = 1.5)
ggsave(
  filename = file.path(path_out, "brecha_02_desarrollo_economico.png"),
  plot = g_economia, width = w, height = h * 0.6, dpi = d, bg = "white"
)

# Gráficos con 2 componentes: salud, seguridad, adultez, infancia, juventud, vejez
categorias_dos_comp <- c("salud", "seguridad", "adultez", "infancia", "juventud", "vejez")
for (cat in categorias_dos_comp) {
  g <- crear_grafico_brecha(lista_brechas[[cat]], add_hline = 1.5)
  ggsave(
    filename = file.path(path_out, categorias_config[[cat]]$archivo),
    plot = g,
    width = w,
    height = categorias_config[[cat]]$altura,
    dpi = d,
    bg = "white"
  )
}

# Capacidad fiscal: 1 componente, sin líneas
g_capacidad <- crear_grafico_brecha(lista_brechas$capacidad, add_hline = NULL)
ggsave(
  filename = file.path(path_out, "brecha_05_capacidad_fiscal.png"),
  plot = g_capacidad, width = w, height = h * 0.4, dpi = d, bg = "white"
)



# ==============================================================================#
# RESUMEN Y EXPORTACIÓN DE DATOS
# ==============================================================================#

# ---- consolidar todas las brechas ----
brechas_consolidadas <- bind_rows(lista_brechas) %>%
  dplyr::select(
    grupo_territorial = sub_grp,
    categoria,
    componente,
    componente_descripcion = componente_label,
    media_grupo,
    media_departamental = media_dept,
    desviacion_estandar = sd_dept,
    brecha_estandarizada = brecha_std
  )

# ---- exportar tabla consolidada ----
write_csv(
  brechas_consolidadas,
  file.path(path_out, "brechas.csv")
)

writexl::write_xlsx(
  brechas_consolidadas,
  file.path(path_out, "brechas.xlsx")
)
