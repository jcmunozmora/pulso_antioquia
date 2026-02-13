# ==============================================================================#
# ANTIOQUIAS - AUX_Functions_Clusters.R
# FUNCIONES AUXILIARES PARA ANÁLISIS DE CLUSTERING TERRITORIAL
#
# OBJETIVO:
# Proporcionar funciones reutilizables para análisis de clustering jerárquico,
# incluyendo diagnósticos (método Elbow, dendrogramas), caracterización de clusters,
# y visualizaciones espaciales y comparativas.
#
# FUNCIONES PRINCIPALES:
#   - ps_cluster(): Pipeline completo de clustering territorial con outputs
#
# ETAPA DEL PIPELINE: 3 (Clustering territorial)
# ==============================================================================#

# ---- packages ----
pacman::p_load(
  stargazer,
  openxlsx,
  writexl,
  dplyr,
  factoextra,
  tidyr,
  tidyverse,
  glue,
  cluster,
  ggplot2,
  sp,
  sf,
  ggrepel,
  maps,
  cowplot
)

# ==============================================================================#
# CONFIGURACIÓN GLOBAL DE VISUALIZACIÓN ----
# ==============================================================================#

# ---- parámetros gráficos ----
w <- 4.5*2.5      # Ancho base de gráficos
h <- 3.2*2.5      # Alto base de gráficos
text <- 15        # Tamaño de fuente base
d <- 900          # DPI para exportación
s <- 1.2          # Factor de escala
a_line <- 0.6     # Alpha de líneas
a_dot <- 0.7      # Alpha de puntos

# ---- tema general ----
theme <- list(
  theme_classic(base_size = text*1.5),
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()))

# ---- tema para mapas ----
map_theme <- list(
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = rel(1)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(vjust = -0.2,colour = "black"),
    axis.text = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ))

# ---- paletas de colores ----
# Paleta base para clusters globales (3 grupos)
base_palette <- c(
  "Consolidada" = "#88B08D",
  "Transición"  = "#DDD878",
  "Vulnerable"  = "#CE5244"
)

# Paleta extendida para subclusters (3 grupos × 3 niveles = 9 combinaciones)
col_palette <- c(
  # Consolidada (tonos verdes)
  "Consolidada - Alto"  = "#88B08D",
  "Consolidada - Medio"= "#9EBA88",
  "Consolidada - Bajo" = "#B3C383",
  
  # Transición (tonos amarillos)
  "Transición - Alto"  = "#C8CE7D",
  "Transición - Medio"= "#DDD878",
  "Transición - Bajo" = "#D9B76B",
  
  # Vulnerable (tonos rojos)
  "Vulnerable - Alto"  = "#D6955F",
  "Vulnerable - Medio"= "#D27452",
  "Vulnerable - Bajo" = "#CE5244"
)

# ==============================================================================#
# LABELS DE VARIABLES (31 COMPONENTES PCA) ----
# ==============================================================================#
# Vector de etiquetas descriptivas para los 31 componentes principales del
# análisis de clustering. Corresponden a PC1 de cada subdimensión temática.

lab <- c(   "Acceso a servicio de agua potable y saneamiento - 1",
            "Adultez - 1",
            "Adultez - 2",
            "Adultez - 3",
            "Cambio climatico - 1",
            "Cambio climatico - 2",
            "Capacidad fiscal - 1",
            "Capacidad fiscal - 2",
            "Características de las viviendas - 1",
            "Características de las viviendas - 2",
            "Desarrollo económico - 1",
            "Desarrollo económico - 2",
            "Desarrollo económico - 3",
            "Desigualdad - 1",
            "Estructura demográfica - 1",
            "Infancia y Niñez - 1",
            "Infancia y Niñez - 2",
            "Infancia y Niñez - 3",
            "Infancia y Niñez - 4",
            "Juventud - 1",
            "Juventud - 2",
            "Juventud - 3",
            "Juventud - 4",
            "Pobreza - 1",
            "Pobreza - 2",
            "Salud - 1",
            "Salud - 2",
            "Salud mental - 1",
            "Salud mental - 2",
            "Seguridad - 1",
            "Seguridad - 2",
            "Vejez - 1"
                )



# ==============================================================================#
# FUNCIÓN: ps_cluster ----
# ==============================================================================#
# Pipeline completo de análisis de clustering jerárquico territorial.
# Implementa Ward's method con reordenamiento por desempeño, genera diagnósticos
# (método Elbow, dendrogramas, matriz de distancias), caracterización de clusters,
# y visualizaciones comparativas (barras) y espaciales (mapas).
#
# PARÁMETROS:
#   pca_ds  - Data frame con índices PCA y metadata municipal (ind_mpio, nvl_label)
#   df      - Data frame numérico con 31 componentes principales para clustering
#   lab_g   - Vector de 3 etiquetas para clusters (ej: c("Consolidada", "Transición", "Vulnerable"))
#   path    - Subcarpeta en 03_Outputs/ para guardar resultados (ej: "all", "Consolidada_ref_dept")
#   eje     - Etiqueta del eje Y en gráficos de barras (ej: "Componente Principal (normalizado)")
#
# OUTPUTS GENERADOS:
#   03_Outputs/{path}/04_Cluster/
#     - Summary_Stats.xlsx: Estadísticas descriptivas de variables
#     - cluster_dist.png: Matriz de distancias visualizada
#     - cluster_elbow.png: Método Elbow para número óptimo de clusters
#     - cluster_dendo.png: Dendrograma jerárquico con colores
#     - cluster_dendo_vars.png: Biplot de clusters por componentes
#     - cluster_variables_contexto_1.png: Variables de contexto (agua, clima, fiscal, vivienda)
#     - cluster_variables_contexto_2.png: Variables de contexto (economía, demografía, salud, seguridad)
#     - cluster_variables_res_1.png: Variables de resultado (ciclo de vida)
#     - cluster_map.png: Mapa coroplético de clusters
#     - ds_cluster.csv/.rds: Dataset completo con asignación de clusters
#
# METODOLOGÍA:
#   1. Clustering jerárquico con Ward's method (minimiza varianza intra-cluster)
#   2. Corte del dendrograma en k=3 grupos
#   3. Reordenamiento: Cluster 1 = mejor desempeño (promedio PC1 más alto)
#   4. Caracterización por promedios de grupo
#   5. Visualización espacial con shapefile de Antioquia
# ==============================================================================#

ps_cluster <- function(pca_ds, df, lab_g, path, eje){

  # ---- crear directorios de salida ----
  output_dirs <- c(
    file.path("03_Outputs", path, "04_Cluster")
  )
  for (dir in output_dirs) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # ---- corregir nombres de municipios ----
  # Usar nombres bien formateados del shapefile (nvl_lbl) en lugar de nvl_label
  # Esto asegura consistencia en mapas y dendrogramas
  map_dptos_lookup <- st_read("01_Data/01_Derived/maps/municipios_antioquia.shp", quiet = TRUE)
  
  # Limpiar topología: corregir geometrías inválidas y solapamientos
  map_dptos_lookup <- st_make_valid(map_dptos_lookup)
  
  # Simplificar geometría preservando topología (elimina vértices redundantes)
  # dTolerance controla qué tanto simplificar (0.0001 ≈ 11 metros)
  map_dptos_lookup <- st_simplify(map_dptos_lookup, preserveTopology = TRUE, dTolerance = 0.0001)
  
  # Crear diccionario: ind_mpio -> nvl_lbl (nombre bien formateado)
  nombres_lookup <- map_dptos_lookup %>%
    st_drop_geometry() %>%
    select(nivl_vl, nvl_lbl) %>%
    distinct()
  
  # Reemplazar rownames con nombres bien formateados
  ind_mpios <- pca_ds$ind_mpio
  nuevos_nombres <- nombres_lookup$nvl_lbl[match(ind_mpios, nombres_lookup$nivl_vl)]
  
  # Si hay match, usar nombre del shapefile; sino, mantener rowname actual
  nuevos_nombres <- ifelse(!is.na(nuevos_nombres), nuevos_nombres, rownames(df))
  rownames(df) <- nuevos_nombres

  # ---- estadísticas descriptivas ----
  # Filtrar solo variables válidas (con al menos un valor no-missing)
  valid_cols <- names(df)[colSums(!is.na(df)) > 0]
  df_num_valid <- df[, valid_cols, drop=FALSE]
  
  # Tabla de estadísticas con etiquetas descriptivas
  summary_export <- data.frame(
    Variable = names(df_num_valid),
    Etiqueta = lab,
    N = sapply(df_num_valid, function(x) sum(!is.na(x))),
    Media = sapply(df_num_valid, mean, na.rm = TRUE),
    SD = sapply(df_num_valid, sd, na.rm = TRUE),
    Min = sapply(df_num_valid, min, na.rm = TRUE),
    Max = sapply(df_num_valid, max, na.rm = TRUE)
  )
  write_xlsx(summary_export, path = glue("03_Outputs/",path,"/04_Cluster/Summary_Stats.xlsx"))
    

  # ==============================================================================#
  # PASO 1: Matriz de distancias ----
  # ==============================================================================#
  # Calcular distancia euclidiana entre municipios (mayor distancia = más diferentes)
  
  dist_mat <- dist(df, method = "euclidean")
  
  fviz_dist(dist_mat,
            gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(fill = "Distancia") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 4),
          axis.text.y = element_text(size = 3))
  
  ggsave(file=paste0("03_Outputs/",path,"/04_Cluster/cluster_dist.png"), 
         height = h, width = w*0.8, dpi = 900)
  

  # ==============================================================================#
  # PASO 2: Selección de método de clustering ----
  # ==============================================================================#
  # Coeficiente aglomerativo: mide estructura de clustering encontrada
  # Valores cercanos a 1 sugieren estructura fuerte. Se usa Ward por mejor desempeño.
  
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average", "single", "complete", "ward")
  
  ac <- function(x) {
    agnes(df, method = x)$ac
  }
  
  map_dbl(m, ac)  # Ward típicamente obtiene coeficiente más alto
  
  
  # ==============================================================================#
  # PASO 3: Número óptimo de clusters (método Elbow) ----
  # ==============================================================================#
  # Calcula within-cluster sum of squares para k=1 a 10
  # El "codo" en k=3 sugiere que 3 clusters es óptimo
  
  wss <- numeric()
  for(h in 1:10){
    b <- kmeans(df, h)
    wss[h] <- b$tot.withinss  # Suma de cuadrados intra-cluster
  }
  
  wss1 <- data.frame(cluster=c(1:10), wss)
  
  p_elbow <- ggplot(wss1, aes(cluster, wss)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_vline(xintercept = 3, linetype = 2, color = "red") +
    labs(title = "Método Elbow") +
    scale_x_continuous(breaks = 1:10) +
    theme_classic()
  
  ggsave(
    filename = paste0("03_Outputs/", path, "/04_Cluster/cluster_elbow.png"),
    plot = p_elbow,
    height = h * 0.8,
    width = w,
    dpi = 900
  )
  
  
  # ==============================================================================#
  # PASO 4: Clustering jerárquico con Ward ----
  # ==============================================================================#
  
  # Clustering jerárquico con Ward's method (minimiza varianza intra-cluster)
  hc5 <- hclust(dist_mat, method = "ward.D2")
  
  # Corte inicial del dendrograma en 3 clusters
  sub_grp_raw <- cutree(hc5, k = 3)
  
  # Reordenamiento por desempeño
  cols_pc1 <- grep("1$", colnames(df), value = TRUE)
  scores <- rowMeans(df[, cols_pc1, drop = FALSE], na.rm = TRUE)
  
  df_ranking <- data.frame(
    old_cluster = sub_grp_raw,
    score_orden = scores,
    peso = ifelse(grepl("^Medellín$|^Medellin$", rownames(df), ignore.case = TRUE), 1.5, 1.0)
  )
  
  # Calcular desempeño promedio por cluster y asignar nuevos IDs
  resumen_clusters <- df_ranking %>%
    group_by(old_cluster) %>%
    summarise(avg_score = sum(score_orden * peso) / sum(peso)) %>%
    arrange(desc(avg_score)) %>%
    mutate(new_cluster_id = row_number())
  
  # Mapear clusters: 1 = Mejor desempeño, 3 = Peor desempeño
  map_clusters <- resumen_clusters$new_cluster_id
  names(map_clusters) <- resumen_clusters$old_cluster
  
  # Aplicar reordenamiento
  sub_grp <- as.integer(map_clusters[as.character(sub_grp_raw)])
  
  # ---- generar dendrograma ----
  p_dendo <- fviz_dend(
    hc5,
    cex = 0.5,
    k = 3,
    rect = TRUE
  )
  
  ggsave(
    filename = paste0("03_Outputs/", path, "/04_Cluster/cluster_dendo.png"),
    plot = p_dendo,
    height = h * 0.8,
    width = w,
    dpi = 300
  )
  
  # ---- biplot de clusters por componentes principales ----
  p_dendo_vars <- fviz_cluster(list(data = df, cluster = sub_grp),
                   palette = c("#2E9FDF",  "#E7B800", "#FC4E07"),
                   ellipse.type = "convex",
                   repel = F,
                   show.clust.cent = FALSE, 
                   ggtheme = theme_minimal())
  
  ggsave(
    filename = paste0("03_Outputs/", path, "/04_Cluster/cluster_dendo_vars.png"),
    plot = p_dendo_vars,
    height = h * 0.8,
    width = w,
    dpi = 300
  )
 
  # ==============================================================================#
  # PASO 5: Caracterización de clusters ----
  # ==============================================================================#
  
  # Construir dataset con clusters asignados
  datos.j <- as.data.frame(cbind(df, sub_grp))
  datos.j_aux <- tibble::rownames_to_column(as.data.frame(datos.j), var = "municipio")
  
  # ---- calcular promedios por cluster ----
  datos.j %>%   
    group_by(sub_grp) %>%
    summarise_all(list(mean)) -> medias
  
  # Media general de todas las variables
  datos.j %>%  
    dplyr::select(-c(sub_grp)) %>% 
    summarise_if(base::is.numeric, mean) %>%
    base::round(4) -> general
  general <- cbind(sub_grp="general", general)
  
  # ---- transformar a formato largo para visualización ----
  gathered_datos.j <- pivot_longer(data=medias,
                                   -sub_grp,
                                   names_to="variable",
                                   values_to = "valor")
  
  # Aplicar etiquetas descriptivas
  gathered_datos.j$variable <- factor(gathered_datos.j$variable,
                                      levels=unique(gathered_datos.j$variable),
                                      labels=lab)
  
  # Aplicar etiquetas personalizadas a clusters
  gathered_datos.j$sub_grp <- factor(gathered_datos.j$sub_grp, 
                                     levels=c(1,2,3),
                                     labels = lab_g)
  
  # ---- configurar paleta dinámica ----
  # Detecta si es cluster simple (3 grupos) o subcluster (9 combinaciones)
  levs <- levels(gathered_datos.j$sub_grp)
  
  if (all(levs %in% names(base_palette))) {
    # Caso 1: Cluster global (usa paleta base)
    col_palette_named <- base_palette[levs]
    
  } else {
    # Caso 2: Subclusters (normaliza nombres y usa paleta extendida)
    levs_clean <- levs
    levs_clean <- gsub(" - Alta$",  " - Alto",  levs_clean)
    levs_clean <- gsub(" - Media$", " - Medio", levs_clean)
    levs_clean <- gsub(" - Baja$",  " - Bajo",  levs_clean)
    
    faltan <- setdiff(levs_clean, names(col_palette))
    if (length(faltan) > 0)
      stop(paste("No están en col_palette:", paste(faltan, collapse = ", ")))
    
    col_palette_named <- col_palette[levs_clean]
    names(col_palette_named) <- levs
  }
    
  # ==============================================================================#
  # PASO 6: Gráficos comparativos de clusters ----
  # ==============================================================================#
  
  # ---- Gráfico 1: Variables de contexto (infraestructura y ambiente) ----
  ds_con_1 <- gathered_datos.j %>% filter((variable %in% c(
    "Acceso a servicio de agua potable y saneamiento - 1",
    "Cambio climatico - 1",
    "Cambio climatico - 2",
    "Capacidad fiscal - 1",
    "Capacidad fiscal - 2",
    "Características de las viviendas - 1",
    "Características de las viviendas - 2"
  )))
  
  ggplot(data=ds_con_1, aes(x=variable, y=valor, fill=sub_grp)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_manual(values = col_palette_named,
                      na.value = "#ededed", na.translate = FALSE)+
    guides(fill = guide_legend(ncol = 3))+
    theme_minimal()+
    ylim(c(-2.5, 2.5))+
    theme(legend.position = c(0.7, 0.1),
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y=eje, x="")
  
  ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_variables_contexto_1.png"),
         height = h*0.8, width = w, dpi = 900) 
    
    
    
  # ---- Gráfico 2: Variables de contexto (economía, demografía, salud, seguridad) ----
  ds_con_2 <- gathered_datos.j %>% filter((variable %in% c(
    "Desarrollo económico - 1",
    "Desarrollo económico - 2",
    "Desarrollo económico - 3",
    "Desigualdad - 1",
    "Estructura demográfica - 1",
    "Pobreza - 1",
    "Pobreza - 2",
    "Salud - 1",
    "Salud - 2",
    "Salud mental - 1",
    "Salud mental - 2",
    "Seguridad - 1",
    "Seguridad - 2"
  )))
  
  ggplot(data=ds_con_2, aes(x=variable, y=valor, fill=sub_grp)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_manual(values = col_palette_named,
                      na.value = "#ededed", na.translate = FALSE)+
    guides(fill = guide_legend(ncol = 3))+
    theme_minimal()+
    ylim(c(-2.5, 2.5))+
    theme(legend.position = c(0.7, 0.1),
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y=eje, x="")
  
  ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_variables_contexto_2.png"),
         height = h*0.8, width = w, dpi = 900) 
    
  
  # ---- Gráfico 3: Variables de resultado (ciclo de vida) ----
  ds_res_1 <- gathered_datos.j %>% filter((variable %in% c(
    "Infancia y Niñez - 1",
    "Infancia y Niñez - 2",
    "Infancia y Niñez - 3",
    "Infancia y Niñez - 4",
    "Juventud - 1",
    "Juventud - 2",
    "Juventud - 3",
    "Juventud - 4",
    "Adultez - 1",
    "Adultez - 2",
    "Adultez - 3",
    "Vejez - 1"
  )))
  
  ggplot(data=ds_res_1, aes(x=variable, y=valor, fill=sub_grp)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_manual(values = col_palette_named,
                      na.value = "#ededed", na.translate = FALSE)+
    guides(fill = guide_legend(ncol = 3))+
    theme_minimal()+
    ylim(c(-2.5, 2.5))+
    theme(legend.position = c(0.7, 0.1),
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y=eje, x="")
  
  ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_variables_res_1.png"),
         height = h*0.8, width = w, dpi = 900) 
    
  # ==============================================================================#
  # PASO 7: Mapa coroplético de clusters ----
  # ==============================================================================#
  
  # Nota: shapefile ya fue cargado al inicio de la función (map_dptos_lookup)
  # Reutilizar para evitar doble lectura
  
  # Combinar datos de clusters con geometría espacial
  ds_out <- cbind(pca_ds[,c("ind_mpio","nvl_label")], datos.j)
  
  data_map <- merge(
    map_dptos_lookup, ds_out,
    by.x = "nivl_vl", by.y = "ind_mpio",
    all.x = TRUE,  # Mantener todos los municipios del shapefile
    sort = FALSE
  )
  
  # Aplicar etiquetas personalizadas a clusters
  data_map$sub_grp <- factor(data_map$sub_grp,
                             levels = c(1,2,3),
                             labels = lab_g)
  
  # ---- configurar paleta para mapa ----
  levs <- levels(data_map$sub_grp)
  
  if (all(levs %in% names(base_palette))) {
    # Caso 1: Cluster global
    col_palette_named <- base_palette[levs]
    
  } else {
    # Caso 2: Subclusters
    levs_clean <- levs
    levs_clean <- gsub(" - Alta$",  " - Alto",  levs_clean)
    levs_clean <- gsub(" - Media$", " - Medio", levs_clean)
    levs_clean <- gsub(" - Baja$",  " - Bajo",  levs_clean)
    
    faltan <- setdiff(levs_clean, names(col_palette))
    if (length(faltan) > 0)
      stop(paste("No están en col_palette:", paste(faltan, collapse = ", ")))
    
    col_palette_named <- col_palette[levs_clean]
    names(col_palette_named) <- levs
  }
    
  # ---- calcular centroides para etiquetas ----
  data_map <- data_map %>%
    mutate(centroid = st_centroid(geometry),
           x_centroid = st_coordinates(centroid)[,1],
           y_centroid = st_coordinates(centroid)[,2])
  
  # ---- generar mapa coroplético ----
  g1 <- ggplot(data_map) +
    geom_sf(aes(fill = factor(sub_grp)), color = "#606060", size = 0.05) +
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
    scale_fill_manual(values = col_palette_named,
                      na.value = "#ededed", na.translate = FALSE)+
    guides(fill = guide_legend(ncol = 1)) +
    xlab("") +
    labs(fill="") +
    map_theme +
    theme(legend.position = c(0.8, 0.1))
  
  ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_map.png"), 
         g1, height = h*1.2, width = w*0.8, dpi = d)
    
  # ==============================================================================#
  # PASO 8: Exportación de resultados ----
  # ==============================================================================#
  
  # Reconstruir dataset final con clusters
  datos.j <- as.data.frame(cbind(df, sub_grp))
  datos.j_aux <- tibble::rownames_to_column(as.data.frame(datos.j), var = "municipio")
  
  # Guardar dataset sin geometría
  data_map_aux <- st_drop_geometry(data_map)
  readr::write_csv(data_map_aux, paste0("03_Outputs/", path, "/04_Cluster/ds_cluster.csv"))
  write_rds(data_map_aux, paste0("03_Outputs/", path, "/04_Cluster/ds_cluster.rds"), 
            "xz", compression = 9L)
  
}  # Fin de ps_cluster() 