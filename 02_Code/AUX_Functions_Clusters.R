## 00 - Libraries
###===============
library(stargazer)
library(openxlsx)
library(writexl)
#library(igraph)
library(dplyr)
library(factoextra)
library(tidyr)
library(tidyverse)
library(glue)
library(cluster)
library(ggplot2)
library(sp)
library(sf)
library(ggrepel)
library("maps")
library("ggplot2")
library("cowplot")

###===================
### 01 - Map Stylish
#===

w <- 4.5*2.5
h <- 3.2*2.5
text <- 15
d <- 900
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

theme <- list(
  theme_classic(base_size = text*1.5),
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()))

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

# Paleta base (misma en mapas y gráficos)
base_palette <- c(
  "Consolidada" = "#88B08D",
  "Transición"  = "#DDD878",
  "Vulnerable"  = "#CE5244"
)

#Para análisis de clusters
col_palette <- c(
  # Consolidada
  "Consolidada - Alto"  = "#88B08D",
  "Consolidada - Medio"= "#9EBA88",
  "Consolidada - Bajo" = "#B3C383",
  
  # Transición
  "Transición - Alto"  = "#C8CE7D",
  "Transición - Medio"= "#DDD878",
  "Transición - Bajo" = "#D9B76B",
  
  # Vulnerable
  "Vulnerable - Alto"  = "#D6555F",
  "Vulnerable - Medio"= "#D27452",
  "Vulnerable - Bajo" = "#CE5244"
)

# Genera tonos: Bajo (claro) -> Medio -> Alto (color base)
#make_tints3 <- function(hex){
  #pal <- grDevices::colorRampPalette(c("#FFFFFF", hex))(9)
  #pal[c(3, 6, 9)]
#}



## 00 - labels
###===============

### Labels
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



####---------
ps_cluster <- function(pca_ds,df,lab_g,path, eje){

  # Ensure all required output directories exist
  output_dirs <- c(
    file.path("03_Outputs", path, "04_Cluster")
  )
  for (dir in output_dirs) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

    ### Descriptive
    valid_cols <- names(df)[colSums(!is.na(df)) > 0]
    df_num_valid <- df[, valid_cols, drop=FALSE]
    # Exportar datos con etiquetas para Quarto
    summary_export <- data.frame(
      Variable = names(df_num_valid),
      Etiqueta = lab,
      #Etiqueta = lb$lab[match(names(df_num_valid), lb$terms)],
      N = sapply(df_num_valid, function(x) sum(!is.na(x))),
      Media = sapply(df_num_valid, mean, na.rm = TRUE),
      SD = sapply(df_num_valid, sd, na.rm = TRUE),
      Min = sapply(df_num_valid, min, na.rm = TRUE),
      Max = sapply(df_num_valid, max, na.rm = TRUE)
    )
    write_xlsx(summary_export, path = glue("03_Outputs/",path,"/04_Cluster/Summary_Stats.xlsx"))
    

  ##### Distance Matrix
  #==============================
  
    ### Prepare our data
    d <- dist(df, method = "euclidean")
    
    fviz_dist(d,
              gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
      labs(fill = "Distancia") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 4),
            axis.text.y = element_text(size = 3))
    
    ggsave(file=paste0("03_Outputs/",path,"/04_Cluster/cluster_dist.png"), 
           height = h, width = w*0.8, dpi = 900)
  

  #### Step 01: Choosing best 
  # Agglomerative coefficient: which measures the amount of clustering structure found (values closer to 1 suggest strong clustering structure).
  ###==============
  
  # methods to assess
  m <- c( "average", "single", "complete", "ward")
  names(m) <- c( "average", "single", "complete", "ward")
  
  # function to compute coefficient
  ac <- function(x) {
    agnes(df, method = x)$ac
  }
  
  map_dbl(m, ac)
  
  
  #### Step 02: Optimal number of cluster
  ###==============
  

  wss <- numeric()
  for(h in 1:10){
    b<-kmeans(df,h)
    wss[h]<-b$tot.withinss #scintra
  }
  wss 
  
  wss1 <- data.frame(cluster=c(1:10),wss)
  
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
  
  
  #### Step 03: Dendogram
  ###==============
  
  # Ward's method
  hc5 <- hclust(d, method = "ward.D2")
  
  # 1. Corte inicial del árbol
  sub_grp_raw <- cutree(hc5, k = 3)
  
  # 2. Identificar solo los PC1 para determinar el orden (más es mejor)
  # Según el vector 'vars', son los que terminan exactamente en "1"
  cols_pc1 <- grep("1$", colnames(df), value = TRUE)
  
  # 3. Crear un ranking basado en el promedio de esos PC1
  # Usamos un dataframe temporal para no alterar el 'df' original
  df_ranking <- data.frame(
    old_cluster = sub_grp_raw,
    score_orden = rowMeans(df[, cols_pc1, drop = FALSE], na.rm = TRUE)
  )
  
  # 4. Calcular el desempeño promedio por cluster
  resumen_clusters <- df_ranking %>%
    group_by(old_cluster) %>%
    summarise(avg_score = mean(score_orden)) %>%
    arrange(desc(avg_score)) %>% 
    mutate(new_cluster_id = row_number())
  
  # 5. Mapear los viejos IDs a los nuevos IDs (1=Mejor, 3=Peor)
  map_clusters <- resumen_clusters$new_cluster_id
  names(map_clusters) <- resumen_clusters$old_cluster
  
  # 6. Reemplazar sub_grp con los valores ordenados
  sub_grp <- as.integer(map_clusters[as.character(sub_grp_raw)])
  
  #### Dendrogram
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
  
  
  #### By Component
  p_dendo_vars <- fviz_cluster(list(data = df, cluster = sub_grp),
                   #palette = c("#2E9FDF",  "#E7B800", "#FC4E07","#e4546c"),
                   palette = c("#2E9FDF",  "#E7B800", "#FC4E07"),
                   ellipse.type = "convex", # Concentration ellipse
                   repel = F, # Avoid label overplotting (slow)
                   show.clust.cent = FALSE, ggtheme = theme_minimal())
  ggsave(
    filename = paste0("03_Outputs/", path, "/04_Cluster/cluster_dendo_vars.png"),
    plot = p_dendo_vars,
    height = h * 0.8,
    width = w,
    dpi = 300
  )
 
  #### Step 04: Build Data
  ###=============
  
  datos.j <- as.data.frame(cbind(df, sub_grp))
  datos.j_aux <- tibble::rownames_to_column(as.data.frame(datos.j), var = "municipio")
  
  ### Caracterizando a los Clusters
  #### Step 05: Describiendo los datos
  ###==============
   ### Medias by cluster
    datos.j %>%   
      group_by(sub_grp) %>%
      summarise_all(list(mean)) -> medias
    medias
    
  ### Media total
  datos.j %>%  dplyr::select(-c(sub_grp)) %>% summarise_if(base::is.numeric,mean) %>%
    base::round(4) -> general
  general <- cbind(sub_grp="general",general)
  
    
    # Convirtiendo la data a formato tidy
    
    gathered_datos.j <- pivot_longer(data=medias,
                                     -sub_grp,
                                     names_to="variable",
                                     values_to = "valor")
    head(gathered_datos.j)
    
    ### Fix Variables
    gathered_datos.j$variable <- factor(gathered_datos.j$variable,
                                       levels=unique(gathered_datos.j$variable),
                                       labels=  lab)
  
    ### Fix Variables
    gathered_datos.j$sub_grp <- factor(gathered_datos.j$sub_grp, 
                                       levels=c(1,2,3),
                                      labels = lab_g)
 
    # --- Paleta dinámica según tipo de gráfico ---
    levs <- levels(gathered_datos.j$sub_grp)
    
    # Caso 1: cluster simple
    if (all(levs %in% names(base_palette))) {
      
      col_palette_named <- base_palette[levs]
      
    } else {
      
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
    
    ##### Variables de contexto
    ds_con_1 <- gathered_datos.j %>% filter((variable %in% c( "Acceso a servicio de agua potable y saneamiento - 1",
                                                             "Cambio climatico - 1",
                                                             "Cambio climatico - 2",
                                                             "Capacidad fiscal - 1",
                                                             "Capacidad fiscal - 2",
                                                             "Características de las viviendas - 1",
                                                             "Características de las viviendas - 2"
                                                            
    )))
    
    ggplot(data=ds_con_1, aes(x=variable,y=valor,fill=sub_grp)) +
      geom_bar(stat="identity", position=position_dodge())+
      #geom_text(aes(label=len), vjust=1.6, color="white",
      #          position = position_dodge(0.9), size=3.5)+
      scale_fill_manual(values = col_palette_named,
                        na.value = "#ededed", na.translate = FALSE)+
    #scale_fill_brewer(palette="Paired")+
      guides(fill = guide_legend(ncol = 3))+
      theme_minimal()+ylim(c(-2.5, 2.5))+
      theme(legend.position = c(0.7, 0.1),legend.title=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(y=eje,x="")
    
    
    ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_variables_contexto_1.png"),
           height = h*0.8, width = w, dpi = 900) 
    
    
    
    ##### Variables de contexto
    ds_con_2 <- gathered_datos.j %>% filter((variable %in% c( "Desarrollo económico - 1",
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
    
    ggplot(data=ds_con_2, aes(x=variable,y=valor,fill=sub_grp)) +
      geom_bar(stat="identity", position=position_dodge())+
      #geom_text(aes(label=len), vjust=1.6, color="white",
      #          position = position_dodge(0.9), size=3.5)+
      scale_fill_manual(values = col_palette_named,
                        na.value = "#ededed", na.translate = FALSE)+
      #scale_fill_brewer(palette="Paired")+
      guides(fill = guide_legend(ncol = 3))+
      theme_minimal()+ylim(c(-2.5, 2.5))+
      theme(legend.position = c(0.7, 0.1),legend.title=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(y=eje,x="")
    
    
    ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_variables_contexto_2.png"),
           height = h*0.8, width = w, dpi = 900) 
    
  
    # Variables de resultado
    ds_res_1 <- gathered_datos.j %>% filter((variable %in% c( "Infancia y Niñez - 1",
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
    
    
    
    ggplot(data=ds_res_1, aes(x=variable,y=valor,fill=sub_grp)) +
      geom_bar(stat="identity", position=position_dodge())+
      #geom_text(aes(label=len), vjust=1.6, color="white",
      #          position = position_dodge(0.9), size=3.5)+
      scale_fill_manual(values = col_palette_named,
                        na.value = "#ededed", na.translate = FALSE)+
      #scale_fill_brewer(palette="Paired")+
      guides(fill = guide_legend(ncol = 3))+
      theme_minimal()+ylim(c(-2.5, 2.5))+
      theme(legend.position = c(0.7, 0.1),legend.title=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(y=eje,x="")
    
   
    
    ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_variables_res_1.png"),
           height = h*0.8, width = w, dpi = 900) 
    
  ###===================
  ### 01 - Map Stylish
  #===
    
    w <- 4.5*2.5
    h <- 3.2*2.5
    text <- 15
    d <- 900
    s <- 1.2
    a_line <- 0.6
    a_dot <- 0.7
    
    theme <- list(
      theme_classic(base_size = text*1.5),
      theme(axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank()))
    
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
    
    
  #### Step 06: Map
  ###==============
    map_dptos <- st_read("01_Data/01_Derived/maps/municipios_antioquia.shp")
    #col_palette <- c("#9ADCFF","#FFF89A","#FFB2A6","#FF8AAE")
    
    
    ds_out <- cbind( pca_ds[,c("ind_mpio","nvl_label")],datos.j)
  
    data_map <- merge(map_dptos,ds_out,by.x="nivl_vl",by.y="ind_mpio")
    names(data_map)
    
    data_map <- merge(
      map_dptos, ds_out,
      by.x = "nivl_vl", by.y = "ind_mpio",
      all.x = TRUE,     
      sort = FALSE
    )
    
    
    # data_map <- data_map[data_map$nivl_vl == 88,]
    # Cambie la paleta
    data_map$sub_grp <- factor(data_map$sub_grp,
                               levels = c(1,2,3),
                               labels = lab_g)
    
    # --- Paleta según tipo ---
    levs <- levels(data_map$sub_grp)
    
    # Caso 1: cluster simple
    if (all(levs %in% names(base_palette))) {
      
      col_palette_named <- base_palette[levs]
      
    } else {
      
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
    
    # Calcular centroides reales de cada municipio
    data_map <- data_map %>%
      mutate(centroid = st_centroid(geometry),
             x_centroid = st_coordinates(centroid)[,1],
             y_centroid = st_coordinates(centroid)[,2])
    
    g1 <- ggplot(data_map) +
      geom_sf(aes(fill = factor(sub_grp)), color = "#606060", size = 0.05, alpha = 0.60) +
      geom_text_repel(aes(x_centroid, y_centroid, label = nvl_lbl),
                      size = 2,
                      color = "black",
                      alpha = 0.9,
                      segment.size = 0,
                      max.overlaps = Inf,
                      force = 2,
                      force_pull = 0.5,
                      box.padding = 0.3) +
      scale_fill_manual(values = col_palette_named,
                        na.value = "#ededed", na.translate = FALSE)+
      guides(fill = guide_legend(ncol = 1)) +
      xlab("") +
      labs(fill="") +
      map_theme +
      theme(legend.position = c(0.8, 0.1))
    
    
    ggsave(file=paste0("03_Outputs/", path, "/04_Cluster/cluster_map.png"), 
           g1, height = h*1.2, width = w*0.8, dpi = d)
    
    
    datos.j <- as.data.frame(cbind(df, sub_grp))
    datos.j_aux <- tibble::rownames_to_column(as.data.frame(datos.j), var = "municipio")

    ### Save
    data_map_aux <- st_drop_geometry(data_map)
    readr::write_csv( data_map_aux, paste0("03_Outputs/", path, "/04_Cluster/ds_cluster.csv"))
    write_rds(data_map_aux, paste0("03_Outputs/", path, "/04_Cluster/ds_cluster.rds"), "xz", compression = 9L)
    
} 