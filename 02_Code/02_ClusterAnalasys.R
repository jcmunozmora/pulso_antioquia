rm(list=ls())
# For replicability
set.seed(1601)
source("02_Code/AUX_Functions_Clusters.R")
g_out <- "04_Cluster_Outputs/Total"
tx<- "04_Cluster_Outputs/Total"
library(stargazer)
library(openxlsx)
library(writexl)
library(igraph)
library(dplyr)

##### Initial PCA Data Base
#==============================
pca_ds <- read_rds("01_Data/01_Derived/pca_ds_Total.rds") 
  
  df <- pca_ds %>% dplyr::select(
                  idx1_servicio1,idx1_servicio2,idx1_servicio3,
                  idx2_Adultez1,idx2_Adultez2,idx2_Adultez3,
                  idx3_climatico1,idx3_climatico2,
                  idx4_Capacidad1,idx4_Capacidad2,
                  idx5_viviendas1,idx5_viviendas2,idx5_viviendas3,
                  idx6_crecimiento1, idx6_crecimiento2, idx6_crecimiento3, idx6_crecimiento4,idx6_crecimiento5,
                  idx7_gini1,
                  idx8_demografica1,idx8_demografica2,
                  idx9_infnin1, idx9_infnin2, idx9_infnin3, idx9_infnin4,
                  idx10_juventud1, idx10_juventud2,idx10_juventud3, idx10_juventud4, idx10_juventud5,
                  idx11_pobreza1, idx11_pobreza2,
                  idx12_salud1,
                  idx13_saludmental1, idx13_saludmental2,
                  idx14_seguridad1,idx14_seguridad2,
                  idx15_vejez1) 
  # Prepare Data
  df <- na.omit(df)
  df <- scale(df)
  rownames(df) <- pca_ds$nvl_label
  df<- as.data.frame(df)
  
  ### Descriptive
  res <- stargazer(as.data.frame(df[,setdiff(names(df),c("ind_dpto","nvl_label"))]),
                   font.size = "small",
                   align = TRUE,
                   covariate.labels=lab,
                   summary.stat = c("n", "mean", "sd","min","max"))

  res <- res[12:length(res)-2]

  cat(res, file=glue("04_Cluster_Outputs/Total/cluster_desc.tex", sep="\n"))
  cat(res, file=glue("04_Cluster_Outputs/Total/cluster_desc.tex", sep="\n"))

  

##### Distance Matrix
#==============================

  ### Prepare our data
  d <- dist(df, method = "euclidean")
  
  fviz_dist(d,
            gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(fill = "Distancia") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 4),
          axis.text.y = element_text(size = 4))
  
  ggsave(file=paste0("04_Cluster_Outputs/Total/cluster_dist.pdf"), 
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

library(factoextra)
set.seed(123)
wss <- numeric()
for(h in 1:10){
  b<-kmeans(df,h)
  wss[h]<-b$tot.withinss #scintra
}
wss 

wss1 <- data.frame(cluster=c(1:10),wss)

library(ggplot2)
ggplot(wss1) + aes(cluster,wss) + geom_line(color="blue") + 
  geom_point(color="blue") +
  geom_vline(xintercept = 3, linetype = 2, col="red") +
  labs(title = "Método Elbow") + 
  scale_x_continuous(breaks=1:10) +
  theme_classic()

ggsave(file=paste0("04_Cluster_Outputs/Total/cluster_elbow.pdf"),
       height = h*0.8, width = w, dpi = 900) 

#### Step 03: Dendogram
###==============

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 3)


#### Dendograma
pdf(file = "04_Cluster_Outputs/Total/cluster_dendo.pdf", height = h*0.8, width = w) 
fviz_dend(hc5, cex = 0.5, k = 3, rect = TRUE)
dev.off()

#### By Component
pdf(file = "04_Cluster_Outputs/Total/cluster_dendo_vars.pdf", height = h*0.8, width = w) 
fviz_cluster(list(data = df, cluster = sub_grp),
             #palette = c("#2E9FDF",  "#E7B800", "#FC4E07","#e4546c"),
             palette = c("#2E9FDF", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = F, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())
dev.off()

pdf(file = "04_Cluster_Outputs/Total/cluster_dendo_phylogenic.pdf", height = h*0.8, width = w) 
fviz_dend(hc5,k=3,k_colors="jco",
          type="phylogenic", repel=TRUE)
dev.off()

#### Step 04: Build Data
###==============

df <- pca_ds %>% dplyr::select(
  idx1_servicio1,idx1_servicio2,idx1_servicio3,
  idx2_Adultez1,idx2_Adultez2,idx2_Adultez3,
  idx3_climatico1,idx3_climatico2,
  idx4_Capacidad1,idx4_Capacidad2,
  idx5_viviendas1,idx5_viviendas2,idx5_viviendas3,
  idx6_crecimiento1, idx6_crecimiento2,idx6_crecimiento3, idx6_crecimiento4,idx6_crecimiento5,
  idx7_gini1,
  idx8_demografica1,idx8_demografica2,
  idx9_infnin1, idx9_infnin2, idx9_infnin3, idx9_infnin4,
  idx10_juventud1, idx10_juventud2, idx10_juventud3, idx10_juventud4, idx10_juventud5,
  idx11_pobreza1, idx11_pobreza2,
  idx12_salud1,
  idx13_saludmental1, idx13_saludmental2,
  idx14_seguridad1,idx14_seguridad2,
  idx15_vejez1
)

df <- na.omit(df)
df <- scale(df)
rownames(df) <- pca_ds$nvl_label

datos.j <- as.data.frame(cbind(df, sub_grp))
datos.j_aux <- tibble::rownames_to_column(as.data.frame(datos.j), var = "municipio")
write.csv(datos.j_aux, "04_Cluster_Outputs/Total/datos_j.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")

### Caracterizando a los Clusters
#### Step 05: Describiendo los datos
###==============
### Medias by cluster
  #datos.j %>%   
  #  group_by(sub_grp) %>%
  #  summarise_all(list(mean)) -> medias
  #medias


  ### Media total
  datos.j %>%  dplyr::select(-c(sub_grp)) %>% summarise_if(base::is.numeric,mean) %>%
    base::round(4) -> general
  general <- cbind(sub_grp="general",general)
  
  # Medellin 
  datos.j <- rownames_to_column(datos.j, var = "municipio")

  ref_med <- datos.j %>%
    filter(str_detect(municipio, regex("^medell[ií]n$", ignore_case = TRUE))) %>%
    summarise(across(where(base::is.numeric), ~ mean(.x, na.rm = TRUE)))
  
  stopifnot(nrow(ref_med) == 1)
  
  # Medias por grupo
  medias <- datos.j %>%
    group_by(sub_grp) %>%
    summarise(across(where(base::is.numeric), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop")
  
  #centrar en Medellín 
  num_vars <- names(medias)[vapply(medias, base::is.numeric, TRUE)]
  medias_vs_med <- medias
  medias_vs_med[num_vars] <- sweep(medias[num_vars], 2,
                                   base::as.numeric(ref_med[1, num_vars]), "-")
  datos_vs_med <- medias_vs_med
  datos_vs_med$sub_grp <- datos_vs_med$sub_grp + 1

  
  gathered_datos.j <- pivot_longer(
    data = datos_vs_med,
    #data = medias,
    -sub_grp,
    names_to = "variable",
    values_to = "valor"
  )
  
  ### Merge
  #medias  <- as.data.frame(rbind(medias,general))
  
  # Convirtiendo la data a formato tidy
  gathered_datos.j <- pivot_longer(data=datos_vs_med,
                                   - sub_grp,
                                   names_to="variable",
                                   values_to = "valor")
  head(gathered_datos.j)
  
  ### Fix Variables
  gathered_datos.j$variable <- factor(gathered_datos.j$variable,
                                     levels=unique(gathered_datos.j$variable),
                                     labels=  c("Acceso a servicio de agua potable y saneamiento - 1",
                                                "Acceso a servicio de agua potable y saneamiento - 2",
                                                "Acceso a servicio de agua potable y saneamiento - 3",
                                                "Adultez - 1",
                                                "Adultez - 2",
                                                "Adultez - 3",
                                                "Cambio climatico - 1",
                                                "Cambio climatico - 2",
                                                "Capacidad fiscal - 1",
                                                "Capacidad fiscal - 2",
                                                "Características de las viviendas - 1",
                                                "Características de las viviendas - 2",
                                                "Características de las viviendas - 3",
                                                "Desarrollo económico - 1",
                                                "Desarrollo económico - 2",
                                                "Desarrollo económico - 3",
                                                "Desarrollo económico - 4",
                                                "Desarrollo económico - 5",
                                                "Desigualdad - 1",
                                                "Estructura demográfica - 1",
                                                "Estructura demográfica - 2",
                                                "Infancia y Niñez - 1",
                                                "Infancia y Niñez - 2",
                                                "Infancia y Niñez - 3",
                                                "Infancia y Niñez - 4",
                                                "Juventud - 1",
                                                "Juventud - 2",
                                                "Juventud - 3",
                                                "Juventud - 4",
                                                "Juventud - 5",
                                                "Pobreza - 1",
                                                "Pobreza - 2",
                                                "Salud - 1",
                                                "Salud mental - 1",
                                                "Salud mental - 2",
                                                "Seguridad - 1",
                                                "Seguridad - 2",
                                                "Vejez - 1"
                                                ))

  ### Fix Variables
  col_palette <- c("#fcac24","#e4546c","#55E459")
  gathered_datos.j$sub_grp <- factor(gathered_datos.j$sub_grp, 
                        
                                     levels=c(1,2,3),
                                     labels = c("Consolidada","Transición","Vulnerable"))
                                     
  ##### Variables de contexto
  ds_con_1 <- gathered_datos.j %>% filter((variable %in% c( "Acceso a servicio de agua potable y saneamiento - 1",
                                                           "Acceso a servicio de agua potable y saneamiento - 2",
                                                           "Acceso a servicio de agua potable y saneamiento - 3",
                                                           "Cambio climatico - 1",
                                                           "Cambio climatico - 2",
                                                           "Capacidad fiscal - 1",
                                                           "Capacidad fiscal - 2",
                                                           "Características de las viviendas - 1",
                                                           "Características de las viviendas - 2",
                                                           "Características de las viviendas - 3"
                                                          
  )))
  
  ggplot(data=ds_con_1, aes(x=variable,y=valor,fill=sub_grp)) +
    geom_bar(stat="identity", position=position_dodge())+
    #geom_text(aes(label=len), vjust=1.6, color="white",
    #          position = position_dodge(0.9), size=3.5)+
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
    #scale_fill_brewer(palette="Paired")+
    guides(fill = guide_legend(ncol = 3))+
    theme_minimal()+ylim(-5,5)+
    theme(legend.position = c(0.7, 0.1),legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y="Desviaciones de la Media Nacional (std. desv.)",x="")
  

  ggsave(file=paste0("04_Cluster_Outputs/Total/cluster_variables_contexto_1.pdf"),
         height = h*0.8, width = w, dpi = 900) 

  
  
  ##### Variables de contexto
  ds_con_2 <- gathered_datos.j %>% filter((variable %in% c( "Desarrollo económico - 1",
                                                            "Desarrollo económico - 2",
                                                            "Desarrollo económico - 3",
                                                            "Desarrollo económico - 4",
                                                            "Desarrollo económico - 5",
                                                            "Desigualdad - 1",
                                                            "Estructura demográfica - 1",
                                                            "Estructura demográfica - 2",
                                                            "Pobreza - 1",
                                                            "Pobreza - 2",
                                                            "Salud - 1",
                                                            "Salud mental - 1",
                                                            "Salud mental - 2",
                                                            "Seguridad - 1",
                                                            "Seguridad - 2"
                                                            
  )))
  
  ggplot(data=ds_con_2, aes(x=variable,y=valor,fill=sub_grp)) +
    geom_bar(stat="identity", position=position_dodge())+
    #geom_text(aes(label=len), vjust=1.6, color="white",
    #          position = position_dodge(0.9), size=3.5)+
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
    #scale_fill_brewer(palette="Paired")+
    guides(fill = guide_legend(ncol = 3))+
    theme_minimal()+ylim(-5,5)+
    theme(legend.position = c(0.7, 0.1),legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y="Desviaciones de la Media Nacional (std. desv.)",x="")
  
  ggsave(file=paste0("04_Cluster_Outputs/Total/cluster_variables_contexto_2.pdf"),
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
                                                            "Juventud - 5",
                                                            "Adultez - 1",
                                                            "Adultez - 2",
                                                            "Adultez - 3",
                                                            "Vejez - 1"
  )))
  
  
  
  ggplot(data=ds_res_1, aes(x=variable,y=valor,fill=sub_grp)) +
    geom_bar(stat="identity", position=position_dodge())+
    #geom_text(aes(label=len), vjust=1.6, color="white",
    #          position = position_dodge(0.9), size=3.5)+
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
    #scale_fill_brewer(palette="Paired")+
    guides(fill = guide_legend(ncol = 3))+
    theme_minimal()+ylim(-5,5)+
    theme(legend.position = c(0.7, 0.1),legend.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(y="Desviaciones de la Media Nacional (std. desv.)",x="")
  
  ggsave(file=paste0("04_Cluster_Outputs/Total/cluster_variables_res_1.pdf"),
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
  data_map_aux <- st_drop_geometry(data_map)
  readr::write_csv( data_map_aux, "04_Cluster_Outputs/Total/datos_map.csv")
  
  
  
#  data_map <- data_map[data_map$nivl_vl == 88,]
  # Cambie la paleta
  #col_palette <- c("#55E459","#4C9CCC","#FCAC24","#E4546C")
  col_palette <- c("#55E459","#4C9CCC","#FCAC24")
 
  data_map$sub_grp <- factor(data_map$sub_grp,levels=c(1,2,3),labels = c("Consolidada","Transición","Vulnerable"))
  
  g1 <- ggplot(data_map) +
    geom_sf(aes(fill = factor(sub_grp)), color = "#606060", size = 0.05, alpha = 0.60) +
    geom_text(aes(X, Y, label = nvl_lbl), vjust = 1.5, color = "black",
              position = position_dodge(0.9), size = 2,alpha = 1) +
    scale_fill_manual(values =  rep(col_palette, 40), na.value = "#ededed", na.translate = F) +
    guides(fill = guide_legend(ncol = 1)) +
    xlab("") +
    labs(fill="") +
   map_theme +
    theme_map(base_size = 12)+
    theme(legend.position = c(0.8, 0.1),
          plot.caption = element_text(color = "blue", 
                                      face = "italic",size=5))
  
  
  ggsave(file=paste0("04_Cluster_Outputs/Total/cluster_map.pdf"), 
         g1,height = h, width = w*0.8, dpi = d)