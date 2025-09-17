rm(list=ls())
# For replicability
set.seed(1601)
source("02_Code/AUX_Functions_Clusters.R")


##### Initial PCA Data Base
#==============================
pca_ds <- read_rds("01_Data/01_Derived/pca_ds_Total.rds") 
  
  df <- pca_ds %>% dplyr::select(
                  idx1_servicio1,
                  idx2_Adultez1,idx2_Adultez2,idx2_Adultez3,
                  idx3_climatico1,idx3_climatico2,
                  idx4_Capacidad1,idx4_Capacidad2,
                  idx5_viviendas1,idx5_viviendas2,
                  idx6_crecimiento1, idx6_crecimiento2, idx6_crecimiento3,
                  idx7_gini1,
                  idx8_demografica1,
                  idx9_infnin1, idx9_infnin2, idx9_infnin3, idx9_infnin4,
                  idx10_juventud1, idx10_juventud2,idx10_juventud3, idx10_juventud4, 
                  idx11_pobreza1, idx11_pobreza2,
                  idx12_salud1,idx12_salud2,
                  idx13_saludmental1, idx13_saludmental2,
                  idx14_seguridad1,idx14_seguridad2,
                  idx15_vejez1) 
  # Prepare Data
  df <- na.omit(df)
  df <- scale(df)
  rownames(df) <- pca_ds$nvl_label
  df<- as.data.frame(df)

ps_cluster(pca_ds,df,c("Consolidada","Transición","Vulnerable"),"all")

##### Por parte -- Vulnerable

for (i in c("Consolidada","Transición","Vulnerable")) {

  vul <- read_rds("03_Outputs/all/04_Cluster/ds_cluster.rds") %>% 
          filter(sub_grp==i)  %>% mutate(ind_mpio=nivl_vl)

  df <- na.omit(vul) %>% select(-sub_grp,-(nivl_vl:nvl_label),-ind_mpio)
  df <- scale(df)
  rownames(df) <- vul$nvl_label
  df<- as.data.frame(df)

  ps_cluster(vul,df,c(paste0(i," - Alto"),paste0(i," - Medio"),paste0(i," - Bajo")),i)

}
