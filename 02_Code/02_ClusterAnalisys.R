rm(list=ls())
# For replicability
set.seed(1601)
source("02_Code/AUX_Functions_Clusters.R")

# Función para estandatizar las desviaciones de acuerdo al subgrupo o el departamento
standardize_df <- function(df_subset_raw, df_dept_raw,
                           ref = c("subset", "dept"),
                           scale_sd = c("dept", "subset")) {
  
  ref <- match.arg(ref)
  scale_sd <- match.arg(scale_sd)
  
  # SD para escalar
  sds <- if (scale_sd == "dept") apply(df_dept_raw, 2, sd, na.rm = TRUE)
  else apply(df_subset_raw, 2, sd, na.rm = TRUE)
  
  # Centro (media de referencia)
  center <- switch(
    ref,
    subset   = colMeans(df_subset_raw, na.rm = TRUE),
    dept     = colMeans(df_dept_raw, na.rm = TRUE)
  )
  
  out <- sweep(df_subset_raw, 2, center, "-")
  out <- sweep(out, 2, sds, "/")
  as.data.frame(out)
}

#----------------------------------------#
# Initial PCA Data Base ----
# ---------------------------------------#

pca_ds <- read_rds("01_Data/01_Derived/pca_ds_Total.rds")

vars <- c(
  "idx1_servicio1",
  "idx2_Adultez1","idx2_Adultez2","idx2_Adultez3",
  "idx3_climatico1","idx3_climatico2",
  "idx4_Capacidad1","idx4_Capacidad2",
  "idx5_viviendas1","idx5_viviendas2",
  "idx6_crecimiento1","idx6_crecimiento2","idx6_crecimiento3",
  "idx7_gini1",
  "idx8_demografica1",
  "idx9_infnin1","idx9_infnin2","idx9_infnin3","idx9_infnin4",
  "idx10_juventud1","idx10_juventud2","idx10_juventud3","idx10_juventud4",
  "idx11_pobreza1","idx11_pobreza2",
  "idx12_salud1","idx12_salud2",
  "idx13_saludmental1","idx13_saludmental2",
  "idx14_seguridad1","idx14_seguridad2",
  "idx15_vejez1"
)

#----------------------------------------#
# Clusters ----
# ---------------------------------------#
# Prepare Data
pca_ds_all <- pca_ds %>% dplyr::select(ind_mpio, nvl_label, dplyr::all_of(vars)) %>% tidyr::drop_na()
df_dept_raw <- pca_ds_all %>% dplyr::select(dplyr::all_of(vars))

# 1) referencia departamental (equivalente a scale sobre Antioquia)
df_all_dept <- standardize_df(df_dept_raw, df_dept_raw, ref="dept", scale_sd="dept")
rownames(df_all_dept) <- pca_ds_all$nvl_label
ps_cluster(pca_ds_all, df_all_dept, c("Consolidada","Transición","Vulnerable"), "all", "Desviaciones de la Media Departamental (std. desv.)")

#----------------------------------------#
# Subclusters ----
# ---------------------------------------#
base_clusters <- read_rds("03_Outputs/all/04_Cluster/ds_cluster.rds") %>%
  dplyr::mutate(ind_mpio = nivl_vl) 

dept_ref <- base_clusters %>% tidyr::drop_na() %>% dplyr::select(dplyr::all_of(vars))

for (i in c("Consolidada", "Transición", "Vulnerable")) {
  
  pca_sub <- base_clusters %>%
    dplyr::filter(sub_grp == i) %>%
    tidyr::drop_na()
  
  df_subset_raw <- pca_sub %>% dplyr::select(dplyr::all_of(vars))
  
  # labels para subcluster
  lab_g <- c(paste0(i," - Alto"), paste0(i," - Medio"), paste0(i," - Bajo"))
  
  # 1) referencia SUBSET (dentro del grupo)
  df_sub_subset <- standardize_df(df_subset_raw, dept_ref, ref="subset", scale_sd="subset")
  rownames(df_sub_subset) <- pca_sub$nvl_label
  ps_cluster(pca_sub, df_sub_subset, lab_g, paste0(i, "_ref_subset"), paste0("Desviaciones de la Media de Antioquia ", i, " (std. desv.)"))

  # 2) referencia DEPARTAMENTO
  df_sub_dept <- standardize_df(df_subset_raw, dept_ref, ref="dept", scale_sd="dept")
  rownames(df_sub_dept) <- pca_sub$nvl_label
  ps_cluster(pca_sub, df_sub_dept, lab_g, paste0(i, "_ref_dept"), "Desviaciones de la Media Departamental (std. desv.)")
}

