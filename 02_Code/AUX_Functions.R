rm(list = ls())
# Aux Funciones
# ------------------------------------------------------------------------------
# File: AUX_Functions.R
# Created by: Laura Quintero & Juan Carlos Muñoz
# Last modified: 2024-06-10
# Description: Auxiliary functions for data analysis and visualization in Antioquias project.
# ------------------------------------------------------------------------------

#### 00 - Loading packages
#==============================
# Load required packages using pacman::p_load for easier management
library(haven)
library(tidyr)
library(dplyr)
library(stringi)
library(writexl)
library(readxl)
library(tidymodels)
library(ggrepel)
library(stringr)
tidymodels_prefer()
library(tidyverse) # Basic data management
library(bestNormalize) ### Important package to normalize
library(embed) # load the library to use `step_umap()`  
library(ggpubr) # Nice graphs
library(gridExtra) ## Better graphs
library(ggforce) ### Force images
#devtools::install_github("tidymodels/learntidymodels")
library(learntidymodels) ## Graphs for PCA
#library(PulsoSocialColombia)
library(sf)
library(colorspace)
library(ggthemes)
library(stargazer) # Tables
library(glue)

tidymodels_prefer() # Prefer tidymodels functions over base R


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


###===================
### 01 - Plot numbers 
#====================

plot_top_loadings_jc <- function(v,label,x, ..., n = 4, id = NULL, type = "pca") {
  comp_vals <- get_loading_data(x, ..., id = id, type = type)
  
  comp_vals <-
    comp_vals %>%
    dplyr::mutate(
      value=value*v,
      `Positivo?` = value > 0,
      abs_value = abs(value)
    )%>%
    dplyr::group_by(component) %>%
    dplyr::slice_max(abs_value, n = n) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(component, abs_value) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::left_join(label,by="terms")
  
  ggplot2::ggplot(comp_vals, ggplot2::aes(x = order, y = abs_value, fill = `Positivo?`)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap( vars(component), scales = "free_y") +
    ggplot2::scale_x_continuous(
      breaks = comp_vals$order,
      labels = comp_vals$lab,
      expand = c(0,0)
    )  +
    ggplot2::labs(x = NULL, y = "Abs. Coefficient Value")
  
}


###===================
### 01 - Map
#=====================
map_mpios <- st_read("01_Data/01_Derived/maps/municipios_antioquia.shp")
col_palette <- c("#9ADCFF","#FFF89A","#FFB2A6","#FF8AAE")



mk_map <- function(data_map,var,ann){
  
  ### Var
  dat <- data_map[,c(var,"nvl_lbl","X","Y")]
  colnames(dat) <- c("var","nvl_lbl","X","Y","geometry")
  dat <- dat %>%
    mutate(tertiles = ntile(var, 4)) %>%
    mutate(tertiles = if_else(tertiles == 1, '1 Cuartil',
                              if_else(tertiles == 2, '2 Cuartil',
                                      ifelse(tertiles==3,'3 Cuartil','4 Cuartil')))) %>%
    arrange(var)
  
  g1<- ggplot(dat) +
    geom_sf(aes(fill = tertiles),
            color = "#606060", size = 0.05, alpha = 0.65) +
    geom_text(aes(X, Y, label = nvl_lbl), vjust = 1.5,  size = 2.5,  color = "black",
              position = position_dodge(0.9), 
              size = 2.5,alpha = 1, color = "black") +
    scale_color_manual(values =  col_palette, 
                       na.value = "#ededed", na.translate = F) +
    guides(fill = guide_legend(ncol = 2)) +
    xlab("") +
    #labs(fill="",caption = ann) +
    labs(fill="",caption = NULL) +
    theme + map_theme+
    theme_map(base_size = 12)+
    theme(legend.position = c(0.8, 0.2),
          plot.caption = element_text(color = "blue", 
                                      face = "italic",size=7))
  
  
  return(g1)
  }



###===================
### 01 - PIVOT + FILTER
#=====================
get_subdimension_data <- function(data, subdimension_) {
  data_filtered <- data %>%
    filter(Subdimension == subdimension_) %>%
    select(-Subdimension)
  # 2. Pivotear la data
  data_values <- data_filtered %>%
    select(ind_mpio, nvl_label, var0, value) %>%
    pivot_wider(names_from = var0, values_from = value)
  return(data_values)
}

### =================
### 02- PCA 
### =================

reduc_dim <- function(ds_raw,trans,label,nm,path) {

###
dir.create(file.path("03_Outputs", path), recursive = TRUE, showWarnings = FALSE)

#### 00 - Normalize the data
  # Ensure all required output directories exist
  output_dirs <- c(
    file.path("03_Outputs", path),
    file.path("03_Outputs", path, "01_TablasDescriptivas"),
    file.path("03_Outputs", path, "02_Imagenes")
    #file.path("03_Outputs", path, "03_DS")
  )
  for (dir in output_dirs) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  dp <- ds_raw[rowSums(is.na(ds_raw))>1,]$nvl_label 
  
  #if (length(dp)>0) {
  #  ann <- str_wrap(paste0("Algunos de los datos de los siguientes departamentos fueron imputados por falta de datos: ",paste(dp,collapse=", ")),300)
  #} else {
    ann <- ""
  #}
  
  ###
  ### 00 a- Sumary stats
  ###================================
  lb <- label[label$terms %in% names(ds_raw),]
  vars <- lb$terms
  df_num <- ds_raw[,vars]
  df_num <- df_num[,sapply(df_num, is.numeric), drop=FALSE] # Solo numéricas
  if (ncol(df_num) == 0) {

    message("No hay variables numéricas válidas para mostrar en stargazer. No se genera tabla descriptiva.")
    res <- NULL
  } else {
    # Filtrar columnas que tengan al menos un valor no NA
    valid_cols <- names(df_num)[colSums(!is.na(df_num)) > 0]
    df_num_valid <- df_num[, valid_cols, drop=FALSE]
    # Exportar datos con etiquetas para Quarto
    summary_export <- data.frame(
      Variable = names(df_num_valid),
      Etiqueta = lb$lab[match(names(df_num_valid), lb$terms)],
      N = sapply(df_num_valid, function(x) sum(!is.na(x))),
      Media = sapply(df_num_valid, mean, na.rm = TRUE),
      SD = sapply(df_num_valid, sd, na.rm = TRUE),
      Min = sapply(df_num_valid, min, na.rm = TRUE),
      Max = sapply(df_num_valid, max, na.rm = TRUE)
    )
    write_xlsx(summary_export, path = glue("03_Outputs/",path,"/01_TablasDescriptivas/", nm, "_summary_labels.xlsx"))
    
    # Exportar datos con etiquetas para Quarto
    if (ncol(df_num_valid) == 0) {
      message("Las variables numéricas existen pero todas son NA. No se genera tabla descriptiva.")
      res <- NULL
    } else {
      idx_lab <- match(names(df_num_valid), lb$terms)
  res <- stargazer(df_num_valid,
           font.size = "small",
           align = TRUE,
           covariate.labels=lb$lab[idx_lab],
           summary.stat = c("n", "mean", "sd","min","max"),
           na.omit = TRUE)
      res <- res[12:length(res)-2]
      # Solo guardar si hay filas de variables en la tabla
      if (length(res) > 0 && any(grepl("Statistic", res))) {
        cat(res, file=glue("03_Outputs/",path,"/01_TablasDescriptivas/",nm,".tex", sep="\n"))
      } else {
        message("La tabla generada por stargazer está vacía. No se guarda el archivo.")
      }
    }
  }
  
  res_tbl <- data.frame(Variable = lb$lab,
                        N = sapply(ds_raw[,lb$terms], function(x) sum(!is.na(x))),
                        Mean = sapply(ds_raw[,lb$terms], mean, na.rm = TRUE),
                        SD = sapply(ds_raw[,lb$terms], sd, na.rm = TRUE),
                        Min = sapply(ds_raw[,lb$terms], min, na.rm = TRUE),
                        Max = sapply(ds_raw[,lb$terms], max, na.rm = TRUE))
  write_xlsx(res_tbl, path = glue("03_Outputs/",path,"/01_TablasDescriptivas/", nm, ".xlsx"))
  

  ### Normalize Data
  ds_prep <- recipe(~ ., 
                    data = ds_raw) %>%
    update_role(ind_mpio,nvl_label,new_role = "id") %>%
    ### Step 01 - Delete variables single value
    step_zv(all_numeric_predictors()) %>%
    ### Step 02 - Imputation Data 
    #step_naomit(all_numeric_predictors()) %>%
    #step_impute_knn(all_predictors(),neighbors = 6) %>%
    #step_impute_knn(all_predictors(),neighbors = 7) %>%
    #step_impute_bag(all_predictors() , trees = 2000) %>%
    #step_impute_lower(all_numeric_predictors()) %>%
    #step_impute_mean(all_numeric_predictors()) %>%
    step_impute_median(all_numeric_predictors())  %>%
    #step_impute_linear(all_numeric_predictors()) %>%
    ### Step 03 - using the ORQ (orderNorm) transformation, 
    ### which approximatesthe "true" 
    ### normalizing tranformation if one exists
    
    # 
  ### Step 04 - will normalize numeric data to have a 
  ### standard deviation of one and a mean of zero.
  step_normalize(all_numeric_predictors()) 
  
  # test <- bake(prep(ds_prep),new_data=NULL)
  
  #### 01 - Dimensionality Methods
  ###================================
  n_pred <- ds_raw %>%
    dplyr::select(-ind_mpio, -nvl_label) %>%
    dplyr::select(where(base::is.numeric)) %>% ncol()
  
  #### Principal Component Analysis
  pca_prep <- ds_prep %>%
    step_pca(all_numeric_predictors(),
             num_comp = min(5, n_pred), id = "pca") %>%
    prep(verbose = TRUE)
  
  #### % Varianza Explicado
  pca_bake <- bake(pca_prep, ds_raw)
  
  
  # ====== BLOQUE COMPLETO  ======

  
  # Etiquetas "bonitas" desde tu tabla 'label' (si la tienes)
  lb_ <- label %>%
    dplyr::filter(terms %in% names(ds_raw)) %>%
    dplyr::select(terms, lab)
  
  # 1) LOADINGS por variable y componente (desde el step_pca)
  pca_loadings <- recipes::tidy(pca_prep, id = "pca") %>%   # cols: terms, component, value
    dplyr::transmute(variable = terms, component, loading = value)

  # Agregar peso relativo y porcentaje
  pca_loadings <- pca_loadings %>%
    dplyr::group_by(component) %>%
    dplyr::mutate(
      peso_relativo = abs(loading) / sum(abs(loading)),
      peso_relativo_pct = round(100 * peso_relativo, 1)
    ) %>%
    dplyr::ungroup()
  
  
  if (trans == 1) {
    pca_loadings <- pca_loadings %>% dplyr::mutate(loading = -loading)
  }
  

  pca_loadings <- pca_loadings %>%
    dplyr::left_join(lb_, by = c("variable" = "terms")) %>%
    dplyr::mutate(variable = ifelse(!is.na(lab), lab, variable)) %>%
    dplyr::select(-lab)
  
  # 2) VARIANZA EXPLICADA por componente 
  which_pca <- which(sapply(
    pca_prep$steps,
    function(x) base::`&&`(inherits(x, "step_pca"), x$id == "pca")
  ))[1]
  stopifnot(length(which_pca) == 1)
  
  eig      <- pca_prep$steps[[which_pca]]$res$sdev^2
  var_prop <- eig / sum(eig)
  
  pca_var <- tibble::tibble(
    component = paste0("PC", seq_along(var_prop)),
    percent   = var_prop   # proporción 0–1
  )
  
  # 3) TOP variables que MÁS pesan en PC1 y PC2 (por |loading|)
  top_PC1 <- pca_loadings %>%
    dplyr::filter(component == "PC1") %>%
    dplyr::mutate(sign = ifelse(loading >= 0, "+", "-"),
                  abs_loading = abs(loading)) %>%
    dplyr::arrange(dplyr::desc(abs_loading)) %>%
    dplyr::select(variable, loading, sign) %>%
    head(10)
  
  top_PC2 <- pca_loadings %>%
    dplyr::filter(component == "PC2") %>%
    dplyr::mutate(sign = ifelse(loading >= 0, "+", "-"),
                  abs_loading = abs(loading)) %>%
    dplyr::arrange(dplyr::desc(abs_loading)) %>%
    dplyr::select(variable, loading, sign) %>%
    head(10)
  
  # Guardar tablas a Excel 
  writexl::write_xlsx(
    list(loadings = pca_loadings,
         variance = pca_var,
         top_PC1 = top_PC1,
         top_PC2 = top_PC2),
    path = glue::glue("03_Outputs/",path,"/{nm}_loadings_y_tops.xlsx")
  )
  
  # 4) TELARAÑAS: una imagen por componente (hasta K = los definidos en step_pca)
  K <- min(5, n_pred)   # igual que num_comp = min(5, n_pred)
  for (k in seq_len(K)) {
    g <- plot_radar_pc_tbl(pca_loadings, pca_var, comp = k)
    ggsave(glue::glue("03_Outputs/",path,"/{nm}_radar_PC{k}.png"),
           g, width = 8, height = 6, dpi = 300)
  }

  
  if (trans == 1) {
    pcs_present <- intersect(paste0("PC", 1:5), names(pca_bake))
    pca_bake <- pca_bake %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(pcs_present), ~ .x * -1))
    v <- -1
  } else {
    v <- 1
  }

  
  #### Uniform manifold approximation and projection (UMAP)
  
  ds_umap <- ds_prep %>%
    ### Step 05 - 
    step_umap(all_predictors()) %>%
    ### Prepare - Booking
    prep(verbose = TRUE)
  
  umap_bake <- bake(ds_umap, ds_raw)
  
  #### 02 - Compare Methods
  ###================================
  
  g1 <- umap_bake %>%
    ggplot(aes(UMAP1, UMAP2, label=nvl_label)) +
    geom_point(aes(color = nvl_label), alpha = 0.7, size = 2)+
    geom_text(check_overlap = TRUE, hjust = "inward") +
    labs(color = NULL) +
    theme_minimal()+ theme(legend.position = "none")
  
  g2 <- pca_bake %>%
    ggplot(aes(PC1, PC2, label=nvl_label)) +
    geom_point(aes(color = nvl_label), alpha = 0.7, size = 2)+
    geom_text(check_overlap = TRUE, hjust = "inward",size=3) +
    labs(color = NULL,
         caption = ann) +
    theme_minimal()+ theme(legend.position = "none",
                           plot.caption = element_text(color = "blue", face = "italic",
                                                       size=5))
  
  #### 02 - PCA
  ###================================
  
  pca_variances <- tidy(pca_prep, id = "pca", type = "variance")
  write_xlsx(pca_variances, path = glue("03_Outputs/",path,"/01_TablasDescriptivas/", nm, "_vr.xlsx"))
  
  g1_pca <- pca_variances %>%
    filter(terms == "cumulative percent variance") %>%
    filter(component<4) %>%
    ggplot(aes(component, value)) +
    geom_col(fill="#372F60")+
    labs(x = "PC", 
         y = "Varianza Explicada Acumulada (%)") +
    theme_minimal()+theme(legend.position = "none")+
    geom_hline(yintercept=c(60), linetype="dotted") +
    annotate(geom="text", x=4, y=63, label="60%",
             color="red")
  
  g2_pca <- pca_prep %>%
    plot_top_loadings_jc(v,label,.,component_number <= 2, n = 5) + 
    scale_fill_brewer(palette = "Paired") +
    ggtitle("")+ 
    guides(fill = guide_legend(nrow = 1))+
    theme(legend.position = "none" ,
          axis.text.y = element_text(color = "grey20", 
                                     size = 5,  hjust = .5, 
                                     vjust = .5, face = "plain"),
          legend.text=element_text(size=5),
          legend.title=element_text(size=6),
          axis.text.x=element_text(size=6)) +
    labs(y = "")+labs(color = NULL,
                      caption = "Las barras oscuras denotan variables con un peso positivo en el índice, \n
                      mientras que las barras azul claras denotan variables con pesos negativos.")+
    theme(legend.position = "none",
          plot.caption = element_text(color = "blue", face = "italic",
                                      size=5))
  
  
  
  grid.arrange(g2,g1_pca,g2_pca, 
               ncol=2,
               layout_matrix = cbind(c(1,2), c(1,3))
  )
  
  #save
  g <- arrangeGrob(g2,g1_pca,g2_pca, ncol=2,
                   layout_matrix = cbind(c(1,2), c(1,3)))
  ggsave(file=paste0("03_Outputs/",path,"/02_Imagenes/",nm,"_results.png"), g,
         height = h*0.8, width = w, dpi = d) 
  
  
  #### 03 - Get the pca
  ###================================
  # Emparejar datos y mapas
  data_map <- merge(map_mpios,pca_bake,by.x="nivl_vl",by.y="ind_mpio",all.x=TRUE)
  names(data_map)
  
  data_map <- data_map[data_map$nivl_vl != 88,]
  
  mp1 <- mk_map(data_map,"PC1",ann)
  mp2 <- mk_map(data_map,"PC2",ann)
  
  ggsave(file=paste0("03_Outputs/",path,"/02_Imagenes/",nm,"_map_pc1.png"), 
         mp1,height = h, width = w*0.8, dpi = d)
  
  ggsave(file=paste0("03_Outputs/",path,"/02_Imagenes/",nm,"_map_pc2.png"), 
         mp2 ,height = h, width = w*0.8, dpi = d)
  
  g <- arrangeGrob(mp1,mp2, ncol=2)
  
  ggsave(file=paste0("03_Outputs/",path,"/02_Imagenes/",nm,"_map_pcall.png"), 
         g ,height = h*2, width = w*2*0.8, dpi = d)
  
  
  #### 04 - Final
  ###================================
  k <- sum(grepl("^PC\\d+$", names(pca_bake)))   # cuántas PCs hay realmente
  names(pca_bake) <- c("ind_mpio", "nvl_label", paste0(nm, seq_len(k)))
  
  
  return(pca_bake)
  
}

 ### 05 - Telaraña
  plot_radar_pc_tbl <- function(load_tbl, var_tbl, comp = 1, order_by = c("magnitude","name","given")) {
    order_by <- match.arg(order_by)
    comp_name <- paste0("PC", comp)
    
    d <- load_tbl |> dplyr::filter(component == comp_name)
    
    # Orden de ejes (elige uno)
    d <- switch(order_by,
                magnitude = d |> dplyr::arrange(dplyr::desc(abs(loading))),
                name      = d |> dplyr::arrange(variable),
                given     = d
    )
    
    # Escala radial: 0.5 = loading 0; ±M en el borde
    M <- max(abs(d$loading), na.rm = TRUE)
    r <- 0.5 + 0.5 * d$loading / M
    
    n <- nrow(d)
    theta0 <- pi/2                      # opcional: pone el primer eje "arriba"
    theta  <- theta0 - 2*pi*(0:(n-1))/n # ángulos equiespaciados
    
    poly <- d |>
      dplyr::mutate(r = r, theta = theta,
                    x = r*cos(theta), y = r*sin(theta),
                    sign = ifelse(loading >= 0, "+", "-"))
    poly_c <- dplyr::bind_rows(poly, poly[1, ])  # cerrar con segmento recto
    
    var_exp <- var_tbl |> dplyr::filter(component == comp_name) |> dplyr::pull(percent)
    
    # Guías (círculos y radios) para look "polar"
    circle <- function(rr){ a <- seq(0, 2*pi, length.out = 361); data.frame(x=rr*cos(a), y=rr*sin(a)) }
    rays <- data.frame(x0 = 0, y0 = 0, x = cos(theta), y = sin(theta))
    
    ggplot() +
      geom_path(data = circle(1.00), aes(x,y), color="grey85", linewidth=.3) +
      geom_path(data = circle(0.75), aes(x,y), color="grey88", linewidth=.3) +
      geom_path(data = circle(0.50), aes(x,y), color="grey60", linetype="dashed") +  # 0 (loading)
      geom_path(data = circle(0.25), aes(x,y), color="grey88", linewidth=.3) +
      geom_segment(data = rays, aes(x=x0,y=y0,xend=x,yend=y), color="grey92", linewidth=.3) +
      # Contorno NEGRO, cerrado y SIN relleno (sin arco)
      geom_polygon(data = poly_c, aes(x,y), fill = NA, color = "black",
                   linewidth = 0.7, linejoin = "round", lineend = "round") +
      geom_point(data = poly, aes(x,y, color = sign), size = 2.6) +
      geom_text(data = poly, aes(x = 1.07*cos(theta), y = 1.07*sin(theta), label = variable),
                size = 3.2, color = "grey35") +
      coord_equal(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15)) +
      scale_color_manual(values = c("-" = "#F25F5C", "+" = "#06B6D4"), guide = guide_legend(title = "Signo")) +
      labs(title = sprintf("PC%d — Cargas por variable (%.1f%% var.)", comp, 100*var_exp),
           subtitle = "Círculo medio = 0. >0 hacia afuera, <0 hacia adentro.",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid = element_blank(), legend.position = "bottom")
  }
  