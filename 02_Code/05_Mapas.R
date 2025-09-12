rm(list=ls())
# For replicability
source("02_Code/AUX_Functions_Clusters.R")
# 
set.seed(1601)
library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# Shapes
map_mpios <- st_read("01_Data/01_Derived/maps/municipios_antioquia.shp")
map_prov <- st_read("01_Data/00_Inputs/maps/EAT_DAP_26082025/EAT_PROPUESTA_26082025.shp")
# FINAL MAPS
df_consolidado <- read_csv("04_Cluster_Outputs/Sub_Consolidada/datos_map.csv")
df_transicion <- read_csv("04_Cluster_Outputs/Sub_Transicion/datos_map.csv")
df_vulnerable <- read_csv("04_Cluster_Outputs/Sub_Vulnerable/datos_map.csv")

df_consolidado <- df_consolidado[c("nivl_vl","sub_grp")]
df_transicion <- df_transicion[c("nivl_vl", "sub_grp")]
df_vulnerable <- df_vulnerable[c("nivl_vl", "sub_grp")]

df_consolidado$cat <- "Consolidada"
df_transicion$cat <- "transicion"
df_vulnerable$cat <- "vulnerable"

df_consolidado <- df_consolidado %>% filter(!is.na(sub_grp))
df_transicion <- df_transicion %>% filter(!is.na(sub_grp))
df_vulnerable <- df_vulnerable %>% filter(!is.na(sub_grp))

data <- bind_rows(
  list(Consolidada = df_consolidado, Transicion = df_transicion, Vulnerable = df_vulnerable),
  .id = "grupo_org"
)


# Id 
data2 <- data %>%                              
  group_by(sub_grp, cat) %>%
  arrange(nivl_vl, .by_group = TRUE) %>%     
  mutate(
    combo_id     = cur_group_id(),             
    id_en_combo  = row_number()                
  ) %>%
  ungroup()


data_map <- merge(
  map_mpios, data2,
  by.x = "nivl_vl", by.y = "nivl_vl",
  all.x = TRUE,     
  sort = FALSE
)



# Map Theme
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

# Map
#col_palette <- c("#55E459","#4C9CCC","#FCAC24")
col_palette <- c("#008B00","#00CD00","#00EE00","#00868B", "#00C5CD", "#00E5EE", "#8B8B00", "#CDCD00","#EEEE00" )
data_map$sub_grp <- factor(data_map$combo_id,levels=c(1,3,2,4,6,5,7,9,8),
labels = c("Consolidada - Alta","Consolidada - Media","Consolidada - Baja",
"Transición - Alta","Transición - Media", "Transición - Baja", "Vulnerable - Alta",
"Vulnerable - Media", "Vulnerable - Bajo"))

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

g1


ggsave(file=paste0("final_map.pdf"), 
       g1,height = h, width = w*0.8, dpi = d)

# Mapa - provincias
map_prov <- st_make_valid(map_prov)
map_prov <- st_transform(map_prov, st_crs(map_mpios))
prov <- "ZONA"
# 
redes_diss <- map_prov %>%
  filter(!is.na(.data[[prov]])) %>%
  group_by(grupo_red = .data[[prov]]) %>%
  summarise(.groups = "drop")

g2 <- g1 +
  geom_sf(data = redes_diss, fill = NA, color = "#838B8B", linewidth = 06, inherit.aes = FALSE) #+
  #geom_sf(data = redes_diss, fill = NA, color = "#838B8B", linewidth = 0.6,  inherit.aes = FALSE)

# Save map
ggsave(file=paste0("final_map_prov.pdf"), 
       g2,height = h, width = w*0.8, dpi = d)
