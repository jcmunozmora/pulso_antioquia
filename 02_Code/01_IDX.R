# ------------------------------------------------------------------------------
# Project: Antioquias 2025 - Indicadores
# File: 01_IDX.R
# Authors: Laura Quintero, Juan Carlos Muñoz
# Description: Main script for calculating and analyzing indicators for the 
#              Antioquias 2025 project. Loads auxiliary functions and prepares 
#              the environment for further analysis.
# ------------------------------------------------------------------------------
rm(list = ls())

# Load auxiliary functions
source("02_Code/AUX_Functions.R")

# DATOS 
ds_ind <- read_excel("01_Data/01_Derived/01_DATOS_INICIALES.xlsx")
ds <- read_excel("01_Data/01_Derived/01_DATOS_INICIALES_dim.xlsx")
label <- read_excel("01_Data/01_Derived/labels_2.xlsx")


## IDX 1 : Acceso a servicio de agua potable y saneamiento 
cat("Calculando IDX 1: Acceso a servicio de agua potable y saneamiento...\n")

  idx_1 <-  get_subdimension_data(ds, "Acceso a servicio de agua potable y saneamiento")
  
    idx_1 <- idx_1 %>%
    mutate(
      #ircaurbano_dec = -1 *  ircaurbano_dec,
      #ircarural_dec = -1 *   ircarural_dec,
      #ircaurbano_res = -1 *  ircaurbano_res,
      #ircarural_res = -1 *   ircarural_res,
      irca = -1 *            irca,
      ircaurbano = -1 *      ircaurbano,
      ircarural = -1*        ircarural
    ) %>% select(ind_mpio, nvl_label,
                #ircaurbano_dec,
                #ircarural_dec,
                #ircaurbano_res,
                #ircarural_res,
                irca,
                ircaurbano,
                ircarural)  %>%
  reduc_dim(., 1,label, "idx1_servicio","all")


### IDX 2 : Adultez

cat("Calculando IDX 2: Adultez...\n")

idx_2 <-  get_subdimension_data(ds, "Adultez")

idx_2 <- idx_2 %>%
  mutate(
    td_brecha = -1 *  td_brecha,
    td_mujeres = -1 *   td_mujeres,
    td_hombres = -1 *  td_hombres,
    emp_informal = -1 *   emp_informal
  ) %>% select(ind_mpio, nvl_label,
               ss_salud_hombres,
               ss_salud_mujeres,
               #tgp_mujeres,
               td_brecha,
               td_mujeres,
               td_hombres,
               emp_informal
               )  %>%
  reduc_dim(., 1,label, "idx2_Adultez","all")

### IDX 3: Cambio climatico
cat("Calculando IDX 3: Cambio climático...\n")

idx_3 <-  get_subdimension_data(ds, "Cambio climatico")
idx_3 <- idx_3 %>%
  mutate(
    afectados_dn = -1 *  afectados_dn,
    ircc = -1 *   ircc,
    perdida_ca = -1 *  perdida_ca
  ) %>% select(ind_mpio, nvl_label,
               afectados_dn,
               ircc,
               perdida_ca
  )  %>%
  reduc_dim(., 1,label, "idx3_climatico","all")

### IDX 4: Capacidad fiscal
cat("Calculando IDX 4: Capacidad fiscal...\n")

idx_4 <-  get_subdimension_data(ds, "Capacidad fiscal")
idx_4 <- idx_4 %>%
 select(ind_mpio, nvl_label,
        cat_mun,
        idf,
        med_d_mun
  )  %>%
  reduc_dim(., 1,label, "idx4_Capacidad","all")

    
### IDX 5: Características de las viviendas
cat("Calculando IDX 5: Características de las viviendas...\n")

idx_5 <-  get_subdimension_data(ds, "Características de las viviendas")
idx_5 <- idx_5 %>%
  mutate(
    alcantarillado_rural = -1 *  alcantarillado_rural,
    alcantarillado_urbano = -1 *   alcantarillado_urbano,
    deficit_cuali = -1 *  deficit_cuali,
    deficit_cuanti = -1* deficit_cuanti,
    hacinamiento_rural = -1*hacinamiento_rural,
    hacinamiento_urbano = -1*hacinamiento_urbano
  ) %>% select(ind_mpio, nvl_label,
               alcantarillado_rural,
               alcantarillado_urbano,
               deficit_cuali,
               deficit_cuanti,
               hacinamiento_rural,
               hacinamiento_urbano
  )  %>%
  reduc_dim(., 1,label, "idx5_viviendas","all")

### IDX 6: Desarrollo económico
cat("Calculando IDX 6: Desarrollo económico...\n")

idx_6 <-  get_subdimension_data(ds, "Desarrollo económico")
idx_6 <- idx_6 %>%
  select(ind_mpio, nvl_label,
    
                dens_emp,
                fin_banc_pc,
                fin_microcr_pc,
                idv_pri,
                idv_sec,
                idv_ter,
                idv_total,
                #p_m_anticonceptivos,
                tnat_emp,
                #va_pc,
                #va_primario,
                #vpc_pri,
                #vpc_sec,
                #vpc_ter,
                #vpc_total
                internet)  %>%
  reduc_dim(., 1,label, "idx6_crecimiento","all")

    
### IDX 7: Desigualdad
cat("Calculando IDX 7: Desigualdad...\n")

idx_7 <-  get_subdimension_data(ds, "Desigualdad")
                
idx_7 <- idx_7 %>%
  mutate(
    gini_hog = -1 *  gini_hog,
    gini_lab = -1 *   gini_lab
  ) %>% select(ind_mpio, nvl_label,
               gini_hog,
               gini_lab        
  )  %>%
  reduc_dim(., 1,label, "idx7_gini","all")
                
### IDX 8: Estructura demográfica
cat("Calculando IDX 8: Estructura demográfica...\n")

idx_8 <-  get_subdimension_data(ds, "Estructura demográfica")
                
idx_8 <- idx_8 %>%
  mutate(
    hog_mono_fem = -1 *  hog_mono_fem,
    ide_h = -1* ide_h,
    ide_h_c = -1*ide_h_c,
    ide_h_r = -1*ide_h_r,
    ide_m = -1*ide_m,
    ide_m_c = -1*ide_m_c,
    ide_m_r = -1*ide_m_r,
    ide_t = -1*ide_t,
    ide_t_c = -1*ide_t_c,
    ide_t_r = -1*ide_t_r,
                
    
  ) %>% select(ind_mpio, nvl_label,
               pr_t,
               hog_mono_fem,
               ide_h,
               ide_h_c,
               ide_h_r,
               ide_m,
               ide_m_c,
               ide_m_r,
               ide_t,
               ide_t_c,
               ide_t_r)  %>%
  reduc_dim(., 1,label, "idx8_demografica","all")

                

### IDX 9: Infancia y Niñez
cat("Calculando IDX 9: Infancia y Niñez...\n")

idx_9 <-  get_subdimension_data(ds, "Infancia y Niñez")
idx_9 <- idx_9 %>%
  mutate( 
    bajo_peso = -1*bajo_peso,
    controles_prenatales = -1*controles_prenatales,
    desnutricion_aguda = -1*desnutricion_aguda,
    ira = -1*ira,
    mortalidad_desnutricion = -1*mortalidad_desnutricion,
    tdes_primaria =-1*tdes_primaria,
    tdes_transicion = -1*tdes_transicion,
    trep_primaria = -1*trep_primaria,
    trep_transicion = -1*trep_transicion,
    ) %>% select(ind_mpio, nvl_label,
                 tcb_primaria,
                 tcb_transicion,
                 bajo_peso,
                 controles_prenatales,
                 desnutricion_aguda,
                 ira,
                 mortalidad_desnutricion,
                 tdes_primaria,
                 tdes_transicion,
                 trep_primaria,
                 trep_transicion
               )  %>%
  reduc_dim(., 1,label, "idx9_infnin","all")

### IDX 10: Juventud
cat("Calculando IDX 10: Juventud...\n")

idx_10 <-  get_subdimension_data(ds, "Juventud")
idx_10 <- idx_10 %>%
  mutate( 
    vbg_adolescentes = -1* vbg_adolescentes,
    vbg_jovenes = -1*vbg_jovenes,
    ninis = -1*ninis,
    clas_cole = -1*clas_cole,
    tdes_media =-1*tdes_media,
    tdes_secundaria = -1*tdes_secundaria,
    fecundidad_adolescentes = -1*fecundidad_adolescentes,
    fecundidad_ninas = -1*fecundidad_ninas,
    trep_media = -1*trep_media,
    trep_secundaria = -1*trep_secundaria,
    vinculacion_nna = -1*vinculacion_nna
    ) %>% select(ind_mpio, nvl_label,
               #tcb_edsup,
               tcb_media,
               tcb_secundaria,
               tti_edsup,
               #vbg_adolescentes,
               #vbg_jovenes,
               ninis,
               clas_cole,
               tdes_media,
               tdes_secundaria,
               fecundidad_adolescentes,
               fecundidad_ninas,
               trep_media,
               trep_secundaria,
               vinculacion_nna
  )  %>%
  reduc_dim(., 1,label, "idx10_juventud","all")

### IDX 11: Pobreza
cat("Calculando IDX 11: Pobreza...\n")

idx_11 <-  get_subdimension_data(ds, "Pobreza")
idx_11 <- idx_11 %>%
  mutate( 
    ins_alim = -1*ins_alim,
    ins_alim_18 = -1*ins_alim_18,
    ins_alim_5 = -1*ins_alim_5,
    ins_alim_mod = -1*ins_alim_mod,
    ins_alim_mod_18 = -1*ins_alim_mod_18,
    ins_alim_mod_5 = -1*ins_alim_mod_5,
    ins_alim_sev = -1*ins_alim_sev,
    ins_alim_sev_18 = -1*ins_alim_sev_18,
    ins_alim_sev_5 = -1*ins_alim_sev_5,
    pob_ext = -1*pob_ext,
    pob_ext_hombres = -1*pob_ext_hombres,
    pob_ext_mujeres = -1*pob_ext_mujeres,
    pob_ipm = -1*pob_ipm,
    pob_ipm_hogares = -1*pob_ipm_hogares,
    pob_ipm_hombres = -1*pob_ipm_hombres,
    pob_ipm_mujeres = -1*pob_ipm_mujeres,
    pob_mon = -1*pob_mon,
    pob_mon_hombres = -1*pob_mon_hombres,
    pob_mon_mujeres = -1*pob_mon_mujeres,
    pob_nbi = -1*pob_nbi,
    pob_nbi_hogares = -1*pob_nbi_hogares,
    pob_nbi_hombres = -1*pob_nbi_hombres,
    pob_nbi_mujeres = -1*pob_nbi_mujeres
    ) %>% select(ind_mpio, nvl_label,
                 ins_alim,
                 ins_alim_18,
                 ins_alim_5,
                 ins_alim_mod,
                 ins_alim_mod_18,
                 ins_alim_mod_5,
                 ins_alim_sev,
                 ins_alim_sev_18,
                 ins_alim_sev_5,
                 pob_ext,
                 pob_ext_hombres,
                 pob_ext_mujeres,
                 pob_ipm,
                 pob_ipm_hogares,
                 pob_ipm_hombres,
                 pob_ipm_mujeres,
                 pob_mon,
                 pob_mon_hombres,
                 pob_mon_mujeres,
                 pob_nbi,
                 pob_nbi_hogares,
                 pob_nbi_hombres,
                 pob_nbi_mujeres)  %>%
  reduc_dim(., 1,label, "idx11_pobreza","all")


### IDX : Salud
cat("Calculando IDX 12: Salud...\n")

idx_12 <-  get_subdimension_data(ds, "Salud")
idx_12 <- idx_12 %>%
  mutate( 
    dengue = -1*dengue,
    leishmaniasis = -1*leishmaniasis,
    malaria = -1*malaria
  ) %>% select(ind_mpio, nvl_label,
               camas,
               dengue,
               leishmaniasis,
               malaria)  %>%
  reduc_dim(., 1,label, "idx12_salud","all")

### IDX 13: Salud mental
cat("Calculando IDX 13: Salud mental...\n")

idx_13 <-  get_subdimension_data(ds, "Salud mental")
idx_13 <- idx_13 %>%
  mutate( 
    tis_15_29 = -1*tis_15_29,
    tis_15_64 = -1*tis_15_64,
    tis_30_59 = -1*tis_30_59,
    tis_hombres = -1*tis_hombres,
    tis_mujeres = -1*tis_mujeres,
    tis_total = -1*tis_total,
    ts_15_29 = -1*ts_15_29,
    ts_15_64 =-1*ts_15_64,
    ts_30_59 = -1*ts_30_59,
    ts_hombres = -1*ts_hombres,
    ts_mujeres = -1*ts_mujeres,
    ts_total = -1*ts_total
  ) %>% select(ind_mpio, nvl_label,
               tis_15_29,
               tis_15_64,
               tis_30_59,
               tis_hombres,
               tis_mujeres,
               tis_total,
               ts_15_29,
               ts_15_64,
               ts_30_59,
               ts_hombres,
               ts_mujeres,
               ts_total
               )  %>%
  reduc_dim(., 1,label, "idx13_saludmental","all")

### IDX 14 : Seguridad
cat("Calculando IDX 14: Seguridad...\n")

idx_14 <-  get_subdimension_data(ds, "Seguridad")
idx_14 <- idx_14 %>%
  mutate( 
    homicidios = -1*homicidios,
    irv = -1*irv,
    percepcion_seguridad = percepcion_seguridad,
    tasa_delitos = -1*tasa_delitos,
    vbg = -1*vbg
  ) %>% select(ind_mpio, nvl_label,
               homicidios,
               irv,
               percepcion_seguridad,
               tasa_delitos,
               vbg          
               )  %>%
  reduc_dim(., 1,label, "idx14_seguridad","all")

### IDX 15: Vejez
cat("Calculando IDX 15: Vejez...\n")

idx_15 <-  get_subdimension_data(ds, "Vejez")
idx_15 <- idx_15 %>%
  mutate( 
    i_enve_c_t = -1*i_enve_c_t,
    i_enve_h = -1*i_enve_h,
    i_enve_h_c = -1*i_enve_h_c,
    i_enve_h_r = -1*i_enve_h_r,
    i_enve_m = -1*i_enve_m,
    i_enve_m_c = -1*i_enve_m_c,
    i_enve_m_r = -1*i_enve_m_r,
    i_enve_t = -1*i_enve_t,
    i_enve_t_r = -1*i_enve_t_r,
  ) %>% select(ind_mpio, nvl_label,
               ss_pension_hombres,
               ss_pension_mujeres,
               i_enve_c_t,
               i_enve_h,
               i_enve_h_c,
               i_enve_h_r,
               i_enve_m,
               i_enve_m_c,
               i_enve_m_r,
               i_enve_t,
               i_enve_t_r,
  )  %>%
  reduc_dim(., 1,label, "idx15_vejez","all")
# Join data
lista_idx <- mget(ls(pattern = "^idx_"))

df_final <- reduce(lista_idx, function(x, y) {
  left_join(x, y, by = c("ind_mpio", "nvl_label"))
})

write_xlsx(df_final, path = "01_Data/01_Derived/pca_ds_total.xlsx")
saveRDS(df_final, file = "01_Data/01_Derived/pca_ds_total.rds")


