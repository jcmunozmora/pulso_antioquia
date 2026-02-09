# Proyecto Antioquias – Brechas territoriales

Este documento **NO está dirigido a personas**. 

Es un **archivo de instrucciones para sistemas de IA** (GitHub Copilot, Copilot Chat u otros LLMs integrados en VS Code) que colaboren en el desarrollo de código dentro del proyecto *Antioquias*.

El objetivo es que la IA:
- Comprenda el **contexto metodológico y territorial** del proyecto
- Respete la **estructura del pipeline existente**
- Genere código **consistente, reproducible y alineado** con los estándares definidos

La IA debe tratar este documento como **fuente prioritaria de contexto y reglas** antes de proponer cualquier cambio o fragmento de código.

---

## 1. Contexto obligatorio del proyecto

El proyecto aplica la **metodología de brechas territoriales** basada en *Pulso Social del BID*, con el fin de identificar oportunidades y cuellos de botella en la formación de capital humano.

El análisis se construye a partir de:

1. **Indicadores de contexto** (condiciones estructurales del territorio):
- Agua potable y saneamiento
- Crecimiento económico 
- Cambio climático
- Pobreza
- Características de la vivienda
- Capacidad fiscal
- Desigualdad
- Estructura demográfica
- Salud
- Salud Mental
- Seguridad

2. **Indicadores de resultado** (bienestar individual por ciclo de vida):
- Infancia y niñez  
- Juventud  
- Adultez  
- Vejez  

Las salidas del proyecto alimentan análisis comparativos territoriales, clústeres y visualizaciones espaciales para soporte de política pública.

---

## 2. Arquitectura del pipeline (modelo mental que debes seguir)

El proyecto tiene un pipeline secuencial. Cualquier sugerencia de código debe ubicarse explícitamente en **una de estas etapas**:

1. **Preparación y reshaping de datos**
   - Limpieza
   - Transformación a formato largo
   - Construcción de diccionarios y labels

2. **Construcción de índices multidimensionales**
   - Reducción de dimensiones mediante PCA
   - PCA por subdimensión
   - Exportación de scores, cargas, diagnósticos y gráficos

3. **Clustering territorial**
   - Clúster global
   - Clúster por grupos territoriales (Consolidada / Transición / Vulnerable)

4. **Visualización espacial**
   - Preparación de shapefiles
   - Mapas finales por índice y clúster

Nunca propongas código sin indicar claramente **en qué etapa del pipeline se inserta**.

---

## 2.1. Mapeo detallado de scripts por etapa

### ETAPA 1: Preparación y reshaping de datos

#### **00_BuildData.do** (Stata)
- **Función**: Consolidación inicial de todos los archivos Excel de indicadores municipales
- **Inputs**: 
  - `01_Data/01_Derived/*.xlsx` (12 archivos de indicadores por municipio)
  - Variables clave: `ind_mpio` (código DIVIPOLA), `nvl_label` (nivel territorial)
- **Output**: 
  - `01_Data/01_Derived/01_final_data.dta` (dataset consolidado Stata)
- **Proceso**:
  1. Importa archivos Excel a formato .dta
  2. Corrige códigos municipales (agrega cero inicial para estandarizar a 5 dígitos)
  3. Realiza merges sucesivos por `ind_mpio`
  4. Trata valores faltantes en variables de salud mental
- **Peculiaridades**:
  - Primer archivo como base, no hace merge consigo mismo
  - Algunos archivos requieren corrección de código municipal, otros no
  - Variables de suicidios/intentos tienen tratamiento especial de missings

#### **01_BuildFinalData.R**
- **Función**: Transformación de formato ancho a largo y construcción de datasets específicos
- **Inputs**:
  - `01_Data/01_Derived/01_final_data.dta` (output de script Stata)
  - `01_Data/00_Inputs/INVENTARIO_VARIABLES.xlsx` (diccionario metodológico)
- **Outputs**:
  - `01_DATOS_INICIALES.xlsx` → Datos base para PCA (ind_mpio, nvl_label, var0, value, var_id)
  - `01_DATOS_INICIALES_dim.xlsx` → Con subdimensión para análisis agrupado
  - `01_DATOS_INICIALES_dim_signo.xlsx` → Con subdimensión + sentido para PCA direccional
  - `label.xlsx` → Diccionario var_id ↔ variable
  - `01_final_data_long.xlsx` → Versión intermedia con metadata completa
- **Proceso**:
  1. Pivot de ancho a largo (`pivot_longer`)
  2. Limpieza de nombres: quitar tildes, espacios → guiones bajos, minúsculas
  3. Conversión a numérico y filtrado de variables temporales (`time_*`)
  4. Integración con diccionario metodológico (subdimensión, sentido, descripción)
  5. Generación de 3 versiones según nivel de detalle requerido
- **Peculiaridades**:
  - Usa pipe nativo `|>` con placeholder `_`
  - Filtra variables que inician con `time_` sin explicación detallada
  - Elimina columna "Unidad de medida_descriptiva"
  - La diferencia entre las 3 versiones finales es crucial para diferentes análisis

---

### ETAPA 2: Construcción de índices multidimensionales

#### **01_IDX.R**
- **Función**: Cálculo de 15 índices temáticos mediante PCA
- **Inputs**:
  - `01_Data/01_Derived/01_DATOS_INICIALES.xlsx`
  - `01_Data/01_Derived/01_DATOS_INICIALES_dim.xlsx`
  - `01_Data/01_Derived/labels_2.xlsx` ⚠️ **IMPORTANTE: Es distinto de `label.xlsx`**
  - `02_Code/AUX_Functions.R` (funciones auxiliares)
- **Outputs**:
  - `01_Data/01_Derived/pca_ds_total.xlsx` y `.rds` → Dataset consolidado con 15 índices
  - Para cada índice (generados por `reduc_dim()`):
    - `03_Outputs/{path}/01_TablasDescriptivas/` → Estadísticas descriptivas
    - `03_Outputs/{path}/02_Imagenes/` → Gráficos PCA, mapas, cargas
    - `03_Outputs/{path}/03_DS/` → Scores por municipio
    - `03_Outputs/{path}/{nm}_loadings_y_tops.xlsx` → Cargas, varianza, top variables
    - `03_Outputs/{path}/{nm}_radar_PCk.png` → Gráficos de telaraña por componente
- **Índices calculados** (15 en total):
  1. Acceso a servicio de agua potable y saneamiento
  2. Adultez
  3. Cambio climático
  4. Capacidad fiscal
  5. Características de las viviendas
  6. Desarrollo económico
  7. Desigualdad
  8. Estructura demográfica
  9. Infancia y niñez
  10. Juventud
  11. Pobreza
  12. Salud
  13. Salud mental
  14. Seguridad
  15. Vejez
- **Patrón de procesamiento** (repetido 15 veces):
  ```r
  get_subdimension_data()  # Extrae variables de subdimensión
  → mutate()               # Invierte signo variables negativas (*-1)
  → select()               # Selecciona variables finales
  → reduc_dim()            # Aplica PCA y genera outputs
  ```
- **Peculiaridades**:
  - Muchas variables comentadas dentro de `select()` (decisiones metodológicas previas)
  - El parámetro `trans` en `reduc_dim()` controla inversión de signo de componentes
  - Usa `mget(ls(pattern = "^idx_"))` para recopilar todos los índices automáticamente
  - El script es altamente repetitivo por diseño (facilita auditoría por índice)

---

### FUNCIONES AUXILIARES: AUX_Functions.R

**Descripción**: Biblioteca central de funciones reutilizables para PCA y visualización

#### **Funciones principales**:

1. **`get_subdimension_data(data, subdimension_)`**
   - Filtra datos por subdimensión específica
   - Transforma de formato largo a ancho para PCA
   - Retorna: data frame listo para `reduc_dim()`

2. **`reduc_dim(ds_raw, trans, label, nm, path)`** ⭐ **FUNCIÓN CORE**
   - Pipeline completo de PCA:
     - Normalización con `step_normalize()`
     - Imputación con `step_impute_median()`
     - Cálculo de componentes principales (hasta 5)
     - Generación automática de outputs (tablas, gráficos, mapas)
   - Parámetros:
     - `ds_raw`: data frame con variables numéricas
     - `trans`: 1 (mantener signo) o -1 (invertir componentes)
     - `label`: diccionario terms ↔ lab
     - `nm`: nombre base archivos (ej: "idx1_servicio")
     - `path`: subcarpeta en `03_Outputs/`
   - Outputs generados automáticamente:
     - Tablas descriptivas (.xlsx, .tex con `stargazer`)
     - Gráficos de varianza explicada
     - `plot_top_loadings_jc()` para cargas
     - Mapas por cuartiles (PC1, PC2)
     - Radar charts por componente
     - Dataset final con scores municipales
   - Usa `tidymodels::recipe()` para pipeline reproducible

3. **`mk_map(data_map, var, ann)`**
   - Genera mapas cloropléticos de Antioquia
   - Clasificación por cuartiles
   - Usa shapefile `01_Data/01_Derived/maps/municipios_antioquia.shp`

4. **`plot_top_loadings_jc(v, label, x, ..., n, id, type)`**
   - Visualiza top N variables con mayor carga absoluta
   - Parámetro `v`: factor de inversión (1 o -1)
   - Retorna: gráfico ggplot facetado por componente

5. **`plot_radar_pc_tbl(load_tbl, var_tbl, comp, order_by)`**
   - Gráficos de telaraña (radar charts) por componente
   - Visualización radial de cargas: centro = 0, exterior = |max|
   - Color: azul (+), rojo (-)

#### **Configuración global**:
- Parámetros de visualización: `w`, `h`, `d` (dimensiones y dpi)
- `theme`: tema general para gráficos
- `map_theme`: tema específico para mapas sin ejes
- `col_palette`: paleta de 4 colores para cuartiles

#### **Convenciones importantes**:
- Todas las funciones documentadas con parámetros explícitos
- Separadores con `#` al final para navegación R: `# ---- section ---- #`
- NO contiene `rm(list = ls())` (es archivo de funciones, no script ejecutable)
- Usa `pacman::p_load()` con lista completa de dependencias
- Compatible con `tidymodels` workflow

---

### ETAPA 3: Clustering territorial

#### **02_ClusterAnalisys.R**
- **Función**: Clustering jerárquico de municipios en grupos territoriales
- **Inputs**:
  - `01_Data/01_Derived/pca_ds_Total.rds` (31 componentes principales de 15 índices)
  - `01_Data/01_Derived/labels_2.xlsx` (diccionario de variables)
  - `01_Data/01_Derived/maps/municipios_antioquia.shp` (geometría espacial)
  - `02_Code/AUX_Functions_Clusters.R` (funciones auxiliares)
- **Outputs**:
  - **Clustering global** (3 grupos):
    - `03_Outputs/all/04_Cluster/` → 10 archivos (estadísticas, gráficos, mapas, dataset)
  - **Subclustering por grupo** (6 combinaciones: 3 grupos × 2 referencias):
    - `03_Outputs/Consolidada_ref_dept/04_Cluster/` → Consolidada vs departamento
    - `03_Outputs/Consolidada_ref_subset/04_Cluster/` → Consolidada vs su propio grupo
    - `03_Outputs/Transición_ref_dept/04_Cluster/` → Similar para Transición
    - `03_Outputs/Transición_ref_subset/04_Cluster/`
    - `03_Outputs/Vulnerable_ref_dept/04_Cluster/` → Similar para Vulnerable
    - `03_Outputs/Vulnerable_ref_subset/04_Cluster/`
- **Proceso**:
  1. Lee datos consolidados de PCA con 31 componentes (PC1 de cada subdimensión)
  2. Clustering global: identifica 3 grupos territoriales principales
  3. Para cada grupo (Consolidada, Transición, Vulnerable):
     - Subcluster con referencia departamental (`ref="dept"`, `scale_sd=FALSE`)
     - Subcluster con referencia interna (`ref="subset"`, `scale_sd=TRUE`)
  4. Llama a `ps_cluster()` 7 veces (1 global + 6 subclusters)
- **Peculiaridades**:
  - **31 componentes clave**: Lista explícita en líneas 82-113 documenta qué variables se usan
  - **Doble estandarización**: `standardize_df()` permite comparar:
    - vs departamento completo (contexto regional amplio)
    - vs municipios similares (fineza interna del grupo)
  - **Reordenamiento automático**: Clusters se numeran 1=Mejor → 3=Peor según promedio de PC1
  - Loop anidado ejecuta 6 subclusters en secuencia automática

---

### FUNCIONES AUXILIARES: AUX_Functions_Clusters.R

**Descripción**: Biblioteca especializada para análisis de clustering territorial

#### **Función principal**:

**`ps_cluster(pca_ds, df, lab_g, path, eje)`** ⭐ **PIPELINE COMPLETO DE CLUSTERING**
- **Descripción**: Ejecuta análisis completo de clustering jerárquico con Ward's method, incluyendo diagnósticos, caracterización y visualizaciones
- **Parámetros**:
  - `pca_ds`: Data frame con índices PCA + metadata municipal (`ind_mpio`, `nvl_label`)
  - `df`: Data frame numérico con 31 componentes principales para clustering
  - `lab_g`: Vector de 3 etiquetas para clusters (ej: `c("Consolidada", "Transición", "Vulnerable")`)
  - `path`: Subcarpeta en `03_Outputs/` (ej: `"all"`, `"Consolidada_ref_dept"`)
  - `eje`: Etiqueta eje Y en gráficos de barras (ej: `"Componente Principal (normalizado)"`)
- **Metodología implementada**:
  1. **Matriz de distancias**: Distancia euclidiana entre municipios
  2. **Selección de método**: Compara 4 métodos (average, single, complete, ward) vía coeficiente aglomerativo
  3. **Método Elbow**: Identifica k óptimo (típicamente 3) mediante within-cluster sum of squares
  4. **Clustering Ward**: Corte en k=3 con reordenamiento por desempeño (promedio de PC1)
  5. **Caracterización**: Promedios por cluster de las 31 variables
  6. **Visualización**: 3 gráficos de barras + mapa coroplético
- **Outputs generados** (10 archivos en `03_Outputs/{path}/04_Cluster/`):
  - `Summary_Stats.xlsx` → Estadísticas descriptivas (N, media, SD, min, max)
  - `cluster_dist.png` → Heatmap de matriz de distancias
  - `cluster_elbow.png` → Gráfico método Elbow con línea en k=3
  - `cluster_dendo.png` → Dendrograma jerárquico coloreado
  - `cluster_dendo_vars.png` → Biplot de clusters por componentes
  - `cluster_variables_contexto_1.png` → Barras: agua, clima, fiscal, vivienda
  - `cluster_variables_contexto_2.png` → Barras: economía, demografía, salud, seguridad
  - `cluster_variables_res_1.png` → Barras: ciclo de vida (infancia, juventud, adultez, vejez)
  - `cluster_map.png` → Mapa coroplético de Antioquia con clusters
  - `ds_cluster.csv` y `.rds` → Dataset completo con asignación de clusters

#### **Configuración global**:
- **Parámetros gráficos**: `w=11.25`, `h=8`, `text=15`, `d=900` (dpi)
- **Temas**:
  - `theme`: tema clásico para gráficos de barras/líneas
  - `map_theme`: tema limpio sin ejes para mapas
- **Paletas de colores**:
  - `base_palette`: 3 colores para clustering global (Consolidada=verde, Transición=amarillo, Vulnerable=rojo)
  - `col_palette`: 9 colores para subclustering (3 grupos × 3 niveles: Alto/Medio/Bajo)
  - Sistema inteligente de detección: aplica automáticamente la paleta correcta según etiquetas
- **Labels**: Vector `lab` con 31 etiquetas descriptivas de componentes principales

#### **Características técnicas**:
- **Ward's method**: Minimiza varianza intra-cluster (método más robusto para este caso)
- **Reordenamiento por desempeño**: Cluster 1 siempre es el de mejor promedio en PC1
- **Paleta dinámica**: Detecta automáticamente si es cluster simple (3 grupos) o subcluster (9 combinaciones)
- **Uso de `sf`**: Integración completa con geometrías espaciales para mapas
- **Centroides calculados**: Etiquetas de municipios posicionadas en centroides reales de polígonos

#### **Convenciones importantes**:
- NO contiene `rm(list = ls())` (es archivo de funciones auxiliares)
- Usa `pacman::p_load()` con 13 paquetes especializados
- Separadores estandarizados: `# ----` para chunks, `# ====#` con `#` final para visuales
- Documentación exhaustiva de parámetros en bloque de comentarios
- Compatible con pipeline de `02_ClusterAnalisys.R`

---

### ETAPA 4: Visualización espacial

#### **03_Mapas.R**
- **Función**: Generación de mapa final consolidado con 9 categorías territoriales
- **Inputs**:
  - `04_Cluster_Outputs/Sub_Consolidada/datos_map.csv` (resultados subcluster Consolidada)
  - `04_Cluster_Outputs/Sub_Transicion/datos_map.csv` (resultados subcluster Transición)
  - `04_Cluster_Outputs/Sub_Vulnerable/datos_map.csv` (resultados subcluster Vulnerable)
  - `01_Data/01_Derived/maps/municipios_antioquia.shp` (geometría municipal)
  - `01_Data/00_Inputs/maps/EAT_DAP_26082025/EAT_PROPUESTA_26082025.shp` (provincias/zonas)
  - `02_Code/AUX_Functions_Clusters.R` (se carga con `source()` para heredar config global)
- **Outputs**:
  - `final_map.pdf` → Mapa síntesis con 9 categorías sin overlays
  - `final_map_prov.pdf` → Mapa con límites de provincias/zonas superpuestos
- **Proceso**:
  1. Lee 3 datasets de subclustering (Consolidada, Transición, Vulnerable)
  2. Filtra solo municipios con asignación válida (`!is.na(sub_grp)`)
  3. Combina en dataset único con `bind_rows()` e identificador `grupo_org`
  4. Genera IDs únicos para 9 combinaciones: `combo_id = cur_group_id()` sobre `(sub_grp, cat)`
  5. Merge con shapefile de municipios por código `nivl_vl`
  6. Reordenamiento factorial: levels `c(1,3,2,4,6,5,7,9,8)` para agrupar visualmente
  7. Genera mapa base con `geom_sf()` + etiquetas con `geom_text()`
  8. Overlay de provincias: `st_make_valid()` + `st_transform()` + `summarise()` por zona
  9. Exporta 2 versiones: sin y con límites provinciales
- **Peculiaridades**:
  - **9 categorías sintéticas**: 3 grupos territoriales × 3 niveles = matriz final
    - Consolidada: Alta, Media, Baja (verdes)
    - Transición: Alta, Media, Baja (cianes)
    - Vulnerable: Alta, Media, Baja (amarillos)
  - **Reordenamiento no secuencial**: levels `c(1,3,2,4,6,5,7,9,8)` mejora agrupación visual en leyenda
  - **Paleta de 9 colores**: agrupada por familias (verdes, cianes, amarillos) para coherencia perceptual
  - **Dependencia de `source()`**: Hereda variables `w`, `h`, `d`, `map_theme` de `AUX_Functions_Clusters.R`
  - **Dissolve de provincias**: `summarise(.groups = "drop")` fusiona polígonos por `grupo_red`
  - **CRS alignment**: `st_transform()` asegura compatibilidad entre shapefiles
  - **Uso de `rep(col_palette, 40)`**: Garantiza suficientes colores para factor con NAs

#### **Diferencias con mapas de etapa anterior**:
- Etapa 3 (`02_ClusterAnalisys.R`): Genera mapas por separado (1 global + 6 subclusters individuales)
- Etapa 4 (`03_Mapas.R`): Sintetiza subclusters en visualización única consolidada
- Mapas de Etapa 3 son analíticos (comparación interna), Etapa 4 es comunicacional (síntesis ejecutiva)

---

## 3. Reglas estrictas de estilo y lenguaje (OBLIGATORIAS)

### 3.1. Paradigma de programación

- Priorizar claridad y legibilidad sobre soluciones compactas
- Evitar anidamientos profundos

### 3.2. Manejo de paquetes

- Usar `pacman::p_load()` cuando aplique
- NO introducir paquetes nuevos sin una justificación explícita

### 3.3. Funciones y evaluación

- Las funciones deben ser:
  - Reutilizables
  - Explícitas en inputs y outputs
  - Compatibles con pipes (`%>%`)

### 3.4. Estructura obligatoria de scripts

Todo script nuevo o modificado debe respetar esta estructura de ejemplo:

```r
# ==============================================================================#
# ANTIOQUIAS - NombreScript.R
# TÍTULO DESCRIPTIVO
#
# AUTHORS: [Nombres si existen]
#
# OBJETIVO:
# Descripción breve y clara del propósito del script
#
# INPUTS:  Listar archivos de entrada con rutas relativas
# OUTPUTS: Listar archivos de salida con rutas relativas
#
# ETAPA DEL PIPELINE: [1-4 según corresponda]
# ==============================================================================#

# ---- packages ---- 
rm(list = ls())

pacman::p_load(
  tidyverse,
  tidymodels,
  readxl,
  writexl,
  sf
)

tidymodels_prefer()

# ---- paths ----
path_in  <- "01_Data"
path_out <- "03_Outputs"

# ---- lectura de datos ----
# ...

# ---- funciones ----
# helpers locales únicamente

# ---- análisis / gráficos ----
# ...
```

**NOTAS IMPORTANTES sobre la estructura**:

1. **Headers de sección**:
   - SIEMPRE usar formato: `# ---- nombre_sección ---- ` o `# ===========================#`
   - Nombres descriptivos en minúsculas con guiones bajos

2. **Archivos de funciones** (como `AUX_Functions.R`):
   - NO incluir `rm(list = ls())` al inicio
   - Documentar TODOS los parámetros de cada función con bloque de comentarios
   - Formato de documentación de funciones:
   ```r
   # ==============================================================================#
   # FUNCIÓN: nombre_funcion ----
   # ==============================================================================#
   # Descripción breve de qué hace
   #
   # PARÁMETROS:
   #   param1 - Descripción del parámetro 1
   #   param2 - Descripción del parámetro 2
   #
   # RETORNA:
   #   Descripción de qué devuelve
   # ==============================================================================#
   # ---- nombre_funcion ---- 
   ```

3. **Uso de paths**:
   - Siempre usar `file.path()` para construcción de rutas
   - Definir paths base en sección `# ---- paths ---- #`
   - NO hardcodear rutas completas

---

## 3.5. Convenciones específicas descubiertas

### Variables clave que NO se deben modificar:
- **`ind_mpio`**: Código DIVIPOLA municipal (5 dígitos con cero inicial)
- **`nvl_label`**: Nivel territorial (nombre del municipio/departamento)
- **`var0`**: Identificador de variable en formato estandarizado
- **`value`**: Valor numérico de la variable
- **`var_id`**: ID numérico de variable (generado con `factor()`)
- **`Subdimension`**: Categoría temática de la variable
- **`Sentido`**: Dirección de la variable (positivo/negativo)

### Archivos críticos del pipeline:
- **`labels_2.xlsx`** vs **`label.xlsx`**: Son DIFERENTES
  - `label.xlsx`: generado por `01_BuildFinalData.R` (diccionario básico)
  - `labels_2.xlsx`: versión extendida usada en `01_IDX.R` (incluye más metadata)
  - NO son intercambiables

### Transformaciones metodológicas:
- **Inversión de signo** (`*-1`): Variables donde valores altos indican peor situación
  - Ejemplo: tasas de desempleo, pobreza, mortalidad
  - Se aplica ANTES del PCA para que todas las variables apunten en la misma dirección
  - Listado explícito en cada bloque de `01_IDX.R`

### Tratamiento de valores faltantes:
- **Imputación**: `step_impute_median()` en pipeline PCA
- **Variables de salud mental**: reemplazo explícito de blancos por "NA" en Stata
- **Filtrado**: Variables `time_*` se eliminan automáticamente

### Estructura de outputs:
```
03_Outputs/
  └── {path}/                          # ej: "all", "Consolidada_ref_dept"
      ├── 01_TablasDescriptivas/
      │   ├── {nm}.xlsx               # Estadísticas descriptivas
      │   ├── {nm}.tex                # Tabla LaTeX para reportes
      │   ├── {nm}_summary_labels.xlsx
      │   └── {nm}_vr.xlsx            # Varianza explicada
      ├── 02_Imagenes/
      │   ├── {nm}_results.png        # Panel completo (PC1/PC2, varianza, cargas)
      │   ├── {nm}_map_pc1.png
      │   ├── {nm}_map_pc2.png
      │   ├── {nm}_map_pcall.png
      │   └── {nm}_radar_PC{k}.png    # k = 1,2,3,4,5
      ├── 03_DS/
      │   └── ds_{nm}.xlsx            # Scores municipales
      └── {nm}_loadings_y_tops.xlsx   # Cargas, varianza, top variables
```

## 4. Restricciones críticas (NO VIOLAR)

- NO cambiar nombres de variables clave
- NO cambiar rutas de lectura o escritura existentes
- NO modificar silenciosamente estructuras de outputs
- NO alterar la lógica metodológica (PCA, clustering) sin indicarlo explícitamente

Las funciones centrales del proyecto deben considerarse **contratos estables**.

---

## 5. Principio de modificación segura

Cuando propongas cambios:

1. Mantén intactos los inputs y outputs existentes
2. Asegura compatibilidad hacia atrás
3. Añade validaciones solo si no alteran resultados esperados
4. Prioriza soluciones incrementales

---

## 6. Cómo debes responder cuando se te pide ayuda

Cuando el usuario solicite código o explicación:

- Identifica primero la etapa del pipeline
- Explica brevemente el impacto del cambio
- Proporciona código alineado con estas instrucciones
- Evita soluciones “creativas” que no respeten el marco existente

---

## 7. Advertencias y casos especiales detectados

### 7.1. Para agregar nuevas variables al análisis:

1. Agregar archivo Excel a `01_Data/00_Inputs/` o `01_Data/01_Derived/`
2. En `00_BuildData.do`:
   - Añadir nombre a lista `direccion` (línea ~33)
   - Si necesita corrección de código, añadir bloque después de línea ~41
   - Añadir merge correspondiente con comentario descriptivo
3. Actualizar `INVENTARIO_VARIABLES.xlsx` con metadata de nuevas variables
4. Verificar que nuevas variables aparezcan en `01_DATOS_INICIALES_dim.xlsx`
5. Si pertenecen a subdimensión existente, se incluirán automáticamente en PCA
6. Si crean nueva subdimensión, añadir bloque completo en `01_IDX.R`

### 7.2. Modificación de índices existentes:

- **Agregar variables**: Añadir en bloque `select()` del índice correspondiente
- **Remover variables**: Comentar línea en `select()` (NO borrar, dejar trazabilidad)
- **Cambiar signo**: Agregar/quitar multiplicación `*-1` en bloque `mutate()`
- **Verificar**: Reejecutar solo el bloque del índice modificado
- **Importante**: Variables comentadas en scripts actuales reflejan decisiones metodológicas previas

### 7.3. Debugging de PCA:

Si un índice falla en `reduc_dim()`:
1. Verificar suficientes variables numéricas (mínimo 2-3)
2. Revisar valores faltantes excesivos (>50% en alguna variable)
3. Confirmar que no hay variables de varianza cero
4. Verificar nombres de columnas sin caracteres especiales
5. Comprobar que `labels_2.xlsx` contenga todas las variables usadas

### 7.5. Interpretación de componentes:

- **PC1**: Captura máxima varianza, generalmente "nivel general" del índice
- **trans = -1**: Invierte signo para que valores altos = mejor situación
- **Loadings positivos** en gráficos oscuros: contribuyen positivamente al índice
- **Loadings negativos** en gráficos claros: contribuyen inversamente
- **Varianza acumulada**: objetivo típico >60% con primeros 2-3 componentes

### 7.6. Mapas y visualización:

- Solo funciona para Antioquia (shapefile específico)
- Municipios se identifican por código en `nivl_vl` del shapefile
- Clasificación por cuartiles: 1 = peor situación, 4 = mejor situación
- Coordenadas X, Y predefinidas en shapefile para etiquetas

---

## 8. Principio rector

La IA **no es autora del análisis**, sino una asistente técnica.

El criterio metodológico, territorial y de política pública **prevalece siempre** sobre la automatización.

Este archivo debe ser interpretado como **instrucción de sistema** para cualquier IA que colabore en este repositorio.

---

## 9. Registro de revisiones

**Última actualización**: Febrero 2026  
**Scripts documentados**: 
- `00_BuildData.do` (Stata - Etapa 1: Preparación de datos)
- `01_BuildFinalData.R` (Etapa 1: Reshaping y construcción de datasets)
- `01_IDX.R` (Etapa 2: Construcción de 15 índices PCA)
- `AUX_Functions.R` (Funciones transversales para PCA y visualización)
- `02_ClusterAnalisys.R` (Etapa 3: Clustering territorial)
- `AUX_Functions_Clusters.R` (Funciones auxiliares para clustering)
- `03_Mapas.R` (Etapa 4: Mapa final consolidado) **← NUEVO**

**Pendientes de documentar**:
- `aux_maps.R` (Funciones auxiliares para mapas - si existe)
- Scripts en `00_Homogeneizacion_Inputs/` (preprocesamiento)
- Scripts específicos de Quarto en `03_Outputs/` (.qmd)

**Mejoras aplicadas**:
- **Estructura estándar de headers implementada en 7 scripts** (todas las etapas del pipeline)
- Documentación completa de parámetros en 6 funciones principales:
  - `reduc_dim()` (AUX_Functions.R)
  - `ps_cluster()` (AUX_Functions_Clusters.R)
  - `get_subdimension_data()`, `mk_map()`, `plot_top_loadings_jc()`, `plot_radar_pc_tbl()`
- Eliminación de código duplicado (40 líneas en AUX_Functions.R)
- Corrección de bugs identificados:
  - Merge innecesario en `00_BuildData.do` (líneas 63-64)
  - Placeholder incorrecto en `01_BuildFinalData.R` (`.` → `_` para pipe nativo)
  - Linewidth typo en `03_Mapas.R` (06 → 0.6)
- Estandarización completa:
  - `pacman::p_load()` en todos los scripts
  - Paths relativos sin hardcoding
  - Separadores `# ----` vs `# ====#` consistentes
- Mejora de comentarios en lógica compleja:
  - `standardize_df()` con dual reference (Etapa 3)
  - Reordenamiento de clusters por desempeño (Etapa 3)
  - Combinación de 9 categorías sintéticas (Etapa 4)
- Documentación de metodologías clave:
  - Dual standardization: `ref="dept"` vs `ref="subset"`
  - Matriz 3×3 para clasificación final (grupos × niveles)
  - Ward's method con k=3 óptimo

**Pipeline completamente documentado**: Etapas 1-4 cubiertas

---

## Sesiones

### Sesion del 09-02-2026

Resumen:
- Se ajustaron los graficos PCA con lineas guia en PC1=0 y PC2=0 y labels municipales legibles.
- Se corrigio el manejo de columnas auxiliares para evitar nombres vacios en los outputs de PCA.
- Se creo y ejecuto el script de homogenizacion de mortalidad por IAM con promedio 2005-2024.
- Se integro la mortalidad IAM en el pipeline de datos y en el indice de vejez con signo negativo.
