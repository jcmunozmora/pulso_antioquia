# Pulso Social – Réplica metodológica para Antioquia

**Objetivo.** Este repositorio permite reproducir, auditar y extender la metodología de *Pulso Social Colombia* para el caso de **Antioquia**, manteniendo estándares de **transparencia, replicabilidad, escalabilidad y sostenibilidad**.

> Resultados públicos (tablas, gráficos y fichas), scripts reproducibles y un sitio estático con la metodología y hallazgos.

---

## 🧭 Estructura del repositorio

El proyecto sigue un **protocolo de carpetas** diseñado para trazabilidad y replicación.

```
.
├─ Data/                  # Procesamiento de fuentes crudas → bases estandarizadas
│  └─ <Tarea_Fuente>/     # p.ej., GEIH/, ECV/, SISPRO/, etc.
│     ├─ Docs/            # Diccionarios, notas técnicas, documentación de la fuente
│     ├─ Input/           # Datos crudos (solo lectura; no versionar)
│     ├─ Output/          # Productos estandarizados (csv/rds/parquet)
│     └─ Src/             # Códigos R de procesamiento
├─ Descriptives/          # Estadísticas descriptivas y visualizaciones
│  └─ <Tarea_Tema>/
│     ├─ Input/           # Insumos derivados desde Data/*/Output
│     ├─ Output/          # Figuras, tablas, mapas
│     └─ Src/             # Códigos R para bases y gráficos
├─ Analysis/
│  ├─ 01_build_indices/   # PCA por dimensión (contexto y resultados)
│  ├─ 02_cluster/         # Distancias, k-óptimo (codo), dendrograma, mapas
│  └─ 03_antioquia/       # Piezas y panel focalizado para Antioquia
├─ metadata/              # Catálogo de indicadores, diccionario y notas de imputación
├─ docs/                  # Sitio Quarto (GitHub Pages)
├─ src/                   # Utilidades R (funciones reutilizables)
├─ make.R                 # Orquestador: corre el pipeline de punta a punta
├─ .github/workflows/     # CI y despliegue de sitio
├─ .gitignore
├─ LICENSE
└─ README.md
```

**Convenciones de nombres**

- Carpetas con *Mayúscula Inicial* y `barra_al_piso` (ej.: `Ejemplo_base_datos/`).
- Archivos de datos/gráficas/tablas en **minúsculas** y `barra_al_piso`:  
  `base_tasa_desempleo_departamental_2010-2020.rds`, `grafica_gini_departamental.jpeg`.
- Códigos numerados para reflejar orden de ejecución:  
  `01a_lectura_y_limpieza.R`, `01b_homologacion_codigos.R`, `02a_indicadores.R`.  
- Variables y objetos en minúsculas con `barra_al_piso` (ej.: `tasa_desempleo`).  

> **Regla de oro:** `Input/` es **solo lectura** (no sobrescribir ni renombrar crudos). Toda transformación vive en `Src/` y todo resultado va a `Output/`.

---

## 🧩 Metodología (resumen operativo)

### 1) Infraestructura y alcance
- Infraestructura de datos abierta; libro de códigos, scripts y datos derivados en repositorio.  
- Consolidación de indicadores provenientes de fuentes oficiales; trabajo al **nivel departamental** (capital si falta).  
- Dos grandes dominios:
  - **Contexto:** crecimiento/productividad, agua y saneamiento, COVID-19, cambio climático, pobreza, vivienda, desigualdad, estructura demográfica.
  - **Resultados (ciclo de vida):** infancia/niñez, juventud, adultez, vejez.

### 2) Construcción de índices (PCA)
Procedimiento estandarizado (R – {tidymodels}):
1. **Selección de variables** con criterios de cobertura (priorizar ≥80% del territorio), última observación disponible y nivel geográfico departamental.  
2. **Imputación de faltantes** mediante **Nearest Neighbor (NN)**; todo dato imputado queda **marcado** en `metadata/notas_imputacion.csv`.  
3. **Estandarización en dos pasos**:
   - `orderNorm` (transformación rank-normal).  
   - Centrado y escalado (media 0, desviación 1).  
4. **PCA**: reporte de *varianza explicada* y *loadings* por dimensión; índices a nivel departamental exportados a `Analysis/01_build_indices/Output/`.

> El número de componentes retenidas y los umbrales de varianza se documentan por dimensión en cada *run*.

### 3) Agrupamiento territorial (clustering)
- **Distancia**: **euclídea** sobre la matriz de índices por departamento.  
- **Método**: cluster jerárquico aglomerativo (**AGNES**), con comparación de **métodos de enlace**; **Ward** suele ser apropiado.  
- **k óptimo**: **método del codo** (minimiza *within-cluster sum of squares*).  
- Salidas: matriz de distancias, gráfico del codo, dendrograma y mapa por clúster en `Analysis/02_cluster/Output/`.

---

## 🛠️ Reproducibilidad (cómo correr)

### Requisitos
- **R ≥ 4.2**  
- Paquetes: `tidyverse`, `tidymodels`, `recipes`, `yardstick`, `cluster`, `factoextra`, `janitor`, `sf`, `tmap`, `arrow` (opcional), `quarto` (para el sitio).  
- **{renv}** para congelar dependencias.

### Pasos
1. **Clonar** el repositorio.  
2. En R:
   ```r
   install.packages("renv")
   renv::init()      # si es el primer snapshot
   renv::restore()   # si ya existe renv.lock
   ```
3. Configurar (opcional) `config/globals.yml` (rutas, seeds) y `config/regions.yml` (región focal).  
4. **Ejecutar el pipeline**:
   ```r
   source("make.R")  # corre Data -> Descriptives -> PCA -> Clustering -> Reportes
   ```
5. **Renderizar el sitio** (Quarto):
   ```r
   quarto::quarto_render("docs")
   ```

**Notas de datos**
- Los **crudos** van en `Data/<Tarea>/Input/` (excluidos del control de versiones).  
- Documentación y diccionarios en `Data/<Tarea>/Docs/`.  
- Scripts de descarga/lectura/limpieza en `Data/<Tarea>/Src/`.  
- Todo lo que se usa para *Descriptives* debe venir **ya estandarizado** desde `Data/*/Output`.

---

## 🗂️ Metadatos (fuente de verdad)

- `metadata/data_catalog.csv`: catálogo maestro de indicadores (fuente, variable, dimensión, nivel, periodo, licencia, responsable, script_origen, url, notas).
- `metadata/diccionario_variables.csv`: definiciones y transformaciones.
- `metadata/notas_imputacion.csv`: flags y notas por variable/territorio/período.

> Los scripts **leen** de `metadata/` y **no** duplican diccionarios.

---

## 📦 Salidas principales

- **Índices PCA por dimensión** (`Analysis/01_build_indices/Output/`).  
- **Asignación de clústeres y artefactos** (codo, dendrograma, distancias) (`Analysis/02_cluster/Output/`).  
- **Panel Antioquia**: tablas, gráficos y comparativos vs. promedio nacional y/o su grupo de clúster (`Analysis/03_antioquia/Output/` y `docs/antioquia.qmd`).

---

## 🔐 Licencias

- **Código**: MIT.  
- **Datos derivados** (tablas/índices): CC-BY 4.0, salvo restricciones específicas de la fuente (ver `metadata/data_catalog.csv`).  

---

## 📑 Cómo citar

> Muñoz Mora, J.C. & Quintero, L. (2025). *Pulso Social – Réplica metodológica para Antioquia*. Repositorio GitHub.

---

## 👩‍💻👨‍💻 Autores y créditos

- **Metadatos (catálogo & diccionarios):** *Laura Quintero*, *Juan Carlos Muñoz Mora*.  
- Metodología y código: equipo de investigación (ver historial de commits).  

---

## 🤝 Contribuir

1. Crear rama desde `main`.  
2. Seguir convenciones de nombres y *lint*; incluir *headers* en cada script (descripción + fecha).  
3. Abrir *pull request* con descripción de cambios y verificación local del pipeline.

---

### .gitignore recomendado (extracto)

```
# Datos crudos y artefactos grandes
Data/*/Input/**
Data/*/Output/**/*.parquet
Data/*/Output/**/*.rds
Descriptives/*/Output/**
Analysis/*/Output/**
# Archivos locales
.Rproj.user/
.Rhistory
.RData
docs/_site/
renv/library/
renv/staging/
```

**FAQ**

**¿Se puede replicar a otros departamentos?**  
Sí. Ajusta `config/regions.yml` y re-ejecuta `make.R`. La infraestructura está pensada para escalar en cobertura y tiempo.

**¿Por qué PCA y no otro método?**  
Se comparó con alternativas y se adoptó PCA por su **simplicidad de inferencia y trazabilidad**, manteniendo resultados consistentes.
