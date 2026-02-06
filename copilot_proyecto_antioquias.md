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

---

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

## 7. Principio rector

La IA **no es autora del análisis**, sino una asistente técnica.

El criterio metodológico, territorial y de política pública **prevalece siempre** sobre la automatización.

Este archivo debe ser interpretado como **instrucción de sistema** para cualquier IA que colabore en este repositorio.

