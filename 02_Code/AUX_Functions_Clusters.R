## 00 - Libraries
###===============
pacman::p_load('MASS', tidyverse,pulsosocialcolombia,stats,factoextra,gdata,cluster,gridExtra,officer,ClustImpute,factoextra,dendextend,factoextra)
pacman::p_load(tidyverse, glue, sf,ggthemes)
pacman::p_load(cluster, aplpack, fpc, foreign, TeachingDemos,
               factoextra, NbClust, ape, corrplot, DataExplorer,
               funModeling, compareGroups, tidyverse, dendextend,
               igraph, FeatureImpCluster, flexclust, LICORS, h2o,
               gghighlight)

## 00 - Basic Set up
###===============
w <- 4.5*2.5
h <- 3.2*2.5
text <- 15
d <- 900
s <- 1.2
a_line <- 0.6
a_dot <- 0.7

## 00 - labels
###===============

lab <- c("Acceso a servicio de agua potable y saneamiento",
         "Adultez",
         "Cambio climatico",
         "Capacidad fiscal",
         "Características de las viviendas",
         "Desarrollo económico",
         "Desigualdad",
         "Estructura demográfica",
         "Infancia y Niñez",
         "Juventud",
         "Pobreza",
         "Salud",
         "Salud mental",
         "Seguridad",
         "Vejez")
