---
title: "Utilisation des fonctions cartographiques"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Utilisation des fonctions cartographiques}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



```r
library(tools4DCE)
```

```
## Error in library(tools4DCE): there is no package called 'tools4DCE'
```


# Interrogation des services cartographiques du SANDRE
La librairie tools4DCE permet d'interroger les services cartographiques du SANDRE.
Par exemple elle permet d'importer sous R le périmètre géographique d'un ou plusieurs SAGE


```r
SAGE<-charge_shp_SAGE(nom_sage = c("Vilaine", "Iton"), crs=2154)
```

```
## Error in charge_shp_SAGE(nom_sage = c("Vilaine", "Iton"), crs = 2154): impossible de trouver la fonction "charge_shp_SAGE"
```

```r
plot(SAGE[,2])
```

```
## Error in plot(SAGE[, 2]): objet 'SAGE' introuvable
```

Elle permet également de récupérer les stations de mesures et les lieux de prélèvements localisées dans un périmètre géographique précis (sans cette précision, l'ensemble des stations définies sous le SANDRE sont téléchargées).

```r
stations<-charge_shp_STAQ(shp_emprise=SAGE)
```

```
## Error in charge_shp_STAQ(shp_emprise = SAGE): impossible de trouver la fonction "charge_shp_STAQ"
```

```r
plot(stations[,1])
```

```
## Error in plot(stations[, 1]): objet 'stations' introuvable
```
On peut de la même façon charger les points de suivis associés aux stations de mesures depuis le SANDRE.

```r
lieux_qual<-charge_shp_lieux_qualite(shp_emprise=SAGE)
```

```
## Error in charge_shp_lieux_qualite(shp_emprise = SAGE): impossible de trouver la fonction "charge_shp_lieux_qualite"
```

```r
plot(lieux_qual[,1])
```

```
## Error in plot(lieux_qual[, 1]): objet 'lieux_qual' introuvable
```


