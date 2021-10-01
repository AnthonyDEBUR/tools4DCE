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


# Interrogation des services cartographiques du SANDRE
La librairie tools4DCE permet d'interroger les services cartographiques du SANDRE.
Par exemple elle permet d'importer sous R le périmètre géographique d'un ou plusieurs SAGE


```r
SAGE<-charge_shp_SAGE(nom_sage = c("Vilaine", "Iton"), crs=2154)
plot(SAGE[,2])
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Elle permet également de récupérer les stations de mesures et les lieux de prélèvements localisées dans un périmètre géographique précis (sans cette précision, l'ensemble des stations définies sous le SANDRE sont téléchargées).

```r
stations<-charge_shp_STAQ(shp_emprise=SAGE)
plot(stations[,1])
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
On peut de la même façon charger les points de suivis associés aux stations de mesures depuis le SANDRE.

```r
lieux_qual<-charge_shp_lieux_qualite(shp_emprise=SAGE)
plot(lieux_qual[,1])
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


