---
title: "comment-utiliser-tools4DCE"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{comment-utiliser-tools4DCE}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(tools4DCE)
```

```
## Error in library(tools4DCE): there is no package called 'tools4DCE'
```


# Réalisation de graphiques

Pour rendre les graphiques lisibles lorsqu'une longue chronique comporte quelques données très éloignées du reste de la gamme de mesure, on remplace ces données par une étiquette localisée au sommet ou à la base du graphique selon que la valeur non affichée est inférieure ou supérieure au reste de la gamme de valeurs.

Pour écrêter les valeurs on fait appel à la fonction filtre_donnees_extremes.

## la fonction filtre_donnees_extremes sert à écrêter les données que l'on ne souhaite pas afficher à l'écran.

Par exemple si on veut remplacer toutes les données inférieures à 3 par 3 et toutes celles supérieures à 9 par 9 :

```r
filtre_donnees_extremes(seq(1:10), ymin=3, ymax=9)
```

```
## Error in filtre_donnees_extremes(seq(1:10), ymin = 3, ymax = 9): impossible de trouver la fonction "filtre_donnees_extremes"
```
## la fonction compte_decimales sert à déterminer combien de chiffres significatifs comporte un nombre.
Elle sert à déterminer combien de chiffres significatifs il faut conserver sur l'axe des ordonnées dans le graphique final.

```r
compte_decimales(3.234)
```

```
## Error in compte_decimales(3.234): impossible de trouver la fonction "compte_decimales"
```


## Couleurs et échelle des fonds graphiques
Les couleurs et les valeurs des seuils pour les différents paramètres sont listés dans le jeu de données base_seuils

```r
head(base_seuils)
```

```
## Error in head(base_seuils): objet 'base_seuils' introuvable
```

pour avoir la couleur on croise sur les colonnes TYPE et CLASSE avec la table couleurs_classes

```r
head(couleurs_classes)
```

```
## Error in head(couleurs_classes): objet 'couleurs_classes' introuvable
```

pour classer les levels on utilise la colonne CLASSE de la data.frame ordre_facteurs_qualite

```r
head(ordre_facteurs_qualite)
```

```
## Error in head(ordre_facteurs_qualite): objet 'ordre_facteurs_qualite' introuvable
```

### Génération d'un objet de classe seuil
Un objet de classe seuil sert à paramétrer les fonds colorer des graphiques. 
Il précise l'intervalle entre lequel il faut appliquer une classe de qualité et la couleur de la représentation graphique correspondante.

Pour créer un objet de classe seuil on utilise la fonction setSeuils


```r
setSeuils(nom_parametre="parametre test",nom_seuil="AM 25 janv 2010",type_seuil="DCE", code_parametre="1301", synonymes_parametre = "1301",support="3",code_unite="27", seuils=tools4DCE::base_seuils%>%subset(NOM=="TEMPERATURE" & SPECIFICITE=="CYPRINICOLE")%>%left_join(couleurs_classes, by=c("CLASSE", "TYPE"))%>%select(SEUILMIN, SEUILMAX, CLASSE, NOM_COULEUR)%>%mutate_at("CLASSE", factor),bornesinfinclue=T)
```

```
## Error in setSeuils(nom_parametre = "parametre test", nom_seuil = "AM 25 janv 2010", : impossible de trouver la fonction "setSeuils"
```





















