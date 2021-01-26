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


# R�alisation de graphiques

Pour rendre les graphiques lisibles lorsqu'une longue chronique comporte quelques donn�es tr�s �loign�es du reste de la gamme de mesure, on remplace ces donn�es par une �tiquette localis�e au sommet ou � la base du graphique selon que la valeur non affich�e est inf�rieure ou sup�rieure au reste de la gamme de valeurs.

Pour �cr�ter les valeurs on fait appel � la fonction filtre_donnees_extremes.

## la fonction filtre_donnees_extremes sert � �cr�ter les donn�es que l'on ne souhaite pas afficher � l'�cran.

Par exemple si on veut remplacer toutes les donn�es inf�rieures � 3 par 3 et toutes celles sup�rieures � 9 par 9 :

```r
filtre_donnees_extremes(seq(1:10), ymin=3, ymax=9)
```

```
## Error in filtre_donnees_extremes(seq(1:10), ymin = 3, ymax = 9): impossible de trouver la fonction "filtre_donnees_extremes"
```
## la fonction compte_decimales sert � d�terminer combien de chiffres significatifs comporte un nombre.
Elle sert � d�terminer combien de chiffres significatifs il faut conserver sur l'axe des ordonn�es dans le graphique final.

```r
compte_decimales(3.234)
```

```
## Error in compte_decimales(3.234): impossible de trouver la fonction "compte_decimales"
```


## Couleurs et �chelle des fonds graphiques
Les couleurs et les valeurs des seuils pour les diff�rents param�tres sont list�s dans le jeu de donn�es base_seuils

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

### G�n�ration d'un objet de classe seuil
Un objet de classe seuil sert � param�trer les fonds colorer des graphiques. 
Il pr�cise l'intervalle entre lequel il faut appliquer une classe de qualit� et la couleur de la repr�sentation graphique correspondante.

Pour cr�er un objet de classe seuil on utilise la fonction setSeuils


```r
setSeuils(nom_parametre="parametre test",nom_seuil="AM 25 janv 2010",type_seuil="DCE", code_parametre="1301", synonymes_parametre = "1301",support="3",code_unite="27", seuils=tools4DCE::base_seuils%>%subset(NOM=="TEMPERATURE" & SPECIFICITE=="CYPRINICOLE")%>%left_join(couleurs_classes, by=c("CLASSE", "TYPE"))%>%select(SEUILMIN, SEUILMAX, CLASSE, NOM_COULEUR)%>%mutate_at("CLASSE", factor),bornesinfinclue=T)
```

```
## Error in setSeuils(nom_parametre = "parametre test", nom_seuil = "AM 25 janv 2010", : impossible de trouver la fonction "setSeuils"
```





















