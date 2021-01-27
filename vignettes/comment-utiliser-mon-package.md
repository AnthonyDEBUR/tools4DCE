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
## Loading required package: plyr
```

```
## ----------------------------------------------------------------------------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ----------------------------------------------------------------------------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise, summarize
```

```
## The following object is masked from 'package:purrr':
## 
##     compact
```

```
## The following objects are masked from 'package:Hmisc':
## 
##     is.discrete, summarize
```

```
## The following object is masked from 'package:gWidgetsRGtk2':
## 
##     id
```

```
## The following object is masked from 'package:gWidgets':
## 
##     id
```

```
## Loading required package: scales
```

```
## Warning: package 'scales' was built under R version 4.0.3
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

```
## Loading required package: cowplot
```

```
## Warning: package 'cowplot' was built under R version 4.0.3
```

```
## 
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggmap':
## 
##     theme_nothing
```

```
## Loading required package: httr
```

```
## Warning: package 'httr' was built under R version 4.0.3
```

```
## 
## Attaching package: 'httr'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     progress
```

```
## The following object is masked from 'package:svMisc':
## 
##     progress
```

```
## The following object is masked from 'package:tcltk2':
## 
##     config
```

```
## Loading required package: ows4R
```

```
## Warning: package 'ows4R' was built under R version 4.0.3
```

```
## Loading required package: geometa
```

```
## Warning: package 'geometa' was built under R version 4.0.3
```

```
## Loading ISO 19139 XML schemas...
```

```
## Loading ISO 19115 codelists...
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
##  [1] 3 3 3 4 5 6 7 8 9 9
```
## la fonction compte_decimales sert à déterminer combien de chiffres significatifs comporte un nombre.
Elle sert à déterminer combien de chiffres significatifs il faut conserver sur l'axe des ordonnées dans le graphique final.

```r
compte_decimales(3.234)
```

```
## [1] 3
```


## Couleurs et échelle des fonds graphiques
Les couleurs et les valeurs des seuils pour les différents paramètres sont listés dans le jeu de données base_seuils

```r
head(base_seuils)
```

```
## # A tibble: 6 x 12
##   NOM         SUPPORT FRACTION PARAMETRE UNITE SEUILMIN SEUILMAX CLASSE   NOM_SEUIL      TYPE  TYPE_BORNE       SPECIFICITE
##   <chr>       <chr>   <chr>    <chr>     <chr>    <dbl>    <dbl> <chr>    <chr>          <chr> <chr>            <chr>      
## 1 TEMPERATURE 3       <NA>     1301      27      -Inf       20   TRES BON AM. 25/01/2010 DCE   BORNE_INF_INCLUE SALMONICOLE
## 2 TEMPERATURE 3       <NA>     1301      27        20       21.5 BON      AM. 25/01/2010 DCE   BORNE_INF_INCLUE SALMONICOLE
## 3 TEMPERATURE 3       <NA>     1301      27        21.5     25   MOYEN    AM. 25/01/2010 DCE   BORNE_INF_INCLUE SALMONICOLE
## 4 TEMPERATURE 3       <NA>     1301      27        25       28   MEDIOCRE AM. 25/01/2010 DCE   BORNE_INF_INCLUE SALMONICOLE
## 5 TEMPERATURE 3       <NA>     1301      27        28      Inf   MAUVAIS  AM. 25/01/2010 DCE   BORNE_INF_INCLUE SALMONICOLE
## 6 TEMPERATURE 3       <NA>     1301      27      -Inf       24   TRES BON AM. 25/01/2010 DCE   BORNE_INF_INCLUE CYPRINICOLE
```

pour avoir la couleur on croise sur les colonnes TYPE et CLASSE avec la table couleurs_classes

```r
head(couleurs_classes)
```

```
##   TYPE   CLASSE NOM_COULEUR
## 1  DCE TRES BON  dodgerblue
## 2  DCE      BON      green1
## 3  DCE    MOYEN      yellow
## 4  DCE MEDIOCRE      orange
## 5  DCE  MAUVAIS         red
## 6  DCE  INCONNU        grey
```

pour classer les levels on utilise la colonne CLASSE de la data.frame ordre_facteurs_qualite

```r
head(ordre_facteurs_qualite)
```

```
##     CLASSE
## 1 TRES BON
## 2      BON
## 3    MOYEN
## 4 MEDIOCRE
## 5  MAUVAIS
## 6  INCONNU
```

### Génération d'un objet de classe seuil
Un objet de classe seuil sert à paramétrer les fonds colorer des graphiques. 
Il précise l'intervalle entre lequel il faut appliquer une classe de qualité et la couleur de la représentation graphique correspondante.

Pour créer un objet de classe seuil on utilise la fonction setSeuils


```r
setSeuils(nom_parametre="parametre test",nom_seuil="AM 25 janv 2010",type_seuil="DCE", code_parametre="1301", synonymes_parametre = "1301",support="3",code_unite="27", seuils=tools4DCE::base_seuils%>%subset(NOM=="TEMPERATURE" & SPECIFICITE=="CYPRINICOLE")%>%left_join(couleurs_classes, by=c("CLASSE", "TYPE"))%>%select(SEUILMIN, SEUILMAX, CLASSE, NOM_COULEUR)%>%mutate_at("CLASSE", factor),bornesinfinclue=T)
```

```
## An object of class "seuil"
## Slot "nom_parametre":
## [1] "parametre test"
## 
## Slot "nom_seuil":
## [1] "AM 25 janv 2010"
## 
## Slot "type_seuil":
## [1] "DCE"
## 
## Slot "code_parametre":
## [1] "1301"
## 
## Slot "synonymes_parametre":
## [1] "1301"
## 
## Slot "support":
## [1] "3"
## 
## Slot "fraction":
## [1] ""
## 
## Slot "code_unite":
## [1] "27"
## 
## Slot "seuils":
## # A tibble: 5 x 4
##   SEUILMIN SEUILMAX CLASSE   NOM_COULEUR
##      <dbl>    <dbl> <fct>    <chr>      
## 1   -Inf       24   TRES BON dodgerblue 
## 2     24       25.5 BON      green1     
## 3     25.5     27   MOYEN    yellow     
## 4     27       28   MEDIOCRE orange     
## 5     28      Inf   MAUVAIS  red        
## 
## Slot "bornesinfinclue":
## [1] TRUE
## 
## Slot "specificites":
## [1] ""
```

Pour créer une liste de seuils prédéfinis à partir des fichiers de données il suffit de taper la commande makeSeuils().
A ce jour 377 seuils différents sont ainsi définis.
On peut se limiter à créer la liste pour un ensemble de codes paramètres en renseignant l'argument CdParametre.
Idem pour les codes support, fraction, pour les types de seuils (DCE ou NON_DCE), pour les spécificités (ex. SALMONICOLE ou CYPRINICOLE).


```r
tmp<-makeSeuils()
tmp[100]
```

```
## [[1]]
## An object of class "seuil"
## Slot "nom_parametre":
## [1] "Aclonifène"
## 
## Slot "nom_seuil":
## [1] "AM.11/01/2007"
## 
## Slot "type_seuil":
## [1] "AEP"
## 
## Slot "code_parametre":
## [1] "1688"
## 
## Slot "synonymes_parametre":
## [1] "1688"
## 
## Slot "support":
## [1] "3"
## 
## Slot "fraction":
## [1] "23"
## 
## Slot "code_unite":
## [1] "133"
## 
## Slot "seuils":
## # A tibble: 3 x 4
##   SEUILMIN SEUILMAX CLASSE                                     NOM_COULEUR  
##      <dbl>    <dbl> <fct>                                      <chr>        
## 1   -Inf        0.1 [0;seuil distribution]                     darkturquoise
## 2      0.1      2   ]seuil distribution; seuil potabilisation] darkorange   
## 3      2      Inf   >seuil potabilisation                      darkmagenta  
## 
## Slot "bornesinfinclue":
## [1] FALSE
## 
## Slot "specificites":
## [1] "ARS56|ARS56PEST|ARS56DCE"
```

```r
test<-makeSeuils(CdParametre=c("1340", "1301"), specificites=c(NA, "CYPRINICOLE"), type_seuil = "DCE")
return(test)
```

```
## [[1]]
## An object of class "seuil"
## Slot "nom_parametre":
## [1] "TEMPERATURE"
## 
## Slot "nom_seuil":
## [1] "AM. 25/01/2010"
## 
## Slot "type_seuil":
## [1] "DCE"
## 
## Slot "code_parametre":
## [1] "1301"
## 
## Slot "synonymes_parametre":
## [1] "1301"
## 
## Slot "support":
## [1] "3"
## 
## Slot "fraction":
## [1] NA
## 
## Slot "code_unite":
## [1] "27"
## 
## Slot "seuils":
## # A tibble: 5 x 4
##   SEUILMIN SEUILMAX CLASSE   NOM_COULEUR
##      <dbl>    <dbl> <fct>    <chr>      
## 1   -Inf       24   TRES BON dodgerblue 
## 2     24       25.5 BON      green1     
## 3     25.5     27   MOYEN    yellow     
## 4     27       28   MEDIOCRE orange     
## 5     28      Inf   MAUVAIS  red        
## 
## Slot "bornesinfinclue":
## [1] TRUE
## 
## Slot "specificites":
## [1] "CYPRINICOLE"
## 
## 
## [[2]]
## An object of class "seuil"
## Slot "nom_parametre":
## [1] "NO3"
## 
## Slot "nom_seuil":
## [1] "AM. 25/01/2010"
## 
## Slot "type_seuil":
## [1] "DCE"
## 
## Slot "code_parametre":
## [1] "1340"
## 
## Slot "synonymes_parametre":
## [1] "1340"
## 
## Slot "support":
## [1] "3"
## 
## Slot "fraction":
## [1] NA
## 
## Slot "code_unite":
## [1] "162"
## 
## Slot "seuils":
## # A tibble: 3 x 4
##   SEUILMIN SEUILMAX CLASSE   NOM_COULEUR
##      <dbl>    <dbl> <fct>    <chr>      
## 1     -Inf       10 TRES BON dodgerblue 
## 2       10       50 BON      green1     
## 3       50      Inf MOYEN    yellow     
## 
## Slot "bornesinfinclue":
## [1] TRUE
## 
## Slot "specificites":
## [1] NA
```
La liste des paramètres concernés est la suivante :

```r
lapply(tmp, function(x) `@`( x , nom_parametre)[[1]])%>%unlist%>%sort%>%unique
```

```
##   [1] "1-(3,4-dichlorophenyl)-3-methyl-uree" "1,2-dichloroéthane"                   "2-hydroxy atrazine"                  
##   [4] "2,4-D"                                "2,4-DB"                               "2,4-MCPA"                            
##   [7] "2,4-MCPB"                             "3,4-dichlorophenyluree"               "Acetamiprid"                         
##  [10] "Acétochlore"                          "Aclonifène"                           "Alachlor OXA"                        
##  [13] "Alachlore"                            "Aldrine"                              "Aluminium dissous"                   
##  [16] "Amétryne"                             "Amidosulfuron"                        "Aminotriazole"                       
##  [19] "AMPA"                                 "Anthracène"                           "Arsenic"                             
##  [22] "Arsenic dissous"                      "ASPT (I2M2)"                          "Atrazine"                            
##  [25] "Atrazine déisopropyl"                 "Atrazine déséthyl"                    "Azoxystrobine"                       
##  [28] "AZOXYSTROBINE"                        "Beflubutamide"                        "Bentazone"                           
##  [31] "Benzène"                              "Benzo(a)pyrène"                       "Benzo(b)fluoranthène"                
##  [34] "Benzo(g,h,i)pérylène"                 "Benzo(k)fluoranthène"                 "Bifénox"                             
##  [37] "Biphényle"                            "Bixafen"                              "Boscalid"                            
##  [40] "Boscalide"                            "Bromacil"                             "Bromoxynil"                          
##  [43] "C. organique dissous"                 "Cadmium"                              "Cadmium dissous"                     
##  [46] "Carbaryl"                             "Carbendazime"                         "Carbétamide"                         
##  [49] "Carbofuran"                           "Carboxine"                            "Chlorantraniliprole"                 
##  [52] "Chlorfenvin-phos"                     "Chlorfenvinphos"                      "Chloridazone"                        
##  [55] "Chloroalcanes C10-13"                 "Chloroméquat chlorure"                "Chlorophylle A + Phéopigments"       
##  [58] "Chlorothalonil"                       "Chlorprophame"                        "Chlorpyriphos-éthyl"                 
##  [61] "Chlortoluron"                         "Chrome"                               "Chrome dissous"                      
##  [64] "Clethodim"                            "Clomazone"                            "Clopyralide"                         
##  [67] "Clothianidine"                        "Composés du tributylétain"            "Conductivité"                        
##  [70] "Cuivre"                               "Cuivre dissous"                       "Cybutryne"                           
##  [73] "Cycloxydime"                          "Cyperméthrine"                        "Cyproconazole"                       
##  [76] "Cyprodinil"                           "DBO5"                                 "DCO"                                 
##  [79] "DDD 24'"                              "DDD 44'"                              "DDE 24'"                             
##  [82] "DDE 44'"                              "DDT 24'"                              "DDT 44'"                             
##  [85] "DDT total"                            "DEHP"                                 "DELTAO2 dissous"                     
##  [88] "Densité Indiv. Invertivores (DII)"    "Densité Indiv. omnivores (DIO)"       "Densité Indiv. Tolérants (DIT)"      
##  [91] "Densité totale d'individus (DTI)"     "Desméthylisoproturon"                 "Dicamba"                             
##  [94] "Dichlobenil"                          "Dichlormide"                          "dichlorométhane"                     
##  [97] "Dichloropropène-1,3"                  "Dichlorprop"                          "Dichlorvos"                          
## [100] "Dicofol"                              "Didemethylisoproturon"                "Dieldrine"                           
## [103] "Diflufenicanil"                       "Diflufénicanil"                       "Dimétachlore"                        
## [106] "Dimethenamid-P"                       "Diméthénamide"                        "Diméthoate"                          
## [109] "Diméthomorphe"                        "Diphényléthers bromés"                "Diquat"                              
## [112] "Diuron"                               "E. coli"                              "Endosulfan"                          
## [115] "Entérocoques"                         "Epoxiconazole"                        "Ethidimuron"                         
## [118] "Ethofumésate"                         "Ethoprophos"                          "Ethylbenzène"                        
## [121] "Ethylenethiouree"                     "Fénamidone"                           "Fenbuconazole"                       
## [124] "Fenpropidine"                         "Fenpropimorphe"                       "Flonicamid"                          
## [127] "Florasulam"                           "Fluoranthène"                         "Flurochloridone"                     
## [130] "Fluroxypyr"                           "Flurtamone"                           "Flutolanil"                          
## [133] "Fomesafen"                            "Foramsulfuron"                        "Formaldéhyde (Formol)"               
## [136] "Fosthiazate"                          "GFI (IBGN)"                           "Glufosinate"                         
## [139] "Glufosinate-ammonium"                 "Glyphosate"                           "HBCDD"                               
## [142] "Heptachlore"                          "Heptachlore époxyde endo trans"       "Heptachlore époxyde exo cis"         
## [145] "Heptachlore et époxyde d'heptachlore" "Hexachlorobenzène"                    "Hexachlorobutadiène"                 
## [148] "Hexachlorocyclohexane"                "Hexachlorocyclohexane alpha"          "Hexachlorocyclohexane bêta"          
## [151] "Hexachlorocyclohexane delta"          "Hexachlorocyclohexane gamma"          "I2M2"                                
## [154] "IBD"                                  "IBGN"                                 "IBMR"                                
## [157] "Imazaméthabenz"                       "Imazamox"                             "Imazaquine"                          
## [160] "Imidaclopride"                        "iodosulfuron-methyl-sodium"           "Ioxynil"                             
## [163] "IPR"                                  "Iprodione"                            "Irgarol"                             
## [166] "Isoproturon"                          "Isoxaben"                             "Isoxaflutole"                        
## [169] "KRESOXIM-METHYL"                      "Lénacile"                             "Linuron"                             
## [172] "Mancozèbe"                            "Mécoprop"                             "Mépiquat chlorure"                   
## [175] "Mercure"                              "Mercure dissous"                      "MES"                                 
## [178] "Mesosulfuron methyle"                 "Mésotrione"                           "Métalaxyl"                           
## [181] "Métaldéhyde"                          "Métamitrone"                          "Métazachlore"                        
## [184] "Metconazole"                          "Méthabenzthiazuron"                   "Métobromuron"                        
## [187] "Metolachlor ESA"                      "Metolachlor OXA"                      "Métolachlore total"                  
## [190] "Métosulame"                           "Métribuzine"                          "Metsulfuron méthyle"                 
## [193] "N-Nitrosomorpholine"                  "Naphtalène"                           "Napropamide"                         
## [196] "NH4"                                  "Nickel"                               "Nickel dissous"                      
## [199] "Nicosulfuron"                         "NKJ"                                  "NO2"                                 
## [202] "NO3"                                  "Nonylphénols"                         "O2 dissous"                          
## [205] "Octylphénols"                         "Oryzalin"                             "Ovovivip. (I2M2)"                    
## [208] "Oxadiazon"                            "Oxadixyl"                             "P total"                             
## [211] "Paclobutrazole"                       "para-para-DDT"                        "Paraquat"                            
## [214] "Pencycuron"                           "Pendiméthaline"                       "Pentachlorobenzène"                  
## [217] "Pentachlorophénol"                    "Perchlorate"                          "Pesticides cyclodiènes"              
## [220] "PFOS"                                 "pH"                                   "Phosphate de tributyle"              
## [223] "Piclorame"                            "Pirimicarbe"                          "Plomb"                               
## [226] "Plomb dissous"                        "PO4"                                  "Polyvoltin. (I2M2)"                  
## [229] "Prochloraz"                           "Propachlore"                          "Propamocarb"                         
## [232] "Propiconazole"                        "Propoxycarbazone-sodium"              "Propyzamide"                         
## [235] "prosulfocarbe"                        "Prosulfocarbe"                        "Prosulfuron"                         
## [238] "Prothioconazole"                      "Pymétrozine"                          "Pyriméthanil"                        
## [241] "Pyroxsulam"                           "Quinmerac"                            "Quinoxyfen"                          
## [244] "Quinoxyfène"                          "Rich. Tax. (I2M2)"                    "SATUR. O2"                           
## [247] "Score Nb lithophiles (NEL)"           "Score Nb rhéophiles (NER)"            "Score Nb Tot Sp. (NTE)"              
## [250] "Shannon (I2M2)"                       "Silthiopham"                          "Simazine"                            
## [253] "Somme des Hexachlorocyclohexanes"     "Somme des pesticides totaux"          "Somme Heptachlore époxyde cis/trans" 
## [256] "Spiroxamine"                          "Sulcotrione"                          "Sulfosulfuron"                       
## [259] "Tebuconazole"                         "Tébuconazole"                         "Tébutame"                            
## [262] "TEMPERATURE"                          "Terbuthylazine"                       "Terbuthylazine désethyl"             
## [265] "Terbutryne"                           "Tétrachloroéthylène"                  "Tétrachlorure de carbone"            
## [268] "Tetraconazole"                        "Thiabendazole"                        "Thiafluamide"                        
## [271] "Thiamethoxam"                         "Thifensulfuron méthyl"                "Thiophanate-méthyl"                  
## [274] "Toluène"                              "Triadiménol"                          "Triazoxide"                          
## [277] "Tributylétain cation"                 "Trichlorobenzène"                     "Trichloroéthylène"                   
## [280] "Trichlorométhane"                     "Triclopyr"                            "Trifluraline"                        
## [283] "Triflusulfuron-methyl"                "Trinexapac-ethyl"                     "Triticonazole"                       
## [286] "Tritosulfuron"                        "Turbidité"                            "Var. taxo. (IBGN)"                   
## [289] "Xylène"                               "Zinc"                                 "Zinc dissous"
```
# Exemples de graphs en mode points


```r
data0<-data.frame(DatePrel=c("2019-01-01 12:30:00", "2020-05-03 00:00:00","2020-10-25 12:30:00", "2021-07-18 12:30:00")%>%as.POSIXct(tz="Europe/Paris"), RsAna=c(12,35.5,58.3, 42), LqAna=c(3))
graphDCE_points(data0, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), affiche_LQ = T, ymini = 0)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)


## représentation des points hors gamme de mesure

En cas de points hors de l'échelle, la fonction trace remplace ces points par une étiquette colorée qui reprend la valeur du point manquant affiché de la couleur correspondant à sa classe de qualité.


```r
graphDCE_points(data0, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), ymaxi=55, titre = "Nitrates", bilan_annuel = T, nom_legende = "Classe \nqualité")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
En l'état, la fonction graphDCE_points ne permet pas d'afficher de légende pour l'affichage des LQ.
Ce manque peut être contourné en combinant la fonction legend_LQ() avec la fonction plot_grid du package cowplot.


```r
graph<-graphDCE_points(data0, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), affiche_LQ = T, ymini = 0)
plot_grid(graph, legend_LQ(), rel_heights = c(9,1), ncol=1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)


# graphs en barre
On peut également réaliser des graphiques en barre, par exemple pour faire des bilans annuels

```r
data<-data.frame(annee=seq(2010,2013), RsAna=c(12,15.5,67,18.3))
graphDCE_bar(data, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"),ymaxi=60)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
# Attribution d'une classe de qualité à des résultats
La fonction affecte une classe permet d'affecter une classe à un résultat

```r
affecte_une_classe(c(10,10.1,50, 50.1), seuil=makeSeuils(CdParametre="1340", type_seuil = "DCE"))
```

```
## [1] TRES BON BON      BON      MOYEN   
## Levels: TRES BON BON MOYEN
```




# Bilan sur les taux de quantification

Le package permet de réaliser des graphiques bilans sur les taux de quantification.


```r
donnees<-data.frame(Parametre=rep(letters[1:5], 100), RsAna=sample(0.1:100, 500, replace=TRUE), LqAna=c(0.5,1,2,6))
donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
seuils<-makeSeuils(type_seuil="DCE", CdParametre = "1340")
table_distribution(donnees, seuil=seuils)
```

```
## # A tibble: 4 x 6
## # Groups:   CLASSE [3]
##   CLASSE   CdRqAna    nb CATEGORIE                    NOM_COULEUR ALPHA
##   <fct>    <chr>   <int> <chr>                        <chr>       <dbl>
## 1 TRES BON 1          37 TRES BON                     dodgerblue    1  
## 2 TRES BON 10         14 <LQ et LQ de classe TRES BON dodgerblue    0.2
## 3 BON      1         203 BON                          green1        1  
## 4 MOYEN    1         246 MOYEN                        yellow        1
```

Il permet également de faire le bilan par paramètre

```r
donnees<-data.frame(parametres=rep(c("1301", "1340", "1335"), 100), RsAna=sample(0.1:100, 300, replace=TRUE), LqAna=c(0.5,1,6))
donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
seuils<-makeSeuils(CdParametre=donnees$parametres%>%unique, specificites=c("CYPRINICOLE", rep(NA,2)), type_seuil = "DCE")
bilan<-groupe_tableau_distribution(donnees, col_CdParametre="parametres", col_CdSupport=NULL, col_CdFraction=NULL, col_CdUnite=NULL, seuils = seuils)
print(bilan)
```

```
## # A tibble: 10 x 11
## # Groups:   CLASSE [5]
##    CLASSE   CdRqAna    nb CATEGORIE                    NOM_COULEUR ALPHA parametre   code_parametre code_support code_fraction code_unite
##    <fct>    <chr>   <int> <fct>                        <chr>       <dbl> <chr>       <chr>          <chr>        <chr>         <chr>     
##  1 TRES BON 1          10 TRES BON                     dodgerblue    1   NO3         1340           3            <NA>          162       
##  2 BON      1          37 BON                          green1        1   NO3         1340           3            <NA>          162       
##  3 MOYEN    1          53 MOYEN                        yellow        1   NO3         1340           3            <NA>          162       
##  4 MAUVAIS  1          93 MAUVAIS                      red           1   NH4         1335           3            <NA>          162       
##  5 MAUVAIS  10          7 <LQ et LQ de classe MAUVAIS  red           0.2 NH4         1335           3            <NA>          162       
##  6 TRES BON 1          24 TRES BON                     dodgerblue    1   TEMPERATURE 1301           3            <NA>          27        
##  7 TRES BON 10          1 <LQ et LQ de classe TRES BON dodgerblue    0.2 TEMPERATURE 1301           3            <NA>          27        
##  8 BON      1           2 BON                          green1        1   TEMPERATURE 1301           3            <NA>          27        
##  9 MEDIOCRE 1           1 MEDIOCRE                     orange        1   TEMPERATURE 1301           3            <NA>          27        
## 10 MAUVAIS  1          72 MAUVAIS                      red           1   TEMPERATURE 1301           3            <NA>          27
```

et de faire une sortie graphique de ce bilan


```r
graphDCE_distribution(bilan, titre="Bilan par paramètre") 
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

# Ajout d'informations à partir du référentiel SANDRE

## Ajout du nom de paramètre à partir de codes paramètres
La fonction ajoute_nom_param permet d'ajouter à une data frame qui contient des codes paramètres SANDRE les noms correspondants


```r
donnees<-data.frame(code=c("1301", "1302", "1303"))
ajoute_nom_param(donnees, col_parametre="code")
```

```
##   code                NomParametre
## 1 1301        Température de l'Eau
## 2 1302 Potentiel en Hydrogène (pH)
## 3 1303         Conductivité à 25°C
```


