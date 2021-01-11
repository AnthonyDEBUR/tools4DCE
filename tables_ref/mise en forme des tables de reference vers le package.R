library(tidyverse)
library(readxl)

# base des seuils par paramètre
base_seuils <- read_xlsx("tables_ref/base_seuils.xlsx", col_types =  c(rep("text",5),rep("numeric",2), rep("text",5)))
save(base_seuils, file="data/base_seuils.RData")


# couleur des classes de qualité
couleurs_classes <- read.csv2("tables_ref/couleurs_classes.csv")
save(couleurs_classes, file="data/couleurs_classes.RData")


# ordre des facteurs de qualité
ordre_facteurs_qualite <- read.csv2("tables_ref/ordre_facteurs_qualite.csv")
names(ordre_facteurs_qualite)<-"CLASSE"
save(ordre_facteurs_qualite, file="data/ordre_facteurs_qualite.RData")

# telechargement du referentiel unites sandre
download.file("https://api.sandre.eaufrance.fr/referentiels/v1/urf.csv?compress=true", "unites.csv.gz", mode="wb", cacheOK = T)
unites_sandre<-read_delim("unites.csv.gz",delim=";", skip=1)
file.remove("unites.csv.gz")
save(unites_sandre, file="data/unites_sandre.RData")
