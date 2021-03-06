library(tidyverse)
library(readxl)
library(devtools)
library(stringi)

# base des seuils par paramètre
base_seuils <-
  read_xlsx("tables_ref/base_seuils.xlsx", col_types =  c(rep("text", 5), rep("numeric", 2), rep("text", 5)))
save(base_seuils, file = "data/base_seuils.RData")


# couleur des classes de qualité
couleurs_classes <- read.csv2("tables_ref/couleurs_classes.csv")
save(couleurs_classes, file = "data/couleurs_classes.RData")


# ordre des facteurs de qualité
ordre_facteurs_qualite <-
  read.csv2("tables_ref/ordre_facteurs_qualite.csv")
names(ordre_facteurs_qualite) <- "CLASSE"
save(ordre_facteurs_qualite, file = "data/ordre_facteurs_qualite.RData")

# telechargement du referentiel unites sandre
download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/urf.csv?compress=true",
  "unites.csv.gz",
  mode = "wb",
  cacheOK = T
)
unites_sandre <- read_delim("unites.csv.gz", delim = ";", skip = 1)
file.remove("unites.csv.gz")

colnames(unites_sandre)<-stringi::stri_trans_general(colnames(unites_sandre), "Latin-ASCII")

save(unites_sandre, file = "data/unites_sandre.RData")

# telechargement du referentiel paramètres sandre
download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/par.csv?outputSchema=SANDREv4&compress=true",
  "param.csv.gz",
  mode = "wb",
  cacheOK = T
)
parametres_sandre <-
  read_delim("param.csv.gz", delim = ";", skip = 1)
parametres_sandre$CdParametre <-
  as.character(parametres_sandre$CdParametre)
# suppression des colonnes remplies de NA
parametres_sandre <-
  Filter(function(x)
    ! all(is.na(x)), parametres_sandre)
file.remove("param.csv.gz")
save(parametres_sandre, file = "data/parametres_sandre.RData")
