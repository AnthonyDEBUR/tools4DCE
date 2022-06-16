library(tidyverse)
library(readxl)
library(devtools)
library(stringi)
library(tools4DCE)

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

# valeurs de reference EQR
base_ref_eqr <-
  read_xlsx("tables_ref/base_reference_EQR.xlsx",
            col_types =  c(rep("text", 3), rep("numeric", 2), rep("text", 1)))
base_ref_eqr$TYPEFR <- factor(base_ref_eqr$TYPEFR)
save(base_ref_eqr, file = "data/base_ref_eqr.RData")


# telechargement du referentiel unites sandre
download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/urf.csv?compress=true",
  "unites.csv.gz",
  mode = "wb",
  cacheOK = T
)
unites_sandre <- read_delim("unites.csv.gz", delim = ";", skip = 1)
file.remove("unites.csv.gz")

colnames(unites_sandre) <-
  stringi::stri_trans_general(colnames(unites_sandre), "Latin-ASCII")

unites_sandre <-
  unites_sandre %>% dplyr::rename(
    "CdUniteMesure" = "Code de l'unite de reference",
    "SymUniteMesure" = "Symbole de l'unite de reference",
    "LblUniteMesure" = "Libelle de l'unite de reference"
  )

save(unites_sandre, file = "data/unites_sandre.RData")

# telechargement du referentiel paramètres sandre
download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/par.csv?outputSchema=SANDREv4&compress=true",
  "param.csv.gz",
  mode = "wb",
  cacheOK = T
)
Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

parametres_sandre <-
  read_delim("param.csv.gz", delim = ";")

parametres_sandre <-
  parametres_sandre %>% subset(CdParametre != "Code du paramètre")


parametres_sandre$CdParametre <-
  as.character(parametres_sandre$CdParametre)
# suppression des colonnes remplies de NA
parametres_sandre <-
  Filter(function(x)
    ! all(is.na(x)), parametres_sandre)
file.remove("param.csv.gz")
save(parametres_sandre, file = "data/parametres_sandre.RData")



# Support

# telechargement du referentiel support sandre
download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/sup.csv?outputSchema=SANDREv4&compress=true",
  "support.csv.gz",
  mode = "wb",
  cacheOK = T
)
Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

supports_sandre <-
  read_delim("support.csv.gz", delim = ";")

file.remove("support.csv.gz")

colnames(supports_sandre) <-
  stringi::stri_trans_general(colnames(supports_sandre), "Latin-ASCII")

supports_sandre <-
  supports_sandre %>% dplyr::select(CdSupport, LbSupport)

save(supports_sandre, file = "data/supports_sandre.RData")



# Fraction

# telechargement du referentiel fraction sandre
download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/fan.csv?outputSchema=SANDREv4&compress=true",
  "fraction.csv.gz",
  mode = "wb",
  cacheOK = T
)
Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

fractions_sandre <-
  read_delim("fraction.csv.gz", delim = ";")

file.remove("fraction.csv.gz")

colnames(fractions_sandre) <-
  stringi::stri_trans_general(colnames(fractions_sandre), "Latin-ASCII")

fractions_sandre <-
  fractions_sandre %>% dplyr::select(CdFractionAnalysee, LbFractionAnalysee)

save(fractions_sandre, file = "data/fractions_sandre.RData")


# Réseaux de mesure

# telechargement du referentiel dispositifs de collecte du sandre
options(timeout = 5*60)

curl::curl_download(
  "https://api.sandre.eaufrance.fr/referentiels/v1/dc.csv?outputSchema=SANDREv4&compress=true",
  "reseaux.csv.gz",
  mode = "wb"
)

Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

fractions_sandre <-
  read_delim("reseaux.csv.gz", delim = ";")

file.remove("reseaux.csv.gz")

colnames(fractions_sandre) <-
  stringi::stri_trans_general(colnames(fractions_sandre), "Latin-ASCII")

fractions_sandre <-
  fractions_sandre %>% dplyr::select(CdFractionAnalysee, LbFractionAnalysee)

save(fractions_sandre, file = "data/fractions_sandre.RData")


# Stations

# telechargement du referentiel stations sandre

stations <- charge_shp_STAQ()
# stations$X <- st_coordinates(stations)[, 1]
# stations$Y <- st_coordinates(stations)[, 2]
stations <-
  stations %>% st_drop_geometry %>% select(CdStationMesureEauxSurface, LbStationMesureEauxSurface)



save(stations, file = "data/stations.RData")


# Intervenants

# telechargement du referentiel Intervenants sandre


download.file(
  "https://api.sandre.eaufrance.fr/referentiels/v1/int.json?outputSchema=SANDREv2&compress=true",
  "intervenants.csv.gz",
  mode = "wb",
  cacheOK = T,
  extra=options(timeout=600)
)

# télécharger à la main le référentiel à l'adresse suivante
# https://api.sandre.eaufrance.fr/referentiels/v1/int.csv?outputSchema=SANDREv2&compress=true


Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)

# choisir le fichier téléchargé pour le dézipper et le lire
fichier<-file.choose()
intervenants<-read_delim(fichier, delim = ";", skip = 0)
intervenants<-intervenants[2:nrow(intervenants),] # suppression de la 2ème ligne du fichier avec les descriptifs des noms de champs
#
# intervenants <-
#   read_delim("intervenants.csv.gz", delim = ";")
#
# file.remove("intervenants.csv.gz")

colnames(intervenants) <-
  stringi::stri_trans_general(colnames(intervenants), "Latin-ASCII")

intervenants_sandre <-
  intervenants %>% dplyr::select(CdIntervenant, NomIntervenant, MnIntervenant)

save(intervenants_sandre, file = "data/intervenants_sandre.RData")
