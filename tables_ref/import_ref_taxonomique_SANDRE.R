library(tidyverse)
library(tools4DCE)

#chargement du référentiel taxonomique du SANDRE
# https://www.sandre.eaufrance.fr/atlas/srv/eng/catalog.search#/metadata/0bea2eb9-a4ee-4a81-b494-bd4fb5338ef8

ref_taxo_sandre<-read.csv2("http://mdm.sandre.eaufrance.fr/mdm_sandre/exports/codes_taxonomiques", encoding="UTF-8")
ref_taxo_sandre$CdAppelTaxon<-as.character(ref_taxo_sandre$CdAppelTaxon)


save(ref_taxo_sandre, file="data/ref_taxo_sandre.RData")
