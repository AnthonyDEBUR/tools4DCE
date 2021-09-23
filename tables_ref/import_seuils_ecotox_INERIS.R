library(tidyverse)
library(tools4DCE)
library(openxlsx)

# creation fichier xlsx à ajouter à la base seuils. Ce fichier contient seulement les seuils INERIS ecotox
# https://substances.ineris.fr/fr/page/9
donnees_ineris <-
  read.xlsx("tables_ref\\DRC-18-158732-03350A_NQE-VGE_INERIS_v20180416.xlsx",
            sheet = "Toutes valeurs seuils_201804") %>% subset(!is.na(CODE.SANDRE)) %>%
  mutate(CdParametre = CODE.SANDRE %>% as.character) %>% ajoute_nom_param()

data("base_seuils")

base_seuils <- base_seuils %>% subset(TYPE == "NON_DCE")
donnees_ineris <-
  donnees_ineris %>% subset(!CdParametre %in% base_seuils$PARAMETRE)
donnees_ineris$TYPE <- "NON_DCE"
donnees_ineris$NOM_SEUIL <-
  ifelse(
    donnees_ineris$Référence.fiche != "Pas de fiche disponible",
    donnees_ineris$Référence.fiche,
    donnees_ineris$Source
  )



# NQE concentration moyenne (hors AEP)
donnees_ineris$NQE_moy <-
  ifelse(
    donnees_ineris$`AA-EQS.FW.eau.non.destinée.à.la.production.d'eau.potable.(µg/L)` !=
      "cf. EQS biota",
    donnees_ineris$`AA-EQS.FW.eau.non.destinée.à.la.production.d'eau.potable.(µg/L)`,
    donnees_ineris$`AA-QS.FW.ECO.[µg/L]`
  )
donnees_ineris$NQE_moy <-
  gsub("Pas de valeur", NA, donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  gsub("Non calculée", NA, donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  gsub("Données insuffisantes", NA, donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  gsub("cf. sum of BDEs", NA, donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  gsub("cf. commercial mixture CAS608-73-1",
       NA,
       donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  gsub("cf. Specific PAH", NA, donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  gsub("cf. Remarque" , NA, donnees_ineris$NQE_moy)
donnees_ineris$NQE_moy <-
  donnees_ineris$NQE_moy %>% as.numeric() %>% as.character()


# Concentrations max admissibles
donnees_ineris$CMA <- donnees_ineris$`MAC-EQS.FW.(µg/L)`
donnees_ineris$CMA <- gsub("Pas de valeur", NA, donnees_ineris$CMA)
donnees_ineris$CMA <- gsub("Non calculée", NA, donnees_ineris$CMA)
donnees_ineris$CMA <-
  gsub("Données insuffisantes", NA, donnees_ineris$CMA)
donnees_ineris$CMA <-
  donnees_ineris$CMA %>% as.numeric() %>% as.character()

# selon si NQE, CMA ou NQE et CMA renseignés, on a différents seuils
donnees_ineris$TYPE_SEUIL <-
  paste0(ifelse(!is.na(donnees_ineris$NQE_moy), "NQE_", ""), ifelse(!is.na(donnees_ineris$CMA), "CMA", ""))
donnees_ineris <- donnees_ineris %>% subset(TYPE_SEUIL != "")

# cas des substances avec seulement NQE_moy
table0 <-
  data.frame(
    TYPE_SEUIL = "NQE_",
    SEUILMIN = c("-Inf", "NQE"),
    SEUILMAX = c("NQE", "Inf"),
    CLASSE = c("<NQE moyen", ">NQE moyen")
  )
donnees_ineris0 <- inner_join(donnees_ineris, table0, by = "TYPE_SEUIL")

# cas des substances avec seulement CMA
table0 <-
  data.frame(
    TYPE_SEUIL = "CMA",
    SEUILMIN = c("-Inf", "CMA"),
    SEUILMAX = c("CMA", "Inf"),
    CLASSE = c("< CMA", "> CMA")
  )
donnees_ineris1 <- inner_join(donnees_ineris, table0, by = "TYPE_SEUIL")

# cas des substances avec NQE et CMA
table0 <-
  data.frame(
    TYPE_SEUIL = "NQE_CMA",
    SEUILMIN = c("-Inf", "NQE", "CMA"),
    SEUILMAX = c("NQE", "CMA", "Inf"),
    CLASSE = c("<NQE moyen", "[NQE moyen - CMA[", "> CMA")
  )
donnees_ineris2 <- inner_join(donnees_ineris, table0, by = "TYPE_SEUIL")

# fusion des tableaux
donnees_ineris <-
  bind_rows(donnees_ineris0, donnees_ineris1, donnees_ineris2)

donnees_ineris[donnees_ineris$SEUILMIN == "NQE", ]$SEUILMIN <-
  donnees_ineris[donnees_ineris$SEUILMIN == "NQE", ]$NQE_moy
donnees_ineris[donnees_ineris$SEUILMAX == "NQE", ]$SEUILMAX <-
  donnees_ineris[donnees_ineris$SEUILMAX == "NQE", ]$NQE_moy

donnees_ineris[donnees_ineris$SEUILMIN == "CMA", ]$SEUILMIN <-
  donnees_ineris[donnees_ineris$SEUILMIN == "CMA", ]$CMA
donnees_ineris[donnees_ineris$SEUILMAX == "CMA", ]$SEUILMAX <-
  donnees_ineris[donnees_ineris$SEUILMAX == "CMA", ]$CMA

donnees_ineris$NOM <- donnees_ineris$NomParametre
donnees_ineris$SUPPORT <- "3"
donnees_ineris$FRACTION <- "23"
donnees_ineris$PARAMETRE <- donnees_ineris$CdParametre
donnees_ineris$UNITE <- "133"
donnees_ineris$TYPE_BORNE <- "BORNE_INF_INCLUE"

donnees_ineris$SEUILMIN <- donnees_ineris$SEUILMIN %>% as.character()
donnees_ineris$SEUILMAX <- donnees_ineris$SEUILMAX %>% as.character()

donnees_ineris$SPECIFICITE <- ""

donnees_ineris <-
  donnees_ineris %>% select(
    NOM,
    SUPPORT,
    FRACTION,
    PARAMETRE,
    UNITE,
    SEUILMIN,
    SEUILMAX,
    CLASSE,
    NOM_SEUIL,
    TYPE,
    TYPE_BORNE,
    SPECIFICITE
  )

write.xlsx(donnees_ineris, "tables_ref/base_seuils_ineris.xlsx")
