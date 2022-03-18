#' calcule_SEEE_IBMR
#'
#' fonction pour traiter une liste de diatomées via le script IBMR du SEEE
#' voir : https://seee.eaufrance.fr/ outil d'évaluation IBMR v1.1.5
#'
#' Les colonnes à passer impérativement dans la table d'entrée sont celles prévues dans le script :
#' -  CODE_OPERATION : identifiant unique de l'opération
#' - CODE_STATION : code SANDRE de la station de mesure
#' - DATE : date de l'opération (format "%d/%m/%Y")
#' - CODE_TAXON : code SANDRE du taxon
#' - RESULTAT : Pourcentage de recouvrement du taxon identifié. Le champ est obligatoire et compris entre 0 et 100
#' - UR : Unité de relevé. Le champ est obligatoire et seules les modalités F1, F2 et FU sont acceptées
#' - POURCENTAGE_FACIES :  Pourcentage du faciès dominant dans l’unité de relevé. compris entre 0 et 100.
#'
#' @param donnees = data.table avec les colonnes requises par le script
#'
#'
#' @return Résultats outil IBMR SEEE
#'
#' @examples stations_op<-import_hubeau_indices_hbio(liste_stations = "04207400", indice="mphy")
#' @examples stations_op<-stations_op%>%subset(CdParametre=="2928")
#' @examples donnees<-donnees%>%subset(libelle_type_resultat=="RecTax")
#' @examples donnees$CODE_OPERATION <-paste0(donnees$code_station_hydrobio, "*", donnees$date_prelevement)
#' @examples donnees$CODE_STATION <- donnees$code_station_hydrobio
#' @examples donnees$DATE <- format(donnees$date_prelevement, "%d/%m/%Y")
#' @examples donnees$UR<-donnees$code_lot
#' @examples donnees$CODE_TAXON <- donnees$code_appel_taxon
#' @examples donnees$RESULTAT <- donnees$resultat_taxon*100
#' @examples  # nota : dans Hub Eau les pourcentages de recouvrement de chaque faciès ne sont pas fourni ce qui rend impossible le calcul de l'IBMR
#' @examples donnees$POURCENTAGE_FACIES <-ifelse(donnees$UR=="FU", 100, 50)
#' @examples donnees <- donnees %>% select(CODE_OPERATION, CODE_STATION, DATE, CODE_TAXON, RESULTAT, UR, POURCENTAGE_FACIES)
#' @examples donnees<-donnees%>%subset(!is.na(CODE_TAXON))
#' @examples calcule_SEEE_IBMR(donnees)
#'
#' @export

calcule_SEEE_IBMR <- function(donnees)
{
  if (!("data.frame" %in% class(donnees))) {
    stop("donnees doit etre un data.frame")
  }


  nom_colonnes_voulu <-
    c(
      "CODE_OPERATION",
      "CODE_STATION",
      "DATE",
      "CODE_TAXON",
      "RESULTAT",
      "UR",
      "POURCENTAGE_FACIES"
    )


  if (!all(nom_colonnes_voulu %in% names(donnees))) {
    stop(paste0(
      "Nom(s) de colonnes manquant dans le fichier de données : ",
      paste(nom_colonnes_voulu[!(nom_colonnes_voulu %in% names(donnees))], collapse = ", ")


    ))
  }

  # conversion des codes taxon du SANDRE vers OMNIDIA
  ref_taxo_sandre0 <-
    tools4DCE::ref_taxo_sandre %>% select(CdAppelTaxon, MPHYT_IRSTEA)

  donnees <-
    donnees %>% left_join(ref_taxo_sandre0, by = c("CODE_TAXON" = "CdAppelTaxon"))
  donnees$CODE_TAXON <- donnees$MPHYT_IRSTEA



  url_base <-
    paste0("https://seee.eaufrance.fr/api/calcul?indicateur=IBMR&version=1.1.5")


  fichier_tmp <- tempfile(fileext = ".csv")
  write.table(donnees, fichier_tmp, sep = "\t", row.names = F)

  test <-
    POST(
      url_base,
      query = list(`sortie[complementaire]` = "1"),
      body = list(`multipart/form-data` = upload_file(fichier_tmp, type = ".csv")) ,
      encode = "multipart"
    )

  httr::warn_for_status(test)

  data <- test %>%
    httr::content(as = 'text')

  data <- strsplit(data, '\n[1] \"Fichier\"\n', fixed = TRUE)

  data_base <- read.csv2(text = data[[1]][1], skip = 1)
  data_comp <- read.csv2(text = data[[1]][2], skip = 1)

  data_comp$CODE_PAR<-NA
  data_comp$COMMENTAIRES<-""

  data <- rbind(data_base, data_comp)
  if(length(unique(donnees$CODE_OPERATION))>nrow(data)){warning("Attention, certains résultats d'opération ne figurent pas dans le fichier résultat.")}


  if (length(unique(donnees$CODE_OPERATION)) > length(unique(data$CODE_OPERATION))) {
    warning(
      "Attention, certains résultats d'opération ne figurent pas dans le fichier résultat."
    )
  }



  # # mise en forme des résultats
  data$RESULTAT <- data$RESULTAT %>% as.numeric()
  data$CODE_PAR <- as.character(data$CODE_PAR)

  return(data)
}
