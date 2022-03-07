#' calcule_SEEE_IBG_DCE
#'
#' fonction pour traiter une liste I2M2 via le script diagnostic invertébrés du SEEE
#' voir : https://seee.eaufrance.fr/ outil de diagnostic invertébrés v. 1.0.2
#'
#' Les colonnes à passer impérativement dans la table d'entrée sont celles prévues dans le script :
#' -  CODE_OPERATION : identifiant unique de l'opération
#' - CODE_STATION : code SANDRE de la station de mesure
#' - DATE : date de l'opération (format "%d/%m/%Y")
#' - TYPONATIONALE : Typologie nationale de la station (ex. TP12-A)
#' - CODE_PHASE : code de la phase de prélèvement (A, B ou C)
#' - CODE_TAXON : code SANDRE du taxon
#' - RESULTAT : effectif du taxon
#' - CODE_REMARQUE : code remarque selon le SANDRE
#'
#'
#' @param donnees = data.table avec les colonnes requises par le script
#'
#'
#' @return Résultats outil IBG-DCE invertébrés SEEE
#'
#' @examples stations_op<-import_hubeau_indices_hbio(liste_stations = "04207400", indice="inv")
#' @examples stations_op<-stations_op%>%subset(CdParametre=="7613")
#' @examples donnees<-import_hubeau_liste_hbio(liste_stations = c(station_etudiee), indice="inv")
#' @examples donnees<-donnees%>%subset(date_prelevement%in%stations_op$DatePrel)
#' @examples donnees$CODE_OPERATION <-
#' @examples paste0(donnees$code_station_hydrobio, "*", donnees$date_prelevement)
#' @examples donnees$CODE_STATION <- donnees$code_station_hydrobio
#' @examples donnees$DATE <- format(donnees$date_prelevement, "%d/%m/%Y")
#' @examples donnees$TYPO_NATIONALE <- "P12-A"
#' @examples donnees$CODE_PHASE <- donnees$code_lot
#' @examples donnees$CODE_TAXON <- donnees$code_appel_taxon
#' @examples donnees$RESULTAT <- donnees$resultat_taxon
#' @examples donnees$CODE_REMARQUE <- donnees$code_type_resultat
#' @examples donnees <- donnees %>% select(CODE_OPERATION, CODE_STATION, DATE, TYPONATIONALE, CODE_PHASE, CODE_TAXON, RESULTAT, CODE_REMARQUE)
#' @examples calcule_SEEE_IBG_DCE(donnees)
#'
#' @export

calcule_SEEE_IBG_DCE <- function(donnees)
{
  if (!("data.frame" %in% class(donnees))) {
    stop("donnees doit etre un data.frame")
  }

  nom_colonnes_voulu <- c(
    "CODE_OPERATION",
    "CODE_STATION",
    "DATE",
    "TYPO_NATIONALE",
    "CODE_PHASE",
    "CODE_TAXON",
    "RESULTAT",
    "CODE_REMARQUE"
  )


  if (!all(nom_colonnes_voulu %in% names(donnees))) {
    stop(paste0(
      "Nom(s) de colonnes manquant dans le fichier de données : ",
      paste(nom_colonnes_voulu[!(nom_colonnes_voulu %in% names(donnees))], collapse = ", ")


    ))
  }



  url_base <-
    paste0("https://seee.eaufrance.fr/api/calcul?indicateur=IBG-DCE&version=1.0.6")


  fichier_tmp <- tempfile(fileext = ".csv")
  write.table(donnees, fichier_tmp, sep = "\t", row.names = F)

  test <-
    POST(
      url_base,
      body = list(`multipart/form-data` = upload_file(fichier_tmp, type = ".csv")) ,
      encode = "multipart"
    )

  test$url
  httr::warn_for_status(test)

  data <- test %>%
    httr::content(as = 'text')

  data <- read.csv2(text = data, skip = 1)


  if (length(unique(donnees$CODE_OPERATION)) >length(unique(data$CODE_OPERATION))) {
    warning(
      "Attention, certains résultats d'opération ne figurent pas dans le fichier résultat."
    )
  }

  # mise en forme des résultats
  data$RESULTAT <- data$RESULTAT%>%as.numeric()
  data$CODE_STATION <- paste0("0", data$CODE_STATION)
  data$CODE_PAR<-as.character(data$CODE_PAR)

  return(data)
}
