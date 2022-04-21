#' calcule_SEEE_IBD
#'
#' fonction pour traiter une liste de diatomées via le script IBD du SEEE
#' voir : https://seee.eaufrance.fr/ outil d'évaluation IBD v1.2.4
#'
#' Les colonnes à passer impérativement dans la table d'entrée sont celles prévues dans le script :
#' -  CODE_OPERATION : identifiant unique de l'opération
#' - CODE_STATION : code SANDRE de la station de mesure
#' - DATE : date de l'opération (format "%d/%m/%Y")
#' - CODE_TAXON : code SANDRE du taxon
#' - RESULTAT : effectif du taxon
#'
#'
#' @param donnees = data.table avec les colonnes requises par le script
#'
#'
#' @return Résultats outil IBD SEEE
#'
#' @examples stations_op<-import_hubeau_indices_hbio(liste_stations = "04207400", indice="dia")
#' @examples stations_op<-stations_op%>%subset(CdParametre=="5856")
#' @examples donnees<-import_hubeau_liste_hbio(liste_stations = c(station_etudiee), indice="dia")
#' @examples donnees<-donnees%>%subset(date_prelevement%in%stations_op$DatePrel)
#' @examples donnees$CODE_OPERATION <-
#' @examples paste0(donnees$code_station_hydrobio, "*", donnees$date_prelevement)
#' @examples donnees$CODE_STATION <- donnees$code_station_hydrobio
#' @examples donnees$DATE <- format(donnees$date_prelevement, "%d/%m/%Y")
#' @examples donnees$CODE_TAXON <- donnees$code_appel_taxon
#' @examples donnees$RESULTAT <- donnees$resultat_taxon
#' @examples donnees <- donnees %>% select(CODE_OPERATION, CODE_STATION, DATE, CODE_TAXON, RESULTAT)
#' @examples calcule_SEEE_IBD(donnees)
#'
#' @export

calcule_SEEE_IBD <- function(donnees)
{
  if (!("data.frame" %in% class(donnees))) {
    stop("donnees doit etre un data.frame")
  }

  nom_colonnes_voulu <- c("CODE_OPERATION",
                          "CODE_STATION",
                          "DATE",
                          "CODE_TAXON",
                          "RESULTAT")


  if (!all(nom_colonnes_voulu %in% names(donnees))) {
    stop(paste0(
      "Nom(s) de colonnes manquant dans le fichier de données : ",
      paste(nom_colonnes_voulu[!(nom_colonnes_voulu %in% names(donnees))], collapse = ", ")


    ))
  }

  # conversion des codes taxon du SANDRE vers OMNIDIA
  ref_taxo_sandre0 <-
    tools4DCE::ref_taxo_sandre %>% dplyr::select(CdAppelTaxon, REF_OMNIDIA, SYN_OMNIDIA, TERA_OMNIDIA)

  donnees <-
    donnees %>% left_join(ref_taxo_sandre0, by = c("CODE_TAXON" = "CdAppelTaxon"))
  donnees$CODE_TAXON <-
    paste0(donnees$REF_OMNIDIA,
           donnees$SYN_OMNIDIA,
           donnees$TERA_OMNIDIA)



  url_base <-
    paste0("https://seee.eaufrance.fr/api/calcul?indicateur=IBD&version=1.2.4")


  fichier_tmp <- tempfile(fileext = ".csv")
  write.table(donnees, fichier_tmp, sep = "\t", row.names = F)

  test <-
    POST(
      url_base,
      body = list(`multipart/form-data` = upload_file(fichier_tmp, type = ".csv")) ,
      encode = "multipart"
    )

  httr::warn_for_status(test)

  data <- test %>%
    httr::content(as = 'text')

  data <- read.csv2(text = data, skip = 1)


  if (length(unique(donnees$CODE_OPERATION)) > length(unique(data$CODE_OPERATION))) {
    warning(
      "Attention, certains résultats d'opération ne figurent pas dans le fichier résultat."
    )
  }

  # mise en forme des résultats
  data$RESULTAT <- data$RESULTAT %>% as.numeric()
  data$CODE_STATION <- paste0("0", data$CODE_STATION)
  data$CODE_PAR <- as.character(data$CODE_PAR)

  return(data)
}
