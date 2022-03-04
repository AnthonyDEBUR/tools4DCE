#' calcule_SEEE_ODinvertebres
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
#' @param digits nombre de chiffres significatifs
#'
#' @return l'arrondi du nombre selon les règles usuelles en France
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
#' @examples calcule_SEEE_ODinvertebres(donnees)
#'
#' @export

calcule_SEEE_ODinvertebres <- function(donnees)
{
  url_base <-
    paste0(
      "https://seee.eaufrance.fr/api/calcul?indicateur=ODInvertebres&version=1.0.2"
    )


  fichier_tmp <- tempfile(fileext = ".csv")
  write.table(donnees, fichier_tmp, sep = "\t", row.names = F)

  test <-
    POST(
      url_base,
      query = list(
        `sortie[complementaire]` = "1"),
      body = list(
        `multipart/form-data` = upload_file(fichier_tmp, type = ".csv")
      ) ,
      encode = "multipart"
    )

test$url
  httr::warn_for_status(test)

  data <- test %>%
    httr::content(as = 'text')

    data<-strsplit(data, '\n[1] \"Fichier\"\n',fixed=TRUE)

  data_base <- read.csv2(text = data[[1]][1], skip = 1)
  data_comp <- read.csv2(text = data[[1]][2], skip = 1)

  data<-left_join(data_base, data_comp, by="CODE_OPERATION")

  return(data)
}
