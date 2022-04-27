#' ajoute_nom_station
#'
#' fonction pour ajouter le nom de la station sandre à une data frame qui contient des codes stations SANDRE
#'
#' @param x data.frame avec le code station
#' @param col_station : nom de la colonne avec le code station Valeur par défaut : CdStationMesureEauxSurface
#'
#'
#' @return la fonction le data.frame complété de la colonne NomParametre
#'
#' @examples test<-data.frame(CdStationMesureEauxSurface2=c("03174000", "04207500"))
#' @examples ajoute_nom_station(test, col_station="CdStationMesureEauxSurface2")
#' @export

ajoute_nom_station <-
  function(x,
           col_station = "CdStationMesureEauxSurface") {
    data(stations, package = "tools4DCE")

    return(left_join(x, stations, by = setNames("CdStationMesureEauxSurface", col_station)))
  }
