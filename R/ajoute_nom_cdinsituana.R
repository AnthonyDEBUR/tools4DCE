#' ajoute_nom_cdinsituana
#'
#' fonction pour ajouter le nom du code mesure insitu sandre à une data frame
#'
#' @param x data.frame avec le code mesure in situ
#' @param col_insitu : nom de la colonne avec le code paramètre. Valeur par défaut : CdInsituAna
#'
#'
#' @return la fonction le data.frame complété de la colonne RqAna
#'
#' @examples test<-data.frame(CdInsituAna=c("1", "10"))
#' @examples ajoute_nom_cdinsituana(test, col_insitu="CdInsituAna")
#' @export

ajoute_nom_cdinsituana <-
  function(x,
           col_insitu="CdInsituAna") {

    data("CdInsituAna", envir=environment())

    return(left_join(x, CdInsituAna, by = setNames("CdInsituAna", col_insitu)))
  }
