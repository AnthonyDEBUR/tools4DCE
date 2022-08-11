#' ajoute_nom_cdstatutana
#'
#' fonction pour ajouter le nom du code statut de l'analyse sandre à une data frame
#'
#' @param x data.frame avec le code statut
#' @param col_statut : nom de la colonne avec le code statut Valeur par défaut : CdStatutAna
#'
#'
#' @return la fonction le data.frame complété de la colonne CdStatutAna
#'
#' @examples test<-data.frame(CdStatutAna=c("1", "10"))
#' @examples ajoute_nom_cdstatutana(test, col_rq="CdStatutAna")
#' @export

ajoute_nom_cdstatutana <-
  function(x,
           col_statut="CdStatutAna") {

    data("CdStatutAna", envir=environment())

    return(left_join(x, CdStatutAna, by = setNames("CdStatutAna", col_statut)))
  }
