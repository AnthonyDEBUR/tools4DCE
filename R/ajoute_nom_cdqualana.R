#' ajoute_nom_cdqualana
#'
#' fonction pour ajouter le nom du code qualification de l'analyse sandre à une data frame
#'
#' @param x data.frame avec le code paramètre
#' @param col_qualif : nom de la colonne avec le code qualification Valeur par défaut : CdQualAna
#'
#'
#' @return la fonction le data.frame complété de la colonne RqAna
#'
#' @examples test<-data.frame(CdQualAna=c("1", "10"))
#' @examples ajoute_nom_cdqualana(test, col_qualif="CdQualAna")
#' @export

ajoute_nom_cdqualana <-
  function(x,
           col_qualif="CdQualAna") {

    data("CdQualAna", envir=environment())

    return(left_join(x, CdQualAna, by = setNames("CdQualAna", col_qualif)))
  }
