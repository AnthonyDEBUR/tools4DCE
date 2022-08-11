#' ajoute_nom_cddifficulteana
#'
#' fonction pour ajouter le nom du code difficulté sandre à une data frame
#'
#' @param x data.frame avec le code paramètre
#' @param col_difficulte : nom de la colonne avec le code paramètre. Valeur par défaut : CdDifficulteAna
#'
#'
#' @return la fonction le data.frame complété de la colonne RqAna
#'
#' @examples test<-data.frame(CdDifficulteAna2=c("1", "10"))
#' @examples ajoute_nom_cdrq(test, col_difficulte="CdDifficulteAna2")
#' @export

ajoute_nom_cddifficulteana <-
  function(x,
           col_difficulte="CdDifficulteAna") {

    data("CdDifficulteAna", envir=environment())

    return(left_join(x, CdDifficulteAna, by = setNames("CdDifficulteAna", col_difficulte)))
  }
