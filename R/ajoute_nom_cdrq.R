#' ajoute_nom_cdrq
#'
#' fonction pour ajouter le nom du code remarque sandre à une data frame qui contient des codes remarques SANDRE
#'
#' @param x data.frame avec le code paramètre
#' @param col_rq : nom de la colonne avec le code paramètre. Valeur par défaut : CdRqAna
#'
#'
#' @return la fonction le data.frame complété de la colonne RqAna
#'
#' @examples test<-data.frame(CdRqAna2=c("1", "10"))
#' @examples ajoute_nom_cdrq(test, col_rq="CdRqAna2")
#' @export

ajoute_nom_cdrq <-
  function(x,
           col_rq="CdRqAna") {

    data("CdRqAna", envir=environment())

    return(left_join(x, fractions_sandre, by = setNames("CdRqAna", col_rq)))
  }
