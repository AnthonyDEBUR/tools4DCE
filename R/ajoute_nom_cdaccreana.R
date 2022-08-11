#' ajoute_nom_cdaccreana
#'
#' fonction pour ajouter le nom du code accréditation sandre à une data frame qui contient des codes accréditation SANDRE
#'
#' @param x data.frame avec le code accréditation
#' @param col_cdaccredi : nom de la colonne avec le code accréditation Valeur par défaut : CdAccreAna
#'
#'
#' @return la fonction le data.frame complété de la colonne RqAna
#'
#' @examples test<-data.frame(CdAccreAna2=c("1", "2"))
#' @examples ajoute_nom_cdrq(test, col_cdaccredi="CdAccreAna2")
#' @export

ajoute_nom_cdaccreana <-
  function(x,
           col_cdaccredi="CdAccreAna") {

    data("CdAccreAna", envir=environment())

    return(left_join(x, CdAccreAna, by = setNames("CdAccreAna", col_cdaccredi)))
  }
