#' ajoute_nom_cdreseaumesure
#'
#' fonction pour ajouter le nom du code réseau de mesure sandre à une data frame
#'
#' @param x data.frame avec le code réseau
#' @param col_cdreseau : nom de la colonne avec le code accréditation Valeur par défaut : CdRdd
#'
#'
#' @return la fonction le data.frame complété de la colonne NomRdd
#'
#' @examples test<-data.frame(CdAccreAna2=c("1", "2"))
#' @examples ajoute_nom_cdreseaumesure(test, col_cdaccredi="CdAccreAna2")
#' @export

ajoute_nom_cdreseaumesure <-
  function(x,
           col_cdreseau="CdRdd") {

    data("reseaux_sandre", envir=environment())

    return(left_join(x, reseaux%>%select(CodeSandreRdd, NomRdd), by = setNames("CdRdd", col_cdreseau)))
  }
