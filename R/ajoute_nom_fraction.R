#' ajoute_nom_fraction
#'
#' fonction pour ajouter le nom du paramètre sandre à une data frame qui contient des codes paramètres SANDRE
#'
#' @param x data.frame avec le code paramètre
#' @param col_fraction : nom de la colonne avec le code paramètre. Valeur par défaut : CdFractionAnalysee
#'
#'
#' @return la fonction le data.frame complété de la colonne NomParametre
#'
#' @examples test<-data.frame(CdFractionAnalysee2=c("3", "23"))
#' @examples ajoute_nom_fraction(test, col_fraction="CdFractionAnalysee2")
#' @export

ajoute_nom_fraction <-
  function(x,
           col_fraction = "CdFractionAnalysee") {

    data("fractions_sandre", envir=environment())

    return(left_join(x, fractions_sandre, by = setNames("CdFractionAnalysee", col_fraction)))
  }
