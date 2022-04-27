#' ajoute_nom_support
#'
#' fonction pour ajouter le nom du support sandre à une data frame qui contient des codes support SANDRE
#'
#' @param x data.frame avec le code support
#' @param col_support : nom de la colonne avec le code support. Valeur par défaut : CdSupport
#'
#'
#' @return la fonction le data.frame complété de la colonne NomParametre
#'
#' @examples test<-data.frame(CdSupport2=c("3", "6"))
#' @examples ajoute_nom_support(test, col_support="CdSupport2")
#' @export

ajoute_nom_support <-
  function(x,
           col_support = "CdSupport") {
    data(supports_sandre, package = "tools4DCE")

    return(left_join(x, supports_sandre, by = setNames("CdSupport", col_support)))
  }
