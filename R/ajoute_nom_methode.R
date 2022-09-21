#' ajoute_nom_methode
#'
#' fonction pour ajouter le nom de la méthode correspondant à un code méthode selon la nomenclature SANDRE
#'
#' @param x data.frame avec le code paramètre
#' @param col_methode : nom de la colonne avec le code méthode Valeur par défaut : CdMethode
#'
#'
#' @return la fonction le data.frame complété de la colonne RqAna
#'
#' @examples test<-data.frame(CdMethode=c("712", "405"))
#' ajoute_nom_methode(test, col_methode="CdMethode")
#' @export

ajoute_nom_methode <-
  function(x,
           col_methode="CdMethode") {

    data("methodes_sandre", envir=environment())

    return(left_join(x, methodes_sandre, by = setNames("CdMethode", col_methode)))
  }
