#' ajoute_nom_unite
#'
#' fonction pour ajouter le nom de l'unité sandre à une data frame qui contient des codes unités SANDRE
#'
#' @param x data.frame avec le code paramètre
#' @param col_unite : nom de la colonne avec le code unite Valeur par défaut : CdUniteMesure
#' @param type_nom : variable pouvant valoir soit "symbole", soit "nom" selon si on veut le symbole ou le libellé de l'unité du référentiel SANDRE. Valeur par défaut : "symbole"
#'
#'
#' @return la fonction le data.frame complété de la colonne NomParametre
#'
#' @examples test<-data.frame(CdUniteMesure2=c("129", "13", "133"))
#' @examples ajoute_nom_unite(test, col_unite="CdUniteMesure")
#' @export

ajoute_nom_unite <-
  function(x,
           col_unite = "CdUniteMesure",
           type_nom = "symbole") {
    if (!(type_nom %in% c("symbole", "nom"))) {
      stop("le paramètre type_nom ne correspond pas à un code autorisé")
    }
    data(unites_sandre, package = "tools4DCE")
    colonne <-
      ifelse(
        type_nom == "symbole",
        "SymUniteMesure",
        "LblUniteMesure"
      )
    unites_sandre <-
      unites_sandre %>% dplyr::select(CdUniteMesure, ends_with(colonne))

    return(left_join(x, unites_sandre,
                     by = setNames("CdUniteMesure", col_unite))
           )
  }
