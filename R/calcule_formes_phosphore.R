#' calcule_formes_phosphore
#'
#' fonction pour calculer les différentes formes de phosphore
#' a partir des parametres entrants phosphore total et orthophosphates
#'
#' @param donnees data.frame avec les colonnes CdParametre (code sandre du parametre),
#' RsAna (résultat d'analyses), CdRqAna (code remarque analyse, optionnel)
#' @param CdRqAna character indiquant quelle colonne contient le code remarque SANDRE
#' de l'analyse. Si le paramètre est NULL (par défaut) alors il n'y a pas de code remarque
#' @param CdParametre character indiquant quelle colonne contient le code
#' parametre SANDRE de l'analyse. Par defaut CdParametre
#' @param RsAna character indiquant quelle colonne contient le resultat
#' de l'analyse. Par defaut RsAna
#'
#'
#' @return la fonction renvoie un data.frame (en mg P/L) avec les valeurs
#' de phosphore sous forme PO4 et des autres formes de phosphores. La ligne autres formes de phosphore
#' n'exite pas sousle SANDRE. Il est indique Pautre
#' Une valeur avec code remarque différent de 1 est prise comme nulle
#'
#' @examples data<-data.frame(CdParametre=c("1350", "1433"),
#'                             RsAna=c(0.6,0.5),
#'                             CdRqAna=c("1","1"))
#' @examples calcule_formes_phosphore(data)
#'
#' @export
#'
calcule_formes_phosphore <- function(data,
                                 CdRqAna = NULL,
                                 CdParametre = "CdParametre",
                                 RsAna = "RsAna")
{
  if (!(exists(CdParametre, where = environment()) &&
        CdParametre %in% names(data))) {
    stop("La colonne",
         CdParametre,
         " n'existe pas dans le data.frame data.")
  }
  if (!(exists(RsAna, where = environment()) &&
        RsAna %in% names(data))) {
    stop("La colonne", RsAna, " n'existe pas dans le data.frame data.")
  }
  if (!is.null(CdRqAna)) {
    if (!(exists(CdRqAna, where = environment()) &&
          CdRqAna %in% names(data))) {
      stop("La colonne",
           CdRqAna,
           " n'existe pas dans le data.frame data.")
    }
  }

  # on remplace valeurs avec CdRqAna différent de 1 par 0
  if (!is.null(CdRqAna)) {
    if (any(data[[CdRqAna]] != "1")) {
      data[data[[CdRqAna]] != "1", RsAna] <- 0
    }
  }

  # Vérifier la présence des paramètres spécifiques
  parametres <- c("1433", "1350")
  if (!all(parametres %in% data[[CdParametre]])) {
    stop("Certains des paramètres spécifiques ne sont pas présents dans la dataframe.")
     }

  # Vérifier s'il existe plus d'une ligne pour chaque paramètre spécifique
  for (parametre in parametres) {
    if (sum(data[[CdParametre]] == parametre) > 1) {
      stop(paste("Le parametre ",
                 parametre,
                 " comporte plusieurs lignes"))
    }
  }

  # facteurs de conversion
  convert <- data.frame(
    CdParametre = c("1433", "1350"),
    conversion = c(0.3261, 1)
  )

  # Fusionner les données de conversion avec le dataframe
  data <-
    merge(
      data,
      convert,
      by = c(CdParametre = "CdParametre"),
      all.x = TRUE
    )

  # Effectuer la conversion en multipliant RsAna par le facteur de conversion correspondant
  data$RsAna <- data$RsAna * data$conversion

  # Supprimer la colonne de conversion (si vous ne souhaitez pas la conserver)
  data$conversion <- NULL

  #  test (si PO4>Ptot alors warning)
  if (any(data[[CdParametre]] == "1433") && any(data[[CdParametre]] == "1350")) {
    if (data[data[[CdParametre]] == "1433", RsAna] > data[data[[CdParametre]] == "1350", RsAna]) {
      warning(paste("P-PO4 (", data[data[[CdParametre]] == "1433", RsAna], ") supérieur à P tot (", data[data[[CdParametre]] == "1350", RsAna], ")"))
    }
    # ajout ligne Pautre (code Pautre a defaut de code SANDRE)
    new_row <- data.frame(
      CdParametre = "Pautre",
      RsAna = data[data[[CdParametre]] == "1350", RsAna] - data[data[[CdParametre]] == "1433", RsAna]
    )

    names(new_row) <- setNames(names(new_row), c(CdParametre, RsAna))
    # Récupérer les colonnes de data, à l'exception de CdParametre et RsAna
    cols_to_fill_na <- setdiff(names(data), c(CdParametre, RsAna))

    # Remplir les colonnes avec NA
    for (col in cols_to_fill_na) {
      new_row[[col]] <- NA
    }

    data <- rbind(data, new_row)

    data<-data[data[[CdParametre]] != "1350", ]
  }
  return(data)
}
