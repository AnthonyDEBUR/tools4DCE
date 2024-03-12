#' calcule_formes_azote
#'
#' fonction pour calculer les différentes formes de l'azote
#' a partir des parametres entrants azote kjeldahl, ammonium (en NH4/L),
#' nitrites (en NO2/L) et nitrates (en NO3/L)
#'
#' @param donnees data.frame avec les colonnes CdParametre (code sandre du parametre),
#' RsAna (résultat d'analyses), CdRqAna (code remarque analyse, optionnel)
#' @param CdRqAna character indiquant quelle colonne contient le code remarque SANDRE
#' de l'analyse. Si le paramètre est NULL (par défaut) alors il n'y a pas de code remarque
#' @param CdParametre character indiquant quelle colonne contient le code
#' parametre SANDRE de l'analyse. Par defaut CdParametre
#' @param RsAna character indiquant quelle colonne contient le resultat
#' de l'analyse. Par defaut RsAna
#' @param data_incomplete_stop : booleen : si TRUE alors la fonction s'arrête s'il
#' manque un des paramètres d'entrée. Si FALSE alors le calcule continue
#'
#'
#' @return la fonction renvoie un data.frame (en mg N/L) avec les valeurs
#' d'azote organique, d'azote ammoniacal en N-NH4, de nitrites en N-NO2 et
#' de nitrates en N-NO3.
#' Une valeur avec code remarque différent de 1 est prise comme nulle
#'
#' @examples data<-data.frame(CdParametre=c("1335", "1339", "1340", "1319"),
#'                             RsAna=c(0.5,0.1,30,2),
#'                             CdRqAna=c("1","10","1","1"))
#' @examples calcule_formes_azote(data)
#'
#' @export
#'
calcule_formes_azote <- function(data,
                                 CdRqAna = NULL,
                                 CdParametre = "CdParametre",
                                 RsAna = "RsAna",
                                 data_incomplete_stop = TRUE)
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
  if (!missing(data_incomplete_stop) &&
      !is.logical(data_incomplete_stop)) {
    stop("Le paramètre data_incomplete_stop doit être un booléen.")
  }


  # on remplace valeurs avec CdRqAna différent de 1 par 0
  if (!is.null(CdRqAna)) {
    if (any(data[[CdRqAna]] != "1")) {
      data[data[[CdRqAna]] != "1", RsAna] <- 0
    }
  }

  # Vérifier la présence des paramètres spécifiques
  parametres <- c("1335", "1339", "1340", "1319")
  if (!all(parametres %in% data[[CdParametre]])) {
    if (data_incomplete_stop)
    {
      stop("Certains des paramètres spécifiques ne sont pas présents dans la dataframe.")
    } else
    {
      warning("Certains des paramètres spécifiques ne sont pas présents dans la dataframe.")
    }
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
    CdParametre = c("1335", "1339", "1340", "1319"),
    conversion = c(0.776, 0.304, 0.2259, 1)
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

  #  si NH4 et NKJ sont renseignés, calcule de N orga + test (si NH4>NKJ alors warning)
  if (any(data[[CdParametre]] == "1335") && any(data[[CdParametre]] == "1319")) {
    if (data[data[[CdParametre]] == "1335", RsAna] > data[data[[CdParametre]] == "1319", RsAna]) {
      warning(paste("Ammonium (", data[data[[CdParametre]] == "1335", RsAna], ") supérieur à azote Kjeldahl (", data[data[[CdParametre]] == "1319", RsAna], ")"))
    }
    # ajout ligne NOrga (code SANDRE  5932) si NH4 et NKJ sont renseignés
    new_row <- data.frame(
      CdParametre = "5932",
      RsAna = data[data[[CdParametre]] == "1319", RsAna] - data[data[[CdParametre]] == "1335", RsAna]
    )
    names(new_row) <- setNames(names(new_row), c(CdParametre, RsAna))
    # Récupérer les colonnes de data, à l'exception de CdParametre et RsAna
    cols_to_fill_na <- setdiff(names(data), c(CdParametre, RsAna))

    # Remplir les colonnes avec NA
    for (col in cols_to_fill_na) {
      new_row[[col]] <- NA
    }

    data <- rbind(data, new_row)

    data<-data[data[[CdParametre]] != "1319", ]
  }
  return(data)
}
