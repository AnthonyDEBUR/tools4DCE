#' class Seuil
#'
#'Définition des objets de classe seuil
#'
#'Ces objets précisent les limites de chaque classe de qualité
#'
#'accessors :
#'
#' nom_parametre = Nom du paramètre à représenter graphiquement
#' nom_seuil= Origine du seuil (arrêté ministériel du 25 janvier 2010, ...)
#' type_seuil = type du seuil (DCE ou autre)
#' code_parametre = Code SANDRE du paramètre
#' synonymes_parametre = Codes SANDRE du paramètre + de ses synonymes. Les codes sont séparés par |
#' support = Code SANDRE du support (optionnel)
#' fraction = Code SANDRE de la fraction (optionnel)
#' code_unite = Code SANDRE de l'unité (optionnel)
#' seuils = data.frame avec les colonnes suivantes : CLASSE (character), SEUILMIN (numeric), SEUILMAX (numeric), NOM_COULEUR (un nom ou code hexa de couleur valide)
#' bornesinfinclue = booléen. Si vrai la classe de qualité couvrira l'intervalle [SEUILMIN, SEUILMAX[, si faux elle couvrira ]SEUIL_MIN,SEUIL_MAX]
#' specificites = typologie particulière à laquelle s'applique le seuil
#'
#'

setClass(
  Class = "seuil",
  representation = representation(
    nom_parametre = "character",
    nom_seuil = "character",
    type_seuil = "character",
    code_parametre = "character",
    synonymes_parametre = "character",
    support = "character",
    fraction = "character",
    code_unite = "character",
    seuils = "data.frame",
    bornesinfinclue = "logical",
    #levels_classes="vector",
    specificites = "character"
  ),
  validity = function(object) {
    if (!all(c("CLASSE", "SEUILMIN", "SEUILMAX", "NOM_COULEUR") %in% names(object@seuils))) {
      stop(
        'Le dataframe seuils doit comporter les noms de colonnes "CLASSE", "SEUILMIN", "SEUILMAX", "NOM_COULEUR"'
      )
    } else{
    }
    if (!is.factor(object@seuils$CLASSE)) {
      stop(
        "les classes de qualité dans la colonne CLASSE de la dataframe seuils doivent être de type factor"
      )
    } else{
    }
    if (!is.numeric(object@seuils$SEUILMIN)) {
      stop(
        "les seuils dans la colonne SEUILMIN de la dataframe seuils doivent être de type numeric"
      )
    } else{
    }
    if (!is.numeric(object@seuils$SEUILMAX)) {
      stop(
        "les seuils dans la colonne SEUILMAX de la dataframe seuils doivent être de type numeric"
      )
    } else{
    }
    if (!all(object@seuils$NOM_COULEUR %in% colors())) {
      stop(
        "les couleurs dans la colonne NOM_COULEUR de la dataframe seuils doivent correspondre à des noms de couleur valides"
      )
    } else{
    }
    if (!is.logical(object@bornesinfinclue)) {
      stop("bornesinfinclue doit être de type logical")
    } else{
    }
    #    if(!all(object@seuils$CLASSE %in% object@levels_classes)){stop(paste0("Les classes définies dans le vecteur levels_classes ne correspondent pas à celles définis dans la data frame seuils"))}else{}

  }

)

