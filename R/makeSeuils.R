#' makeSeuils
#'
#'Créé automatiquement une liste de seuils à partir des données stockées dans les objets base_seuils, couleurs_classes et ordre_facteurs_qualite
#'
#'
#' @return la fonction renvoie une liste d'objets de class seuil
#'
#' @param CdParametre vecteur optionnel de character qui liste les codes paramètres à sélectionner
#' @param CdSupport vecteur optionnel de character qui liste les codes support à sélectionner
#' @param CdFraction vecteur optionnel de character qui liste les codes fractions à sélectionner
#' @param CdUnite vecteur optionnel de character qui liste les codes unité à sélectionner
#' @param type_seuil vecteur optionnel de character qui liste les types de seuils à afficher ("DCE", "NON_DCE")
#' @param specificites vecteur optionnel de character qui liste les spécificités ("SALMONICOLE", "CYPRINICOLE")
#'
#'
#' @examples # créé l'ensemble des seuils
#' makeSeuils()
#'
#' @examples # liste l'ensemble des parametres disponibles
#' lapply(makeSeuils(), function(x) `@`( x , nom_parametre)[[1]])
#'
#'
#' @examples # génère les seuils pour les paramètres 1340, 1301 avec la spécificité non renseignée ou CYPRINICOLE et un type de seuil DCE :
#' test<-makeSeuils(CdParametre=c("1340", "1301"), specificites=c(NA, "CYPRINICOLE"), type_seuil = "DCE")
#' print(test)
#'
#' @export

makeSeuils <-
  function(CdParametre = NULL,
           CdSupport = NULL,
           CdFraction = NULL,
           CdUnite = NULL,
           type_seuil = NULL,
           specificites = NULL) {
    # chargement des données du package
    base_seuils <- tools4DCE::base_seuils
    couleurs_classes <- tools4DCE::couleurs_classes
    ordre_facteurs_qualite <- tools4DCE::ordre_facteurs_qualite

    # on duplique les lignes de base_seuils pour lesquelles il existe des synonymes
    base_seuils$SYNONYMES <- base_seuils$PARAMETRE
    base_seuils$PARAMETRE <-
      gsub(" ", "", base_seuils$PARAMETRE) # suppression des éventuels espaces
    base_seuils <-
      base_seuils %>% separate(
        PARAMETRE,
        sep = "\\|",
        into = paste("PARAMETRE", seq(1:20), sep = ""),
        fill = "right"
      )
    base_seuils <-
      base_seuils %>% pivot_longer(
        cols = PARAMETRE1:PARAMETRE20,
        values_to = "PARAMETRE",
        values_drop_na = T
      )

    # On selectionne uniquement les données qui correspondent à la liste donnée en paramètre de la fonction.
    # si ces paramètres ne sont pas renseigné, alors on créé la liste pour tous les paramètres

    seuils_demandes <-
      tibble::tibble(.rows = max(
        length(CdParametre),
        length(CdSupport),
        length(CdFraction),
        length(CdUnite),
        length(type_seuil),
        length(specificites)
      ))
    if (!is.null(CdParametre)) {
      seuils_demandes <-
        seuils_demandes %>% mutate(PARAMETRE = CdParametre)
    }
    if (!is.null(CdSupport)) {
      seuils_demandes <- seuils_demandes %>% mutate(SUPPORT = CdSupport)
    }
    if (!is.null(type_seuil)) {
      seuils_demandes <- seuils_demandes %>% mutate(TYPE = type_seuil)
    }
    if (!is.null(specificites)) {
      seuils_demandes <-
        seuils_demandes %>% mutate(SPECIFICITE = specificites)
    }
    if (!is.null(CdFraction)) {
      seuils_demandes <- seuils_demandes %>% mutate(FRACTION = CdFraction)
    }
    if (!is.null(CdUnite)) {
      seuils_demandes <- seuils_demandes %>% mutate(UNITE = CdUnite)
    }

    # jointure uniquement sur les colonnes renseignées
    if (nrow(seuils_demandes) > 0) {
      base_seuils <-
        inner_join(
          base_seuils,
          seuils_demandes,
          by = names(seuils_demandes),
          na_matches = "na"
        )
    }

    # on ajoute une colonne NOM_COULEUR à la base_seuils
    base_seuils <-
      base_seuils %>% left_join(couleurs_classes, by = c("CLASSE", "TYPE"))

    # ajout des levels pour les classes de la base seuil
    base_seuils$CLASSE <-
      factor(base_seuils$CLASSE, levels = ordre_facteurs_qualite[, "CLASSE"])

    # on groupe la liste des différents seuils à traiter
    base_seuils <-
      base_seuils %>% group_by(SUPPORT, FRACTION, PARAMETRE, UNITE, SPECIFICITE, TYPE)

    # on créé un seuil par groupe
    liste_seuils <-
      base_seuils %>% group_map(
        ~ setSeuils(
          nom_parametre = first(.x$NOM),
          nom_seuil = paste(.x$NOM_SEUIL %>%
                              unique, collapse = " + "),
          type_seuil = first(.x$TYPE),
          code_parametre = first(.x$PARAMETRE),
          synonymes_parametre =
            first(.x$SYNONYMES),
          support = first(.x$SUPPORT),
          fraction = first(.x$FRACTION),
          code_unite = first(.x$UNITE),
          seuils = .x %>% select(SEUILMIN, SEUILMAX, CLASSE, NOM_COULEUR) %>%
            droplevels(),
          bornesinfinclue = ifelse(first(.x$TYPE_BORNE) ==
                                     "BORNE_INF_INCLUE", T, F),
          # levels_classes=ordre_facteurs_qualite$CLASSE[ordre_facteurs_qualite$CLASSE %in% .x$CLASSE],
          # levels_classes=ordre_facteurs_qualite$CLASSE,
          specificites = first(.x$SPECIFICITE)
        ),
        .keep = T
      )


  }
