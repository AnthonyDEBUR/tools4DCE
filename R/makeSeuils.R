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
#' @param nom_seuil vecteur optionnel de character qui liste les noms de suils à afficher ("AM. 25/01/2010", "OSPAR")
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
           nom_seuil = NULL,
           specificites = NULL) {
    #browser()
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
    if (!is.null(nom_seuil)) {
      seuils_demandes <-
        seuils_demandes %>% mutate(NOM_SEUIL = nom_seuil)
    }


    if (nrow(seuils_demandes) == 0) {
      seuils_demandes <- base_seuils
    }


    # jointure uniquement sur les colonnes renseignées
    #if (nrow(seuils_demandes) > 0) {
    seuils_demandes$id <- 1:nrow(seuils_demandes)

    base_seuils <-
      inner_join(
        base_seuils,
        seuils_demandes,
        by = colnames(seuils_demandes)[colnames(seuils_demandes) != "id"],
        na_matches = "na"
      )
    #   }


    # les lignes sont répétées dans le tableau initial ici on ne veut qu'une ligne par élément du tableau
    # seuils_demandes, les colonnes choisies ne sont pas répétées
    base_seuils_paronly <- base_seuils %>%
      select(
        NOM,
        SUPPORT,
        FRACTION,
        UNITE,
        NOM_SEUIL,
        TYPE,
        TYPE_BORNE,
        SPECIFICITE,
        SYNONYMES,
        PARAMETRE,
        id
      ) %>%
      distinct()
    if (nrow(base_seuils_paronly) != nrow(seuils_demandes))
      stop(
        paste0(
          "Plusieurs seuils différents sont possibles pour un même paramètre (ex. 2 températures différentes). Merci de préciser les critères de construction des seuils.\n"
        )
      )

    # je renomme cet object pour le distinguer de base_seuils, il inclut les couleurs
    base_seuils_color <- base_seuils
    # on ajoute une colonne NOM_COULEUR à la base_seuils
    base_seuils_color <-
      base_seuils_color %>% left_join(couleurs_classes, by = c("CLASSE", "TYPE"))


    # on applique deux types d'arguments à la fonction setSeuils
    # les arguments NOM, NOM_SEUIL, TYPE, sont passés un par un
    # au premier passage le premier NOM, le premier seuil....
    # il faut aussi passer le tableau (qui lui reste le même) dans MoreArg
    liste_seuils <- mapply(
      setSeuils,
      nom_parametre = base_seuils_paronly$NOM,
      nom_seuil = base_seuils_paronly$NOM_SEUIL,
      type_seuil = base_seuils_paronly$TYPE,
      code_parametre = base_seuils_paronly$PARAMETRE,
      synonymes_parametre =
        base_seuils_paronly$SYNONYMES,
      support = base_seuils_paronly$SUPPORT,
      fraction = base_seuils_paronly$FRACTION,
      code_unite = base_seuils_paronly$UNITE,
      bornesinfinclue = ifelse(base_seuils_paronly$TYPE_BORNE == "BORNE_INF_INCLUE", T, F),
      specificites = base_seuils_paronly$SPECIFICITE,
      id_ = base_seuils_paronly$id,
      MoreArgs = list(base_seuils_color = base_seuils_color)
    )


    return(liste_seuils)

  }
