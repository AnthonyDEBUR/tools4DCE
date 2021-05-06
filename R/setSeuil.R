#' setSeuils
#'
#' création d'un objet de classe seuil
#'
#' @param nom_parametre = un character qui identifie explicitement le paramètre représenté
#' @param nom_seuil = un character qui précise l'origine des seuils
#' @param type_seuil = un character qui désigne type du seuil (DCE ou autre)
#' @param code_parametre = Code SANDRE du paramètre
#' @param synonymes_parametre = Code SANDRE des synonymes du paramètre (séparés par |)
#' @param support = Code SANDRE du support (optionnel)
#' @param fraction = Code SANDRE de la fraction (optionnel)
#' @param code_unite = Code SANDRE de l'unité (optionnel)
#' @param seuils = data.frame avec les colonnes suivantes : CLASSE (character), SEUILMIN (numeric), SEUILMAX (numeric), NOM_COULEUR (un nom ou code hexa de couleur valide)
#' @param bornesinfinclue = booléen. Si vrai la classe de qualité couvrira l'intervalle [SEUILMIN, SEUILMAX[, si faux elle couvrira ]SEUIL_MIN,SEUIL_MAX]
#' @param specificites = typologie particulière à laquelle s'applique le seuil
#' @param base_seuils_color = un object base_seuils transformé par makeSeuils pour inclure les couleurs
#' @param id_ = l'identifiant unique de la combinaison PARAMETRE TYPE  SPECIFICITE dans base_seuils_color
#' @return la fonction renvoie un objet de class seuil
#' @examples setSeuils(nom_parametre="parametre test",nom_seuil="AM 25 janv 2010",type_seuil="DCE", code_parametre="1301",support="3",code_unite="5", seuils=tools4DCE::base_seuils%>%subset(NOM=="TEMPERATURE" & SPECIFICITE=="CYPRINICOLE")%>%left_join(couleurs_classes, by=c("CLASSE", "TYPE")),bornesinfinclue=T, levels_classes=c("TRES BON", "BON", "MOYEN","MEDIOCRE", "MAUVAIS"))
#'
#' @export
#'
setSeuils <-
  function(nom_parametre,
           nom_seuil,
           type_seuil,
           code_parametre,
           synonymes_parametre,
           support = "",
           fraction = "",
           code_unite = "",
           seuils,
           bornesinfinclue = T,
           specificites = "",					 
					 id_,
					 base_seuils_color)
  {
		#browser()
		if (class(base_seuils_color)[3]!="data.frame") stop ("Erreur interne, base_seuils_color devrait être un tibble, avez vous réussi à charger base_seuils ?")
		if (!is.integer(id_)) stop("id_ doit être un entier")
		if (length(id_)!=1) stop("id_ doit être de longueur 1")
		seuil <- base_seuils_color %>% filter(id==id_) %>%  
				select(SEUILMIN, SEUILMAX, CLASSE, NOM_COULEUR) %>%
				data.frame()
		seuil$CLASSE <-
				factor(seuil$CLASSE, levels = ordre_facteurs_qualite[, "CLASSE"]) %>% droplevels()
    new(
      Class = "seuil",
      nom_parametre = nom_parametre,
      nom_seuil = nom_seuil,
      type_seuil = type_seuil,
      code_parametre = code_parametre,
      synonymes_parametre = synonymes_parametre,
      support = support,
      fraction = fraction,
      code_unite = code_unite,
      seuils = seuil,
      bornesinfinclue = bornesinfinclue,
      #     levels_classes=levels_classes,
      specificites = specificites
    )

  }
