#' makeSeuils
#'
#'Créé automatiquement une liste de seuils à partir des données stockées dans les objets base_seuils, couleurs_classes et ordre_facteurs_qualite
#'
#'
#' @return la fonction renvoie une liste d'objets de class seuil
#' @examples makeSeuils()
#' @export

makeSeuils<-function(){
  # chargement des données du package
  base_seuils<-tools4DCE::base_seuils
  couleurs_classes<-tools4DCE::couleurs_classes
  ordre_facteurs_qualite<-tools4DCE::ordre_facteurs_qualite

  # on ajoute une colonne NOM_COULEUR à la base_seuils
  base_seuils<-base_seuils%>%left_join(couleurs_classes, by=c("CLASSE", "TYPE"))

  # on groupe la liste des différents seuils à traiter
  base_seuils<-base_seuils%>%group_by(SUPPORT, FRACTION, PARAMETRE, UNITE, SPECIFICITE, TYPE)

  # on créé un seuil par groupe
  liste_seuils<-base_seuils%>%group_map(~setSeuils(nom_parametre=first(.x$NOM),
                                                   nom_seuil=paste(.x$NOM_SEUIL%>%unique, collapse=" + "),
                                                   type_seuil=first(.x$TYPE),
                                                   code_parametre=first(.x$PARAMETRE),
                                                   support=first(.x$SUPPORT),
                                                   fraction=first(.x$FRACTION),
                                                   code_unite=first(.x$UNITE),
                                                   seuils=.x%>%select(SEUILMIN, SEUILMAX, CLASSE, NOM_COULEUR),
                                                   bornesinfinclue=ifelse(first(.x$TYPE_BORNE)=="BORNE_INF_INCLUE", T,F),
                                                   levels_classes=ordre_facteurs_qualite$CLASSE[ordre_facteurs_qualite$CLASSE %in% .x$CLASSE],
                                                   # levels_classes=ordre_facteurs_qualite$CLASSE,
                                                   specificites = first(.x$SPECIFICITE)
                                                   ), .keep=T
                                          )


  }


