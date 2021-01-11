#' table_distribution
#'
#' fonction pour faire le bilan de la distribution des données par rapport à un seuil
#' Ce tableau fait le bilan du nombre de données disponibles par classe de qualité
#' en distinguant les valeurs quantifiées de celles <LQ
#'
#' @param donnees : dataframe contenant les analyses dont il faut faire le bilan
#' @param col_parametre : nom de la colonne par rapport à laquelle faire le bilan (par défaut NomParametre)
#' @param col_Valeur : nom de la colonne avec les résultats d'analyses (par défaut RsAna)
#' @param col_CdRq : nom de la colonne avec le code remarque des analyses (par défaut CdRqAna). Les codes correspondent à ceux définis par le SANDRE.
#' @param seuil : object de classe seuil ou liste d'objets de classe seuil
#'
#' @return la fonction renvoie une data.frame avec le nombre d'analyses dans chaque classe et le nombre de données <LQ dans chaque classe
#'
#' @examples donnees<-data.frame(RsAna=sample(0.1:60, 500, replace=TRUE), LqAna=c(0.5,1,2,12))
#' @examples donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
#' @examples donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
#' @examples seuil<-makeSeuils(CdParametre="1340", type_seuil="DCE")
#' @examples table_distribution(donnees, seuil)
#' @export
table_distribution<-function(donnees, col_Valeur="RsAna", col_CdRq="CdRqAna", seuil){
  # test si le format en entrée est correct
  if(length(seuil)>1){warning("Plusieurs seuils différents fournis, seule la première valeur de seuil a été utilisée pour attribuer les classes")
    seuil<-seuil[[1]]}
  if("list"%in%class(seuil)){seuil<-seuil[[1]]}

  # on affecte les noms de colonnes paramétrées à des noms fixes
  donnees$RsAna<-donnees[[col_Valeur]]
  donnees$CdRqAna<-donnees[[col_CdRq]]

  # ajout des classes au tableau de valeur
  donnees<-donnees%>%mutate(CLASSE=affecte_une_classe(x=donnees$RsAna,seuil=seuil))

  # tableau de synthèse par classe et LQ
  donnees<-donnees%>%group_by(CLASSE, CdRqAna)%>%dplyr::summarise(nb = n())

  # légende
  donnees<-donnees%>%mutate(CATEGORIE=if_else(donnees$CdRqAna=="1",donnees$CLASSE%>%as.character,paste0("<LQ et LQ de classe ", donnees$CLASSE)))

  # couleurs
  donnees<-left_join(donnees, seuil@seuils%>%select(CLASSE, NOM_COULEUR), by=c("CLASSE"))
  donnees$NOM_COULEUR<-replace_na(donnees$NOM_COULEUR, "white")
  donnees$ALPHA<-ifelse(donnees$CdRqAna==1, 1,0.2)

  return(donnees)
}

