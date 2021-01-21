#' graphDCE_distribution
#'
#' fonction pour réaliser un graphique bilan de quantifications
#'
#' @param donnees : dataframe contenant les analyses dont il faut faire le bilan dans un format du type de celui rendu par la fonction groupe_tableau_distribution
#' @param titre : titre du graphique
#' @param legende_LQ : vecteur de character de longueur 2 qui précise l'étiquette pour les données inférieures et supérieures à la LQ
#' @param affiche_valeurs : booléen qui indique d'il faut afficher les valeurs ou pas dans les barres
#' @param min_affiche : pourcentage minimum à atteindre pour que la valeur soit affiché (ex si min_affiche=0.3, seules le nb de prélèvements représentant au moins 30% pour une categorie et un paramètre sera affiché)
#' @return la fonction renvoie un objet ggplot avec le graphique de distribution
#'
#' @examples donnees<-data.frame(parametres=rep(c("1301", "1340", "1335"), 100), RsAna=sample(0.1:100, 300, replace=TRUE), LqAna=c(0.5,1,6))
#' @examples donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
#' @examples donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
#' @examples seuils<-makeSeuils(CdParametre=donnees$parametres%>%unique, specificites=c("CYPRINICOLE", rep(NA,2)), type_seuil = "DCE")
#' @examples tableau<-groupe_tableau_distribution(donnees, col_CdParametre="parametres", col_CdSupport=NULL, col_CdFraction=NULL, col_CdUnite=NULL, seuils = seuils)
#' @examples graphDCE_distribution(tableau)
#' @export
graphDCE_distribution<-function(donnees, titre="", legende_LQ=c("NON QUANTIFIE", "QUANTIFIE"), affiche_valeurs=T, min_affiche=0.1){

  # creation d'un tableau de correspondance CATEGORIE - LEGENDE
  cat_leg<-donnees%>%select(CLASSE, NOM_COULEUR)%>%distinct%>%arrange(CLASSE)

  # ajout d'une colonne pour savoir si on doit afficher les étiquettes de valeurs
  seuil_param<-donnees%>%group_by(parametre)%>%dplyr::summarise(total=sum(nb))
  donnees<-donnees%>%left_join(seuil_param, by="parametre")


    graph<-ggplot(donnees, aes(x=nb, y=parametre, fill=CLASSE, alpha=ALPHA%>%as.factor))+
      geom_bar(position="fill", stat="identity") + scale_alpha_manual(values=c(0.20, 1), labels=legende_LQ) +
      scale_fill_manual(labels=cat_leg$CLASSE, values=cat_leg$NOM_COULEUR)+
      scale_x_continuous(labels = scales::percent) +
      labs(title = titre, x = "", y = "", alpha = "QUANTIFICATION")

    # ajout des étiquettes si demandé
    if(affiche_valeurs){graph<-graph+geom_text(data=donnees%>%subset(nb>min_affiche*total), aes(x=nb, y=parametre, label=nb),stat='identity',position=position_fill(vjust=0.5))
}


  return(graph)

}


