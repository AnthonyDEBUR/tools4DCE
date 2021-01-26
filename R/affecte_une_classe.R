#' affecte_une_classe
#'
#' fonction pour affecter une classe de qualité à des résultats
#'
#' @param x un nombre ou un vecteur de nombres
#' @param seuil un objet de classe seuil ou une liste de classe seuil de longueur 1
#'
#' @return la fonction renvoie un vecteur ou un nombre avec les classes de qualité correspondant aux valeurs en entrée
#'
#' @examples seuil<-makeSeuils(CdParametre="1335", type_seuil="DCE")[[1]]
#' @examples affecte_une_classe(0.6, seuil)
#' @examples test<-sample(seq(0,0.6, 0.1),10, replace=T)
#' @examples affecte_une_classe(test, seuil)
#' @export
affecte_une_classe<-function(x, seuil){
  # teste si le format en entrée est correct
  if(length(seuil)>1){warning("Plusieurs seuils différents fournis, seule la première valeur de seuil a été utilisée pour attribuer les classes")
                        seuil<-seuil[[1]]}
  if("list"%in%class(seuil)){seuil<-seuil[[1]]}


  # selon si le seuil est de type bornesinfinclue ou pas
  result<-character(length(x))

  for(i in 1:length(seuil@seuils$SEUILMIN)){
    sens<-ifelse(seuil@seuils$SEUILMIN[i]>seuil@seuils$SEUILMAX[i], F,T) # permet de résoudre les cas comme le pH où on a un paramètre de type borne_inf_inclue pour pHmin et pHmax
    mini<-min(seuil@seuils$SEUILMIN[i], seuil@seuils$SEUILMAX[i])
    maxi<-max(seuil@seuils$SEUILMIN[i], seuil@seuils$SEUILMAX[i])

    if(seuil@bornesinfinclue){ifelse(sens,result<-replace(result,((x>mini) & (x<=maxi)),seuil@seuils$CLASSE[i]%>%as.character),
                                          result<-replace(result,((x>=mini) & (x<maxi)),seuil@seuils$CLASSE[i]%>%as.character))
                                              }
    else{ifelse(sens,
                result<-replace(result,((x>=mini) & (x<maxi)),seuil@seuils$CLASSE[i]%>%as.character),
                result<-replace(result,((x>mini) & (x<=maxi)),seuil@seuils$CLASSE[i]%>%as.character)
    )}}


  # conversion du resultat en factor
  levels_result<-levels(seuil@seuils$CLASSE)
  levels_result<-c(levels_result, if(any(result=="")){""})
  labels_result<-levels(seuil@seuils$CLASSE)
  labels_result<-c(labels_result, if(any(result=="")){"INDEFINI"})

  result<-factor(result, levels=levels_result, labels = labels_result)


return(result)
}


