#' calcule_ymaxi
#'
#' fonction pour afficher les graphiques des paramètres en couleurs (mode point)
#'
#' @param donnees vecteur de données
#'
#' @return la fonction renvoie l'échelle maxi permettant d'afficher un maximum d'information
#'
#' Cette échelle est calculée selon la règle suivante :
#'  - s'il y a moins de 10 données, ymaxi = 1.2*max(donnees, na.rm=T)
#'  - s'il y a plus de 10 données, ymaxi est calculé de manière empirique comme max(4 x interquantile(10%-90%) + quartile 90%)
#'
#' @examples data<-runif(30, 0, 10)%>%round(2)
#' @examples calcule_ymaxi(data)
#'
#' @export
#'
calcule_ymaxi<-function(donnees)
{
  ymaxi<-max(donnees, na.rm=T)*1.2
  if(length(donnees)>10){
    quantile_val<-quantile(donnees,c(0.1,0.9), na.rm=T)
    if(!any(is.na(quantile_val))){ymaxi<-min((4*(quantile_val[2]-quantile_val[1])+quantile_val[2]),ymaxi)
    ymaxi<-tools4DCE::ceiling_dec(ymaxi, max(sapply(donnees,tools4DCE::compte_decimales),na.rm=T))}
  }
  return(ymaxi)
}

