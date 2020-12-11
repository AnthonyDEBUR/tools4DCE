#' compte_decimales
#'
#' fonction pour calculer le nombre de décimales d'un nombre
#'
#' @param x un nombre ou un vecteur de nombres
#' @return la fonction renvoie un chiffre indiquant le nombre de décimales
#' @examples compte_decimales(3.234)
#' @export
compte_decimales<-function(x){x<-nchar(strsplit(as.character(x), "\\.")[[1]][2])
if(is.na(x)){x<-0}
return(x)}


