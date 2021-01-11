#' graphDCE_distribution
#'
#' fonction pour réaliser un graphique bilan de quantifications
#'
#' @param donnees : dataframe contenant les analyses dont il faut faire le bilan
#' @param col_parametre : nom de la colonne par rapport à laquelle faire le bilan (par défaut NomParametre)
#' @param col_Valeur : nom de la colonne avec les résultats d'analyses (par défaut RsAna)
#' @param col_CdRq : nom de la colonne avec le code remarque des analyses (par défaut CdRqAna). Les codes correspondent à ceux définis par le SANDRE.
#' @param seuil : object de classe seuil ou liste d'objets de classe seuil
#' @return la fonction renvoie un chiffre indiquant le nombre de décimales
#' @examples compte_decimales(3.234)
#' @export
graphDCE_distribution<-function(donnees, col_parametre="CdParametre", col_Valeur="RsAna", col_CdRq="CdRqAna", seuil){

  graph<-ggplot()

  return(graph)

}


