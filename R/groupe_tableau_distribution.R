#' groupe_tableau_distribution
#'
#' fonction pour réaliser grouper les tableaux de distribution
#'
#' @param donnees : dataframe contenant les analyses dont il faut faire le bilan
#' @param col_parametre : nom de la colonne par rapport à laquelle faire le bilan (par défaut NomParametre)
#' @param col_Valeur : nom de la colonne avec les résultats d'analyses (par défaut RsAna)
#' @param col_CdRq : nom de la colonne avec le code remarque des analyses (par défaut CdRqAna). Les codes correspondent à ceux définis par le SANDRE.
#' @param col_CdSupport: nom de la colonne avec les codes SANDRE des supports analysés (par défaut : CdSupport), renseigner NULL si pas de colonne correspondant
#' @param col_CdFraction : nom de la colonne avec les codes SANDRE des fractions analysés (par défaut : CdFractionAnalysee), renseigner NULL si pas de colonne correspondant
#' @param seuils : liste d'objets de classe seuil
#'
#' @return la fonction renvoie un chiffre indiquant le nombre de décimales
#'
#' @examples compte_decimales(3.234)
#' @export
groupe_tableau_distribution<-function(donnees, col_Valeur="RsAna", col_CdRq="CdRqAna",
                                col_support="CdSupport", col_CdFraction="CdFractionAnalysee", seuils){

  # temp
  parametres<-c("1301", "1302", "1335", "1339", "1340")
  test<-makeSeuils(CdParametre=c("1340", "1301"), specificites=c("", "CYPRINICOLE"), type_seuil = "DCE")
  seuil1<-makeSeuils(type_seuil="DCE", specificites = "", CdParametre %in% c("1301", "1302", "1335", "1339", "1340"))
  analyses <- readRDS("~/R_Anthony/Naiades/bdd_locale/analyses.rds")
  donnees<-analyses%>%subset(CdParametre%in%c("1301", "1302", "1335", "1339", "1340"))

  # initialisation
    donnees$RsAna<-donnees[[col_Valeur]]
    donnees$CdRsAna<-donnees[[col_CdRq]]
    if(!is.null(col_support)){donnees$CdSupport<-donnees[[col_support]]}






  graph<-ggplot()

  return(graph)

}


