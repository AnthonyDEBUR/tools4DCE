#' ajoute_nom_param
#'
#' fonction pour ajouter le nom du paramètre sandre à une data frame qui contient des codes paramètres SANDRE
#'
#' @param x data.frame avec le code paramètre
#' @param col_parametre : nom de la colonne avec le code paramètre. Valeur par défaut : CdParametre
#' @param type_nom : variable pouvant valoir soit "court", soit "long" selon si on veut le nom complet ou abrégé du référentiel SANDRE. Valeur par défaut : "long"
#'
#'
#' @return la fonction le data.frame complété de la colonne NomParametre
#'
#' @examples test<-data.frame(CdParametre2=c("1335", "1340"))
#' @examples ajoute_nom_param(test, col_parametre="CdParametre2")
#' @export

ajoute_nom_param<-function(x, col_parametre="CdParametre", type_nom="long"){

  data(parametres_sandre, package="tools4DCE")
  colonne<-ifelse(type_nom=="long", "NomParametre", "LbCourtParametre")
  parametres_sandre<-parametres_sandre%>%select(CdParametre, ends_with(colonne))

  return(left_join(x,parametres_sandre, by=setNames("CdParametre", col_parametre)))}

