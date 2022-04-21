#' PercentileDCE
#'
#' fonction pour calculer le percentile DCE selon les règles françaises d'évaluation de l'état écologique.
#' Ce percentile se calcule selon la loi de Hazen (il s’agit de la valeur du rang égal à l’arrondi du produit de 0,9 par le nombre de valeur auquel on ajoute 0,5.)
#'
#' https://www.artois-picardie.eaufrance.fr/qualite-et-quantite-des-eaux/reseaux-de-surveillance/article/5eme-campagne-de-surveillance
#'
#' @param A = vecteur de nombre dans lequel il faut calculer le percentile
#' @param type = type de percentile (0.9 pour percentile 90, 0.1 pour percentile 10)
#' @param na.rm = paramètre pour ne pas prendre en compte les valeurs non numériques (TRUE) ou renvoyer une erreur (F)
#'
#' @return le percentile calculé selon les règles DCE françaises
#'
#' @examples test<-sample(x=1:100, size=20, replace=T)
#' @examples PercentileDCE(test, type=0.9)
#' @export

PercentileDCE <- function(A, type = 0.9, na.rm = T)
{
  if (!is.numeric(type)) {
    stop("type doit être un nombre entre 0 et 1")
  }
  if (type < 0 |
      type > 1) {
    stop("type doit être un nombre entre 0 et 1")
  }
  if (na.rm == F &
      any(is.na(A))) {
    stop(
      "Une valeur dans le vecteur d'entrée n'est pas un nombre. Utiliser le paramètre na.rm=TRUE pour ne pas prendre en compte les valeurs manquantes."
    )
  }
  A <- A[!is.na(A)]
  Nb <- length(A)
  RangP90 <- arrondi((Nb * type) + 0.5)
  B <- sort(A)
  return(B[RangP90])
}
