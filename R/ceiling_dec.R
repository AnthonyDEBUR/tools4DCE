#' ceiling_dec
#'
#' fonction pour arrondir à la valeur supérieure en précisant le nb de décimales de l'arrondi
#'
#' @param x un nombre
#' @param nb_dec nombre de décimales
#'
#'
#' @return la fonction renvoie un vescteur avec les données extrèmes remplacées par ymin ou ymax.
#'
#' @examples ceiling_dec(3.2000,2)
#' @export

ceiling_dec<-function(x, nb_dec){return(ceiling(x*10^nb_dec)/10^nb_dec)}

