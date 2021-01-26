#' filtre_donnees_extremes
#'
#' filtre les donnees extremes qui n'apparaitront pas parmi les points des graphs
#' pour cela on ecrete les valeurs entrant en remplacant les valeurs inférieures à ymin ou supérieures à ymax par ymin ou ymax
#'
#' @param x un vecteur numerique
#' @param ymin un nombre
#' @param ymax un nombre
#' @return la fonction renvoie un vescteur avec les données extrèmes remplacées par ymin ou ymax.
#'
#' @examples filtre_donnees_extremes(seq(1:10), ymin=3, ymax=9)
#' @export
filtre_donnees_extremes <- function(x, ymin, ymax) {
  if (!is.na(ymin)) {
    x[x < ymin] <- ymin
  }
  if (!is.na(ymin)) {
    x[x > ymax] <- ymax
  }
  if (is.na(ymin) &
      is.na(ymax)) {
    return("Au moins une valeur entre ymin et ymax doit etre renseignee")
  } else{
    return(x)
  }

}
