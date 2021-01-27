#' filtre_donnees_extremes
#'
#' filtre les donnees extremes qui n'apparaitront pas parmi les points des graphs
#' pour cela on ecrete les valeurs entrant en remplacant les valeurs inférieures à xmin ou supérieures à xmax par xmin ou xmax
#'
#' @param x un vecteur numerique
#' @param xmin un nombre
#' @param xmax un nombre
#' @return la fonction renvoie un vescteur avec les données extrèmes remplacées par ymin ou ymax.
#'
#' @examples filtre_donnees_extremes(seq(1:10), xmin=3, xmax=9)
#' @export
filtre_donnees_extremes <- function(x, xmin = NA, xmax =NA) {
  if (!is.na(xmin)) {
    x[x < xmin] <- xmin
  }
  if (!is.na(xmax)) {
    x[x > xmax] <- xmax
  }
  if (is.na(xmin) &
      is.na(xmax)) {
    stop("Au moins une valeur entre ymin et ymax doit etre renseignee")
  } 
    return(x)
}
