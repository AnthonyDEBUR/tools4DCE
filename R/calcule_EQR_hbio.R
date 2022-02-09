#' calcule_EQR_hbio
#'
#' fonction pour calculer l'EQR d'un indice hydrobiologique à partir de la note de l'indice et de la typologie de la staation.
#'
#' @param data tableau de données avec les résultats d'analyse
#' @param col_parametre nom de la colonne qui identifie les pesticides. Par défaut CdParametre
#' @param col_date nom de la colonne avec la date du prélèvement. Par defaut DatePrel.
#' @param col_valeur nom de la colonne avec les résultats d'analyse. Par défaut RsAna.
#' @param col_station nom de la colonne qui renseigne sur où se trouve les différentes stations. Par défaut CdStationMesureEauxSurface
#' @param resultat_seul booléen. Si il vaut TRUE, la fonction ne renvoie que la colonne somme pesticides. Si il vaut FALSE, la fonction renvoie une colonne par paramètre pris en compte
#'
#' @return la fonction renvoie une dataframe avec les informations sur la station, la date, l'unité et la valeur de la somme des pesticides ainsi qu'une colonne avec chaque pesticide constituant la somme.
#'
#' @examples data<-import_hubeau_indices_hbio(liste_stations="03174000", indice="inv")
#' @examples calcule_EQR_hbio(data)
#' @export
calcule_EQR_hbio <-
  function(data,
           col_parametre = "CdParametre",
           col_date = "DatePrel",
           col_valeur = "RsAna",
           col_station = "CdStationMesureEauxSurface",
           resultat_seul = T) {



    return(data)
  }
