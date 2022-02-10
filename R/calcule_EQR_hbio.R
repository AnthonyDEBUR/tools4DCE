#' calcule_EQR_hbio
#'
#' fonction pour calculer l'EQR d'un indice hydrobiologique à partir de la note de l'indice et de la typologie de la staation.
#'
#' @param data tableau de données avec les résultats d'analyse
#' @param stations data.frame avec les colonnes suivantes :
#' @param col_parametre nom de la colonne qui identifie les pesticides. Par défaut CdParametre
#' @param col_date nom de la colonne avec la date du prélèvement. Par defaut DatePrel.
#' @param col_valeur nom de la colonne avec les résultats d'analyse. Par défaut RsAna.
#' @param col_station nom de la colonne qui renseigne sur où se trouve les différentes stations. Par défaut CdStationMesureEauxSurface
#' @param resultat_seul booléen. Si il vaut TRUE, la fonction ne renvoie que la colonne somme pesticides. Si il vaut FALSE, la fonction renvoie une colonne par paramètre pris en compte
#'
#' @return la fonction renvoie une dataframe avec les informations sur la station, la date, l'unité et la valeur de la somme des pesticides ainsi qu'une colonne avec chaque pesticide constituant la somme.
#'
#' @examples data<-import_hubeau_indices_hbio(liste_stations="03174000", indice="inv")
#' @examples calcule_EQR_hbio(data, stations)
#' @export
calcule_EQR_hbio <-
  function(valeur_indice,
           CdParametre,
           Typologie,
           specificite = "CAS_GENERAL") {
    data(base_ref_eqr, package = "tools4DCE")
    if (!is.numeric(valeur_indice)) {
      stop("valeur_indice doit être un nombre")
    }
    if (valeur_indice < 0 |
        valeur_indice > 20) {
      stop("valeur_indice doit être compris entre 0 et 20")
    }
    if (!(CdParametre %in% base_ref_eqr$CdParametre)) {
      stop("CdParametre ne correspond pas à un paramète de la table base_ref_eqr.")
    }
    if (!(Typologie %in% base_ref_eqr[base_ref_eqr$CdParametre == CdParametre,]$TYPEFR)) {
      stop("Typologie inconnue pour ce paramètre dans la table base_ref_eqr")
    }
    if (!(specificite %in% base_ref_eqr[base_ref_eqr$CdParametre == CdParametre &
                                        base_ref_eqr$TYPEFR == Typologie,]$SPECIFICITE)) {
      stop("Spécificité inconnue pour cette typologie de station.")
    }

    base_eqr <- base_ref_eqr[base_ref_eqr$CdParametre == CdParametre &
                               base_ref_eqr$TYPEFR == Typologie,]

    EQR = (valeur_indice - base_eqr$NOTE_MINI) / (base_eqr$NOTE_REFERENCE -
                                                    base_eqr$NOTE_MINI)

    return(EQR)
  }
