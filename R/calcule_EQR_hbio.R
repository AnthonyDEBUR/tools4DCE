#' calcule_EQR_hbio
#'
#' fonction pour calculer l'EQR d'un indice hydrobiologique à partir de la note de l'indice et de la typologie de la staation.
#'
#' @param valeur_indice note à convertir en EQR
#' @param CdParametre code sandre du paramètre à convertir
#' @param typologie : typologie de la station à convertir
#' @param specificite : précisions sur la typologie (ex pour IBD de > 10000 km2 ou pas)
#'
#' @return la fonction renvoie l'AQR correspondant à la note.
#'
#' @examples calcule_EQR_hbio(15.5, CdParametre="5856", typologie="P12-A", specificite="CAS_GENERAL")
#' @export
calcule_EQR_hbio <-
  function(valeur_indice,
           CdParametre,
           typologie,
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
    if (!(typologie %in% base_ref_eqr[base_ref_eqr$CdParametre == CdParametre,]$TYPEFR)) {
      stop("typologie inconnue pour ce paramètre dans la table base_ref_eqr")
    }
    if (!(specificite %in% base_ref_eqr[base_ref_eqr$CdParametre == CdParametre &
                                        base_ref_eqr$TYPEFR == typologie,]$SPECIFICITE)) {
      stop("Spécificité inconnue pour cette typologie de station.")
    }

    base_eqr <- base_ref_eqr[base_ref_eqr$CdParametre == CdParametre &
                               base_ref_eqr$TYPEFR == typologie,]

    EQR = (valeur_indice - base_eqr$NOTE_MINI) / (base_eqr$NOTE_REFERENCE -
                                                    base_eqr$NOTE_MINI)

    # on limite EQR entre 0 et 1
    if(EQR<0){EQR<-0}
    if(EQR>1){EQR<-1}

    return(EQR)
  }
