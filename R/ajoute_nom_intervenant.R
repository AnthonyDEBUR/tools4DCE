#' ajoute_nom_intervenant
#'
#' fonction pour ajouter le nom du paramètre sandre à une data frame qui contient des codes paramètres SANDRE
#'
#' @param x data.frame avec le code paramètre
#' @param col_fraction : nom de la colonne avec le code paramètre. Valeur par défaut : CdFractionAnalysee
#' @param type : type de la colonne renvoyée : soit nom complet si type = nom (valeur par défaut), soit mnemonique de l'intervenant si type = mnemo
#' @param nom_output : nom de la colonne de sortie (par défaut le nom de sortie est NomIntervenant si sortie de type nom ou MnIntervenant si type mnemo)
#'
#' @return la fonction le data.frame complété de la colonne NomParametre
#'
#' @examples x<-data.frame(CdProducteur=c("25440124300012"))
#' @examples ajoute_nom_intervenant(test, col_intervenant="CdProducteur")
#' @export

ajoute_nom_intervenant <-
  function(x,
           col_intervenant = "CdProducteur",
           type = "nom",
           nom_output="") {
    if (class(type) != "character")
    {
      stop("type doit être de classe character avec comme valeur nom ou mnemo")
    }
    if (!(type %in% c("nom", "mnemo")))
    {
      stop("type doit avoir comme valeur nom ou mnemo")
    }
    if (class(nom_output) != "character")
    {
      stop("type doit être de classe character")
    }

    data(intervenants_sandre, package = "tools4DCE")

    if(type=="nom"){intervenants_sandre<-intervenants_sandre%>%select(CdIntervenant, NomIntervenant)}
    if(type=="mnemo"){intervenants_sandre<-intervenants_sandre%>%select(CdIntervenant, MnIntervenant)}
    if(nom_output!="") {names(intervenants_sandre)[2]<-nom_output}

    return(left_join(x, intervenants_sandre, by = setNames("CdIntervenant", col_intervenant)))
  }
