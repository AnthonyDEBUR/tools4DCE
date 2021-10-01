#' tri_molecules
#'
#'
#' fonction pour trier les x molécules les plus préoccupantes parmi un ensemble de résultats
#' recoit en entree un tableau de distribution genere par la fonction groupe_tableau_distribution
#'
#' @param donnees un data.frame avec les colonnes obtenues en sortie de la fonction groupe_tableau_distribution
#' @param tri_croissant booleen vrai par défaut. Si faux, l'ordre du tri est inversé.
#'
#' @return la fonction renvoie un vecteur contenant les paramètres les plus impactés. Le tri se fait sur la base suivant : en 1er est renvoyé le parametre qui a été quantifié le plus souvent au niveau le plus dégradé (en pourcentage de quantification sur nb total_tri de mesures)
#'
#' @examples donnees<-data.frame(parametres=rep(c("1301", "1340", "1335"), 100), RsAna=sample(0.1:100, 300, replace=TRUE), LqAna=c(0.5,1,6))
#' @examples donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
#' @examples donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
#' @examples seuils<-makeSeuils(CdParametre=donnees$parametres%>%unique, specificites=c("CYPRINICOLE", rep(NA,2)), type_seuil = "DCE")
#' @examples donnees<-groupe_tableau_distribution(donnees, col_CdParametre="parametres", col_CdSupport=NULL, col_CdFraction=NULL, col_CdUnite=NULL, seuils = seuils)
#' @examples tri_molecules(donnees)
#' @export
tri_molecules <- function(donnees,
                          tri_croissant = T) {
  # creation d'un tableau de correspondance CATEGORIE - LEGENDE
  cat_leg <-
    donnees %>% select(CLASSE, NOM_COULEUR) %>% distinct %>% arrange(CLASSE)

  # nb total de donnees
  seuil_param <-
    donnees %>% group_by(parametre) %>% dplyr::summarise(total_tri = sum(nb))
  donnees000 <- donnees %>% left_join(seuil_param, by = "parametre")

  # pourcentage
  donnees000$pc <- donnees000$nb / donnees000$total_tri


  if (tri_croissant) {
    levels_tri <-
      levels(fct_rev(cat_leg$CLASSE))
  } else {
    levels_tri <- levels(cat_leg$CLASSE)
  }

  # on ajoute une colonne avec l'ordre de tri (de la + quantifiee à un niveau preoccupant au paramètre le moins quantifie à un niveau preoccupant)
  donnees000$ordre_tri <-
    donnees000$CLASSE %>% as.character %>% factor(levels = levels_tri)
  donnees000$CdRqAna <-
    donnees000$CdRqAna %>% as.character %>% factor(levels = c("3", "1", "10", "2", "7"))
  donnees000 <- donnees000 %>% arrange(CdRqAna, ordre_tri,-pc)

  donnees000$parametre <-
    donnees000$parametre %>% as.character %>% factor(levels = unique(donnees000$parametre))

  return(levels(donnees000$parametre))
}
