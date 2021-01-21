#' groupe_tableau_distribution
#'
#' fonction pour réaliser grouper les tableaux de distribution par paramètre et par un éventuel autre critère optionnel
#'
#' @param donnees : dataframe contenant les analyses dont il faut faire le bilan
#' @param col_parametre : nom de la colonne par rapport à laquelle faire le bilan (par défaut NomParametre)
#' @param col_Valeur : nom de la colonne avec les résultats d'analyses (par défaut RsAna)
#' @param col_CdParametre : nom de la colonne avec le code parametre des analyses (par défaut CdParametre). Les codes correspondent à ceux définis par le SANDRE.
#' @param col_CdRq : nom de la colonne avec le code remarque des analyses (par défaut CdRqAna). Les codes correspondent à ceux définis par le SANDRE.
#' @param col_CdSupport: nom de la colonne avec les codes SANDRE des supports analysés (par défaut : CdSupport), renseigner NULL si pas de colonne correspondant
#' @param col_CdFraction : nom de la colonne avec les codes SANDRE des fractions analysés (par défaut : CdFractionAnalysee), renseigner NULL si pas de colonne correspondant
#' @param col_CdUnite : nom de la colonne avec les codes SANDRE des unités (par défaut : CdUniteMesure), renseigner NULL si pas de colonne correspondant
#' @param col_tri : nom optionnel de la colonne complémentaire sur laquelle on veut faire le bilan (en plus des paramètres). Exemple : l'année des données
#' @param seuils : liste d'objets de classe seuil
#'
#' @return la fonction renvoie un chiffre indiquant le nombre de décimales
#'
#'
#' @examples donnees<-data.frame(parametres=rep(c("1301", "1340", "1335"), 100), RsAna=sample(0.1:100, 300, replace=TRUE), LqAna=c(0.5,1,6))
#' @examples donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
#' @examples donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
#' @examples seuils<-makeSeuils(CdParametre=donnees$parametres%>%unique, specificites=c("CYPRINICOLE", rep(NA,2)), type_seuil = "DCE")
#' @examples groupe_tableau_distribution(donnees, col_CdParametre="parametres", col_CdSupport=NULL, col_CdFraction=NULL, col_CdUnite=NULL, seuils = seuils)
#'
#' @export
groupe_tableau_distribution<-function(donnees, col_Valeur="RsAna", col_CdRq="CdRqAna",
                                      col_CdParametre="CdParametre",
                                col_CdSupport="CdSupport", col_CdFraction="CdFractionAnalysee",
                                col_CdUnite="CdUniteMesure", col_tri=NULL, seuils){


  # initialisation
    donnees$RsAna<-donnees[[col_Valeur]]
    donnees$CdRsAna<-donnees[[col_CdRq]]

# vecteur des paramètres par rapport auxquels grouper les analyses
    groupes<-c(if(!is.null(col_CdParametre)){col_CdParametre},
               if(!is.null(col_CdSupport)){col_CdSupport},
               if(!is.null(col_CdFraction)){col_CdFraction},
               if(!is.null(col_CdUnite)){col_CdUnite},
               if(!is.null(col_tri)){col_tri}
               )

# on récupère les informations utiles dans chaque seuil pour pouvoir affecter le bon seuil au bon paramètre
    detail_seuil<-tibble(indice_seuil123456=seq(1, length(seuils)))
    if(!is.null(col_CdParametre)){detail_seuil<-detail_seuil%>%mutate(param_seuil=factor(sapply(seuils, function(x){attr(x, "code_parametre")})))}
    if(!is.null(col_CdSupport)){detail_seuil<-detail_seuil%>%mutate(support_seuil=factor(sapply(seuils, function(x){attr(x, "support")})))}
    if(!is.null(col_CdFraction)){detail_seuil<-detail_seuil%>%mutate(fraction_seuil=factor(sapply(seuils, function(x){attr(x, "fraction")})))}
    if(!is.null(col_CdUnite)){detail_seuil<-detail_seuil%>%mutate(cd_unite_seuil=factor(sapply(seuils, function(x){attr(x, "code_unite")})))}
    names(detail_seuil)<-c("indice_seuil123456",groupes)

# on supprime les colonnes remplies de NA
    detail_seuil<-detail_seuil%>%select(where(~!all(is.na(.x))))
    groupes<-groupes[groupes%in%names(detail_seuil)]

# lorsque une des colonnes de sélection des paramètres contient un NA, on remplace ce NA par autant de ligne qu'il existe de valeur distincte de ce paramèter (astuce pour ne pas filtrer sur les NA)
    # pour chaque colonne de detail_seuil
    if(ncol(detail_seuil)>1){
                for(i in 2:ncol(detail_seuil)){

                        if(any(is.na(detail_seuil[,i]))){jointure<-donnees%>%select(names(detail_seuil)[i])%>%distinct%>%mutate(col_jointure=NA)
                        yname<-(names(detail_seuil)[i])
                        xname<-"col_jointure"
                        detail_seuil<-detail_seuil%>%left_join(jointure, by=setNames(xname, yname), na_matches="na")
                        detail_seuil<-detail_seuil%>%mutate(test = select(., c(all_of(yname), paste0(yname,".y"))) %>% do.call(coalesce, .))
                        detail_seuil<-detail_seuil%>% select(-all_of(yname), -all_of(paste0(yname,".y")))%>%rename(setNames(yname, "test"))
                        }
                }


}

# enregistrement du nb de lignes de données avant sélection
    nb_avant<-nrow(donnees)

# on affecte à chaque ligne d'analyses le numéro du seuil lui correspondant (colonne indice_seuil123456)
    donnees<-inner_join(donnees, detail_seuil, by=groupes)

# si on a perdu des lignes dans la sélection, on envoi un warning
    if(nrow(donnees)<nb_avant){warning(paste0("Attention : ", nb_avant-nrow(donnees), " lignes de données ne vérifient pas les critères d'entrée. Ces lignes n'ont pas été prises en compte dans le traitement."))}

# on calcule le tableau de distribution pour chaque paramètre (et on les fusionne)
for(i in 1:length(seuils))
{
  donnees0<-donnees%>%subset(indice_seuil123456==i)
  if(nrow(donnees0)>0){ tableau0<-table_distribution(donnees0,seuil=seuils[[i]])%>%mutate(parametre=seuils[[i]]@nom_parametre,
                                                                                          code_parametre=seuils[[i]]@code_parametre,
                                                                                          code_support=seuils[[i]]@support,
                                                                                          code_fraction=seuils[[i]]@fraction,
                                                                                          code_unite=seuils[[i]]@code_unite)
                        ifelse(i==1, tableau<-tableau0, tableau<-bind_rows(tableau0, tableau))
                      }

}
# On ordonne les résultats par classe de qualité
    tableau$CATEGORIE<-factor(tableau$CATEGORIE, c(levels(tableau$CLASSE), paste0("<LQ et LQ de classe ", levels(tableau$CLASSE))))

  return(tableau)

}


