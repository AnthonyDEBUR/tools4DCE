#' calcule_somme_pesticides
#'
#' fonction pour calculer la somme des concentrations en pesticides.
#' Si pour une même station et une même date il y a plusieurs résultats du même paramètre alors on retient la valeur max de ces résultats.
#' Si pour une même station et une même date on trouve un paramètre inclus dans un deuxième (ex. S-métolachlore et métolachlore total), alors seule la valeur du paramètre qui englobe l'autre (métolachore total par ex.) est retenu.
#' paramètres incluant d'autres paramètres :
#' - métolachlore total
#' - mecoprop
#' - Somme des Hexachlorocyclohexanes
#' - Somme Heptachlore époxyde cis/trans
#'
#' @param data tableau de données avec les résultats d'analyse
#' @param liste_pesticides vecteur qui contient les identifiants des pesticides à prendre en compte. Si NULL, toutes les molécules du tableau sont prises en compte.
#' @param col_parametre nom de la colonne qui identifie les pesticides. Par défaut CdParametre
#' @param col_date nom de la colonne avec la date du prélèvement. Par defaut DatePrel.
#' @param col_valeur nom de la colonne avec les résultats d'analyse. Par défaut RsAna.
#' @param col_CdRq nom de la colonne avec le code remarque d'analyse. Par défaut CdRqAna.
#' @param col_LQ nom de la colonne avec les limites de quantification des analyses. Par défaut LqAna.
#' @param col_station nom de la colonne qui renseigne sur où se trouve les différentes stations. Par défaut CdStationMesureEauxSurface
#' @param col_unite nom de la colonne avec les unités/ Par défaut CdUniteMesure
#' @param valeur_inf_LQ stratégie à appliquer pour les valeurs inférieures à LQ. Par défaut "0" : on remplace les valeurs inférieures à la LQ par 0. Autre possibilité : "LQ/2" : on remplace les valeurs inférieures à LQ par LQ/2. "LQ" : on remplace les valeurs inférieures à LQ par LQ.
#'
#' @return la fonction renvoie une dataframe avec les informations sur la station, la date, l'unité et la valeur de la somme des pesticides ainsi qu'une colonne avec chaque pesticide constituant la somme.
#'
#'@examples data<-data.frame(DatePrel=Sys.Date() + rep(sort(sample(1:500, 10)),3), RsAna=c(round(runif(60,0,0.5), 2)), LqAna=c(0.1), CdStationMesureEauxSurface=c("A","B","C"), CdParametre=c("1200","1201"))
#'@examples data$CdRqAna<-ifelse(data$RsAna>=data$LqAna, "1","10")
#' @examples calcule_somme_pesticides(data)
#' @export
calcule_somme_pesticides <- function(data, liste_pesticides=NULL, col_parametre="CdParametre", col_date="DatePrel", col_valeur="RsAna",
                                     col_CdRq="CdRqAna", col_LQ="LqAna", col_station="CdStationMesureEauxSurface", col_unite="CdUniteMesure",
                                     valeur_inf_LQ="0") {
  # teste si le format en entrée est correct
  if (!valeur_inf_LQ%in%c("0", "LQ/2","LQ")) {
    stop(
      "valeur_inf_LQ ne correspond pas à une valeur possible ('0','LQ/2','LQ')"
    )}

  # mise en forme du tableau de donnees
  data1<-data
  data1$CdParametre<-data[[col_parametre]]
  data1$DatePrel<-data[[col_date]]
  data1$RsAna<-data[[col_valeur]]
  data1$CdRqAna<-data[[col_CdRq]]
  data1$LqAna<-data[[col_LQ]]
  data1$CdStationMesure<-data[[col_station]]
  data1$CdUniteMesure<-data[[col_unite]]

  # si liste des pesticides n'est pas nulle, on ne retient que les pesticides de la liste
  if(!is.null(liste_pesticides)){data1<-data1%>%subset(CdParametre%in%liste_pesticides)}

  # si la liste des données est nulle, on renvoie un message d'erreur
  if(nrow(data1)==0){stop("Aucune donnée pesticides parmi le tableau de données.")}

  # si les unites sont differentes, on renvoie un message d'erreur
  if(length(unique(data1$CdUniteMesure))>1){stop("Toutes les analyses doivent être dans la même unité pour pouvoir calculer les sommes de pesticides.")}

  unite<-data1$CdUniteMesure[1]

  # on applique la strategie LQ
  if(valeur_inf_LQ=="0"){data1[data1$CdRqAna!="1",]$RsAna<-0}
  if(valeur_inf_LQ=="LQ/2"){data1[data1$CdRqAna!="1",]$RsAna<-data1[data1$CdRqAna!="1",]$LqAna/2}
  if(valeur_inf_LQ=="LQ"){data1[data1$CdRqAna!="1",]$RsAna<-data1[data1$CdRqAna!="1",]$LqAna}

  # on fait un tableau croise par date / code station
  data2<-pivot_wider(data1, id_cols=c("CdStationMesure", "DatePrel"), names_from="CdParametre", names_prefix="par_", values_from = "RsAna", values_fn = max, values_fill = NA)

  # cas du métolachlore
  #  Métolachlore total (1221) > S-Métolachlore (2974) > Métolachlore énantiomère S (8070) +  Métolachlore énantiomère R (8071)
  if(any(c("1221", "2974")%in%liste_pesticides)){if(is.null(data2$par_1221)){data2 <- data2 %>% mutate(par_1221 = NA)}}
  if (!is.null(data2$par_1221)) {
    if (is.null(data2$par_2974)) {
      data2 <- data2 %>% mutate(par_2974 = 0)
    }
    if (is.null(data2$par_8070)) {
      data2 <- data2 %>% mutate(par_8070 = 0)
    }
    if (is.null(data2$par_8071)) {
      data2 <- data2 %>% mutate(par_8071 = 0)
    }
    # on somme les 2 énantiomères
    data2$par_80708071 <-
      rowSums(data2[, c("par_8070", "par_8071")], na.rm = TRUE)
    # on prend le max entre les 2 énantiomères et le S-métolachlore
    data2$par_80708071 <-
      apply(data2[, c("par_80708071", "par_2974")], 1, function(x) {
        max(x, na.rm = T)
      })
    # on remplace le paramètre Métolachlore total s'il n'est pas renseigné
    data2 <-
      data2 %>% mutate(par_1221 = ifelse(!is.na(par_1221),
                                         par_1221,
                                         par_80708071))
# on supprime les colonnes hors métolachlore total
    data2<-data2%>%select(-par_2974, -par_8070, -par_8071)
    }

  # cas du mecoprop
  #  Mécoprop (1214) > Mécoprop-P (2084)
  if (!is.null(data2$par_1214)) {
    if (is.null(data2$par_2084)) {
      data2 <- data2 %>% mutate(par_2084 = 0)
    }

    # on prend le max entre l'énantiomère et la molécule totale
    data2$par_1214 <-
      apply(data2[, c("par_1214", "par_2084")], 1, function(x) {
        max(x, na.rm = T)
      })
    # on supprime les colonnes hors Mécoprop
    data2<-data2%>%select(-par_2084)

  }


  #' - Somme des Hexachlorocyclohexanes
  #' - Somme Heptachlore époxyde cis/trans






















#   data<-analyses%>%subset(CdSupport=="3")
#   liste_pesticides<-liste_pest_2020_ARS56$CdParametre
# liste_pesticides<-c(liste_pesticides, "2974", "8070", "8071")

  return(result)
}
