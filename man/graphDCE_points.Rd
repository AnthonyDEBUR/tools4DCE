% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphDCE_points.R
\name{graphDCE_points}
\alias{graphDCE_points}
\title{graphDCE_points}
\usage{
graphDCE_points(
  data,
  col_dates = "DatePrel",
  col_valeurs = "RsAna",
  col_LQ = "LqAna",
  seuils = NULL,
  affiche_legende = T,
  nom_legende = NULL,
  titre = NULL,
  taille_titre = 12,
  sous_titre = NULL,
  taille_sous_titre = 11,
  unite = NULL,
  bilan_annuel = FALSE,
  xmini = NULL,
  xmaxi = NULL,
  ymini = NULL,
  ymaxi = NULL,
  auto_ymaxi = TRUE,
  lignes = NULL,
  echelleLog = FALSE,
  taille_points = 2,
  taille_legende = 12,
  taille_axes = 11,
  liaison = T,
  affiche_LQ = F,
  separ_stations = NULL,
  alpha = 0.8,
  coef_axes_date = 1
)
}
\arguments{
\item{data}{tableau de données (dataframe)}

\item{col_dates}{nom de la colonne qui contient les dates de prélèvements. Par défaut : "DatePrel" formats Date ou POSIXct}

\item{col_valeurs}{nom de la colonne qui contient les valeurs d'analyses.Par défaut : "RsAna"}

\item{col_LQ}{nom de la colonne qui contient la valeur de LqAna. Par défaut : "LqAna"}

\item{seuils}{objet de classe seuil (factultatif)}

\item{affiche_legende}{booléen indiquant s'il faut afficher la légende (par défaut TRUE)}

\item{nom_legende}{titre de la légende (si absent et si seuils et renseigné, la légende par défaut sera le slot nom_seuil de la légende, si seuil n'est pas renseigné, la légende sera "Légende")}

\item{titre}{titre du graphique (si absent et si seuils et renseigné, la légende par défaut sera le slot nom_parametre)}

\item{taille_titre}{taille police du titre (par défaut 12)}

\item{sous_titre}{sous titre du graphique}

\item{taille_sous_titre}{taille police du sous titre (par défaut 10)}

\item{unite}{unité du paramètre (par défaut si seuils est renseigné : le label SANDRE correspondant au slot code_unite du paramètre seuils)}

\item{bilan_annuel}{si le paramètre vaut TRUE : on ajuste le graphique sur les années civiles. S'il vaut FALSE (valeur par défaut) : on laisse l'échelle graphique libre}

\item{xmini}{année mini (si non précisé, sera calculé automatiquement comme étant le 1er janvier de l'année correspondant au minimum des dates du jeu de données). Format : "2010"}

\item{xmaxi}{année maxi (si non précisé, sera calculé automatiquement comme étant le 31 décembre de l'année correspondant au maximum des dates du jeu de données). Format : "2010"}

\item{ymini}{force l'échelle des valeurs (si non précisé, échelle automatique de ggplot2)}

\item{ymaxi}{force l'échelle des valeurs (si non précisé, échelle automatique de ggplot2)}

\item{auto_ymaxi}{booléen. Si le paramètre vaut TRUE et que ymaxi n'est pas renseigné alors ce dernier est calculé automatiquement de manière à rendre le graph aussi lisible que possible}

\item{lignes}{vecteur permettant d'ajouter des lignes horizontales au graphique. ex c(10, 25)}

\item{echelleLog}{booléen : si vrai le graphique est affiché en échelle logarithmique (FALSE par défaut)}

\item{taille_points}{numerique avec la taille des points (2 par défaut)}

\item{taille_legende}{taille police caractères de la légende}

\item{taille_axes}{taille police de caractères des axes}

\item{liaison}{option faisant afficher ou pas des lignes pointillées entre les points (défaut = TRUE)}

\item{affiche_LQ}{option qui affiche une zone grisée correspondant à la LqAna (par défaut non affiché)}

\item{separ_stations}{nom de la colonne par rapport à laquelle séparer les données avec différents shape (par exemple pour distinguer les résultats de plusieurs stations de mesures)}

\item{alpha}{transparence des applats de couleurs}

\item{coef_axes_date}{coefficient qui détermine le nombre de graduations pour les dates (valeur par défaut : 1). Une valeur 2 correspond au double de repères qu'une valeur de 1}
}
\value{
la fonction renvoie un graphique de classe ggplot
}
\description{
fonction pour afficher les graphiques des paramètres en couleurs (mode point)
}
\examples{
data<-data.frame(DatePrel=c("2020-01-01", "2020-05-03","2020-10-25", "2021-03-25")\%>\%as.Date, RsAna=c(12,15.5,67,18.3))
graphDCE_points(data, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), taille_points=5)
graphDCE_points(data, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), ymaxi=51)

}
