% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcule_somme_pesticides.R
\name{calcule_somme_pesticides}
\alias{calcule_somme_pesticides}
\title{calcule_somme_pesticides}
\usage{
calcule_somme_pesticides(
  data,
  liste_pesticides = NULL,
  col_parametre = "CdParametre",
  col_date = "DatePrel",
  col_valeur = "RsAna",
  col_CdRq = "CdRqAna",
  col_LQ = "LqAna",
  col_station = "CdStationMesureEauxSurface",
  col_unite = "CdUniteMesure",
  valeur_inf_LQ = "0",
  resultat_seul = T
)
}
\arguments{
\item{data}{tableau de données avec les résultats d'analyse}

\item{liste_pesticides}{vecteur qui contient les identifiants des pesticides à prendre en compte. Si NULL, toutes les molécules du tableau sont prises en compte.}

\item{col_parametre}{nom de la colonne qui identifie les pesticides. Par défaut CdParametre}

\item{col_date}{nom de la colonne avec la date du prélèvement. Par defaut DatePrel.}

\item{col_valeur}{nom de la colonne avec les résultats d'analyse. Par défaut RsAna.}

\item{col_CdRq}{nom de la colonne avec le code remarque d'analyse. Par défaut CdRqAna.}

\item{col_LQ}{nom de la colonne avec les limites de quantification des analyses. Par défaut LqAna.}

\item{col_station}{nom de la colonne qui renseigne sur où se trouve les différentes stations. Par défaut CdStationMesureEauxSurface}

\item{col_unite}{nom de la colonne avec les unités/ Par défaut CdUniteMesure}

\item{valeur_inf_LQ}{stratégie à appliquer pour les valeurs inférieures à LQ. Par défaut "0" : on remplace les valeurs inférieures à la LQ par 0. Autre possibilité : "LQ/2" : on remplace les valeurs inférieures à LQ par LQ/2. "LQ" : on remplace les valeurs inférieures à LQ par LQ.}

\item{resultat_seul}{booléen. Si il vaut TRUE, la fonction ne renvoie que la colonne somme pesticides. Si il vaut false, la fonction renvoie une colonne par paramètre pris en compte}
}
\value{
la fonction renvoie une dataframe avec les informations sur la station, la date, l'unité et la valeur de la somme des pesticides ainsi qu'une colonne avec chaque pesticide constituant la somme.
}
\description{
fonction pour calculer la somme des concentrations en pesticides.
Si pour une même station et une même date il y a plusieurs résultats du même paramètre alors on retient la valeur max de ces résultats.
Si pour une même station et une même date on trouve un paramètre inclus dans un deuxième (ex. S-métolachlore et métolachlore total), alors seule la valeur du paramètre qui englobe l'autre (métolachore total par ex.) est retenu.
paramètres incluant d'autres paramètres :
\itemize{
\item métolachlore total
\item mecoprop
\item Somme des Hexachlorocyclohexanes
\item Somme Heptachlore époxyde cis/trans
\item Somme du DDE 44' et de la dieldrine
\item Somme des metabolites des dithiocarbamates (6235) = Ethylenethiouree (5648) + Ethyluree (5484) + Propylene thiouree (6214)
\item Somme de Ethylamine + Diméthylamine (	7887) = Ethylamine (6993) + Diméthylamine (2773)
\item Somme du Fenvalerate RR et Esfenvalerate SS (6613) = Fenvalerate RR 6606 + Esfenvalerate SS 6608
\item Somme des chloroanilines (m+p) 5502	= Chloroaniline-4 1591 + Chloroaniline-3 1592
\item Somme Acétochlore ESA + Alachlore ESA (7750) = Acétochlore ESA 6856 + Alachlore ESA 6800
\item Somme du DDD 44' et du DDT 24'6496 = DDD 44' 1144 + DDT 24' 1147
\item Somme Metacresol, Orthocresol et Paracrésol 6341 = ortho-crésol 1640 + méta-crésol 1639 + para-crésol 1638
\item Somme parathion ethyl+methyl 6947	= parathion éthyl 1232 + parathion methyl 1233
\item Somme du DDD 24', DDE 24', DDT 24', DDT 44' 7170 =  DDD 24' 1143 + DDE 24' 1145 + DDT 24' 1147 + DDT 44' 1148
\item Somme du DDDpp', DDEpp', DDTop', DDTpp' 7146 = DDDpp' 1144 + DDEpp' 1146 + DDTop' 1147 + DDTpp' 1148
\item Somme de l'Alachlor OXA et de l'Acetochlor OXA 8101	= Alachlor OXA 6855 + Acetochlor OXA 6862
}
}
\examples{
data<-data.frame(DatePrel=Sys.Date() + rep(sort(sample(1:500, 10)),3), RsAna=c(round(runif(60,0,0.5), 2)), LqAna=c(0.1), CdStationMesureEauxSurface=c("A","B","C"), CdParametre=c("1200","1201"), CdUniteMesure="133")
data$CdRqAna<-ifelse(data$RsAna>=data$LqAna, "1","10")
calcule_somme_pesticides(data)
}
