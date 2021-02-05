library(tidyverse)
library(tools4DCE)
library(openxlsx)

# creation fichier xlsx à ajouter à la base seuils. Ce fichier contient seulement les seuils AEP
donnees_oeb<-read_csv2("tables_ref\\oeb_referentiels_substances_actives.csv")%>%
  mutate(CdParametre=SA_CodeSANDRE%>%as.character)%>%ajoute_nom_param()

data("base_seuils")

base_seuils<-base_seuils%>%subset(TYPE=="AEP")
donnees_oeb<-donnees_oeb%>%subset(!CdParametre%in%base_seuils$PARAMETRE)
donnees_oeb$TYPE<-"AEP"
donnees_oeb$SPECIFICITE<-"OEB"

seuils_AEP_general <-
  data.frame(
    TYPE = "AEP",
    SEUILMIN = c(-Inf, 0.1, 2),
    SEUILMAX = c(0.1, 2, Inf),
    CLASSE = c(
      "[0;seuil distribution]",

      "]seuil distribution; seuil potabilisation]",
      ">seuil potabilisation"
    )
  )

donnees_oeb<-donnees_oeb%>%left_join(seuils_AEP_general, by="TYPE")
donnees_oeb$NOM<-donnees_oeb$NomParametre
donnees_oeb$SUPPORT<-"3"
donnees_oeb$FRACTION<-"23"
donnees_oeb$PARAMETRE<-donnees_oeb$CdParametre
donnees_oeb$UNITE<-"133"
donnees_oeb$TYPE_BORNE<-"BORNE_INF_INCLUE"
donnees_oeb$NOM_SEUIL<-"AM.11/01/2007"
donnees_oeb$SEUILMIN<-donnees_oeb$SEUILMAX%>%as.character()
donnees_oeb$SEUILMAX<-donnees_oeb$SEUILMAX%>%as.character()

donnees_oeb<-donnees_oeb%>%select(NOM,SUPPORT,	FRACTION,	PARAMETRE,	UNITE,	SEUILMIN,	SEUILMAX,
                     CLASSE,	NOM_SEUIL,	TYPE,	TYPE_BORNE,	SPECIFICITE)

write.xlsx(donnees_oeb, "tables_ref/base_seuils_oeb.xlsx")


