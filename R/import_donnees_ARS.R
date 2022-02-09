#' import_donnees_ARS
#'
#' fonction pour importer un fichier .zip exporté depuis le site data.gouv.fr sous forme d'objets R
#' https://www.data.gouv.fr/api/1/datasets/resultats-du-controle-sanitaire-de-leau-du-robinet/
#'
#' @param x un fichier .zip exporté depuis la base nationale Naïades
#' @param shp objet sf facultatif. Si le paramètre est renseigné, seules les données acquises à l'intérieur du périmètre sont conservées.
#' @param type= type de données : soit "eau_brute" (par défaut : correspond à la ressource en eau brute),soit "traitement" : correspond aux données sur l'eau traitée,  soit "eau_distribuee" : correspond à l'eau distribuée.
#'
#' @return la fonction renvoie une liste de data.frame avec les résultats contenus dans le fichier
#'
#' @examples x<-"http://data.cquest.org/sise-eaux/eaurob-2016.zip"
#' @examples fichier<-paste0(tempdir(), "\\donnees.zip")
#' @examples download.file(x, destfile=fichier, mode="wb")
#' @examples import_donnees_ARS(fichier, charge_shp_SAGE("Vilaine"), type="eau_brute")
#' @export
import_donnees_ARS <- function(x, shp=NULL, type="eau_brute") {
  # lecture et tests sur fichier d'entrée
  if (file_ext(x) != "zip") {
    stop("le fichier d'entrée n'est pas de type zip")
  }
liste_fichiers<-unzip(x, list = T)
  if(!all(
    any(grepl("CAP_PLV",liste_fichiers$Name%>%toupper)),
    any(grepl("CAP_RES",liste_fichiers$Name%>%toupper)),
    any(grepl("TTP_PLV",liste_fichiers$Name%>%toupper)),
    any(grepl("TTP_RES",liste_fichiers$Name%>%toupper)),
    any(grepl("UDI_PLV",liste_fichiers$Name%>%toupper)),
    any(grepl("UDI_RES",liste_fichiers$Name%>%toupper))
     )
          ){
    stop("Le fichier zip ne contient pas tous les fichiers requis (CAP_PLV, CAP_RES, TTP_PLV, TTP_RES, UDI_PLV, UDI_RES).")
  }

tmp_dir<-tempdir()
  # decompression du fichier
  unzip(x, exdir=tmp_dir)



  # lecture du fichier prelevement eaux brutes
  fichier_prel_eaux_brutes<-grep("CAP_PLV", liste_fichiers$Name, value=T)
  prelevement<-(paste0(tmp_dir, "\\", fichier_prel_eaux_brutes))

  prelevements<-read_csv(prelevement)



  lit_fichier_ars<-function(fichier){
    # fichier = chemin d'accès au fichier txt exporté depuis ARS

    # CAP = eau brute


     # lecture du fichier
    Stations<-read_delim(fichier, col_names = T,
                         col_types = cols(.default = "c"), delim=";")

    # on convertit les colonnes dont le nom commence par "Cd" en factor
    a_conv<-substr(names(Stations),1,2)=="Cd"

    Stations<-Stations%>%mutate_if(a_conv, as.factor)

    # on convertit les colonnes dont le nom commence par "Ana" en numeric
    a_conv<-substr(names(Stations),3,5)=="Ana"

    Stations<-Stations%>%mutate_if(a_conv, as.numeric)

    # on convertit les colonnes dont le nom commence par "Coord" en numeric
    a_conv<-substr(names(Stations),1,5)=="Coord"
    Stations<-Stations%>%mutate_if(a_conv, as.numeric)

    # on convertit les colonnes dont le nom commence par "Date" en date
    a_conv<-substr(names(Stations),1,4)=="Date"
    Stations<-Stations%>%mutate_if(a_conv, as.Date)

  }

  ##### lecture des fichiers analyses #####

  # lecture du fichier analyses
  analyses<-lit_fichier_naiades(paste0(tmp_dir,"/Analyses.csv"))

  # extraction des codes remarques (non disponibles sous forme de données de référence sous le SANDRE)
  CdRqAna<-analyses%>%select(CdRqAna, MnemoRqAna)%>%distinct

  # extraction des codes unités (non disponibles sous forme de données de référence sous le SANDRE au 03/12/2020)
  CdUniteMesure<-analyses%>%select(CdUniteMesure, SymUniteMesure)%>%distinct

  # extraction des codes analyses insitu ou pas
  CdInsituAna<-analyses%>%select(CdInsituAna, LbInsituAna)%>%distinct

  # extraction des codes acréditation analyse
  CdAccreAna<-analyses%>%select(CdAccreAna, MnemoAccredAna)%>%distinct

  # conversion de l'information analyse agréée en booléen comme prévu au SANDRE
  analyses$AgreAna<-ifelse(analyses$AgreAna=="1",T,F)

  # extraction des codes statuts analyse
  CdStatutAna<-analyses%>%select(CdStatutAna, MnemoStatutAna)%>%distinct

  # extraction des codes qualification analyse
  CdQualAna<-analyses%>%select(CdQualAna, LbQualAna)%>%distinct


  # extraction des codes statuts analyse
  CdDifficulteAna<-analyses%>%select(CdDifficulteAna, MnemoDifficulteAna)%>%distinct

  # extraction des codes Producteur et Noms producteurs
  CdProducteur<-analyses%>%select(CdProducteur, NomProducteur)%>%distinct

    # on ne conserve que les colonnes indispensables
  analyses<-analyses%>%select("CdStationMesureEauxSurface", "CdSupport", "CdFractionAnalysee", "CdPrelevement", "DatePrel", "HeurePrel", "DateAna", "HeureAna",
                              "CdParametre", "RsAna", "CdUniteMesure", "CdRqAna", "CdInsituAna", "ProfondeurPrel","CdDifficulteAna",
                              "LdAna", "LqAna", "LsAna", "IncertAna","CdMetFractionnement", "CdMethode", "RdtExtraction", "CdMethodeExtraction", "CdAccreAna",
                              "AgreAna", "CdStatutAna", "CdQualAna", "CommentairesAna", "ComResultatAna", "CdRdd", "CdProducteur","CdPreleveur", "CdLaboratoire")


  ##### lecture des fichiers conditions environnementales #####

  # lecture du fichier analyses
  ConditionsEnvironnementales<-lit_fichier_naiades(paste0(tmp_dir,"/ConditionsEnvironnementales.csv"))

  # on ne conserve que les colonnes indispensables
  ConditionsEnvironnementales<-ConditionsEnvironnementales%>%select("CdStationMesureEauxSurface", "DatePrel", "CdParametreEnv", "RsParEnv", "CdUniteMesure", "CdRqParEn",
                                                                    "CdStatutParEn", "CdQualParEnv", "ComParEnv", "DateParEnv", "HeureParEnv", "CdMethodeParEnv", "CdProducteur", "CdPreleveur")


  ##### lecture des fichiers Operations #####

  # lecture du fichier Operations
  Operations<-lit_fichier_naiades(paste0(tmp_dir,"/Operations.csv"))

  # on ne conserve que les colonnes indispensables
  Operations<-Operations%>%select("CdStationMesureEauxSurface", "CdPrelevement", "CoordXPrel", "CoordYPrel", "ProjectPrel",
                                  "CdSupport", "CdMethode", "DatePrel", "HeurePrel", "DateFinPrel", "HeureFinPrel", "CdZoneVerticaleProspectee",
                                  "ProfondeurPrel", "CdDifficultePrel", "CdAccredPrel", "AgrePrel", "CdFinalitePrel",
                                  "CommentairesPrel", "CdRdd")


  ##### lecture des fichiers Stations #####

  # lecture du fichier Stations
  Stations<-lit_fichier_naiades(paste0(tmp_dir,"/Stations.csv"))

# on supprime les doublons
analyses<-analyses%>%distinct()
ConditionsEnvironnementales<-ConditionsEnvironnementales%>%distinct()
Operations<-Operations%>%distinct
Stations<-Stations%>%distinct

##### On ne retient que les stations localisées dans le périmètre du shp d'entrée #####

# on convertit l'objet Stations en couche sig, on affiche sous mapview pour vérifier qu'il n'existe pas de stations avec des erreurs de projection
Stations$X<-Stations$CoordXStationMesureEauxSurface
Stations$Y<-Stations$CoordYStationMesureEauxSurface
Stations<-st_as_sf(Stations, coords=c("X","Y"), crs=2154)

# on ne conserve que les stations qui intersectent le shp
if(!is.null(shp)){Stations<-Stations[shp,]}

# on filtre les données en ne conservant que celles pour les stations sur le shp
analyses<-analyses%>%subset(CdStationMesureEauxSurface%in%Stations$CdStationMesureEauxSurface)
ConditionsEnvironnementales<-ConditionsEnvironnementales%>%subset(CdStationMesureEauxSurface%in%Stations$CdStationMesureEauxSurface)
Operations<-Operations%>%subset(CdStationMesureEauxSurface%in%Stations$CdStationMesureEauxSurface)

# on complète les LQ manquantes qui correspondent à un code remarque = 10
analyses<-mutate(analyses, LqAna = ifelse(CdRqAna == 10 & is.na(LqAna), RsAna, LqAna))

# on converti la colonne profondeur en numeric
analyses$ProfondeurPrel<-as.numeric(analyses$ProfondeurPrel)


  return(
    list(
      analyses = analyses,
      cond_env = ConditionsEnvironnementales,
      operations = Operations,
      CdRqAna=CdRqAna,
      CdUniteMesure=CdUniteMesure,
      CdInsituAna=CdInsituAna,
      CdAccreAna=CdAccreAna,
      CdStatutAna=CdStatutAna,
      CdQualAna=CdQualAna,
      CdDifficulteAna=CdDifficulteAna,
      CdProducteur=CdProducteur

    )
  )
}

