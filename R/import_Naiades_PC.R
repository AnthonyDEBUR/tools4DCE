#' import_Naiades_PC
#'
#' fonction pour importer un fichier .zip exporté de Naiades sous forme d'objets R
#'
#' @param x un fichier .zip exporté depuis la base nationale Naïades
#' @param shp objet sf factultatif. Si le paramètre est renseigné, seules les données acquises à l'intérieur du périmètre sont conservées.
#'
#' @return la fonction renvoie une liste de data.frame avec les résultats contenus dans le fichier
#'
#' @examples x<-"C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\R_Anthony\\Naiades\\exports_naiades\\naiades_export_1950_1989_au 1er decembre 2020.zip"
#' @examples import_Naiades_PC(x, charge_shp_SAGE("Vilaine"))
#' @export
import_Naiades_PC <- function(x, shp=NULL) {
  # lecture et tests sur fichier d'entrée
  if (file_ext(x) != "zip") {
    stop("le fichier d'entrée n'est pas de type zip")
  }
liste_fichiers<-unzip(x, list = T)
  if(!all(c("analyses.csv","operations.csv", "conditionsenvironnementales.csv", "stations.csv")%in%(liste_fichiers$Name%>%tolower))){
    stop("Le fichier zip ne contient pas tous les csv requis (analyses.csv, operations.csv, conditionsenvironnementales.csv et stations.csv")
  }

tmp_dir<-tempdir()
  # decompression du fichier
  unzip(x, exdir=tmp_dir)


  lit_fichier_naiades<-function(fichier){
    # fichier = chemin d'accès au fichier csv exporté depuis Naiades

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
  CdRqAna<-analyses%>%dplyr::select(CdRqAna, MnemoRqAna)%>%distinct

  # extraction des codes unités (non disponibles sous forme de données de référence sous le SANDRE au 03/12/2020)
  CdUniteMesure<-analyses%>%dplyr::select(CdUniteMesure, SymUniteMesure)%>%distinct

  # extraction des codes analyses insitu ou pas
  CdInsituAna<-analyses%>%dplyr::select(CdInsituAna, LbInsituAna)%>%distinct

  # extraction des codes acréditation analyse
  CdAccreAna<-analyses%>%dplyr::select(CdAccreAna, MnemoAccredAna)%>%distinct

  # conversion de l'information analyse agréée en booléen comme prévu au SANDRE
  analyses$AgreAna<-ifelse(analyses$AgreAna=="1",T,F)

  # extraction des codes statuts analyse
  CdStatutAna<-analyses%>%dplyr::select(CdStatutAna, MnemoStatutAna)%>%distinct

  # extraction des codes qualification analyse
  CdQualAna<-analyses%>%dplyr::select(CdQualAna, LbQualAna)%>%distinct


  # extraction des codes statuts analyse
  CdDifficulteAna<-analyses%>%dplyr::select(CdDifficulteAna, MnemoDifficulteAna)%>%distinct

  # extraction des codes Producteur et Noms producteurs
  CdProducteur<-analyses%>%dplyr::select(CdProducteur, NomProducteur)%>%distinct

    # on ne conserve que les colonnes indispensables
  analyses<-analyses%>%dplyr::select("CdStationMesureEauxSurface", "CdSupport", "CdFractionAnalysee", "CdPrelevement", "DatePrel", "HeurePrel", "DateAna", "HeureAna",
                              "CdParametre", "RsAna", "CdUniteMesure", "CdRqAna", "CdInsituAna", "ProfondeurPrel","CdDifficulteAna",
                              "LdAna", "LqAna", "LsAna", "IncertAna","CdMetFractionnement", "CdMethode", "RdtExtraction", "CdMethodeExtraction", "CdAccreAna",
                              "AgreAna", "CdStatutAna", "CdQualAna", "CommentairesAna", "ComResultatAna", "CdRdd", "CdProducteur","CdPreleveur", "CdLaboratoire")


  ##### lecture des fichiers conditions environnementales #####

  # lecture du fichier analyses
  ConditionsEnvironnementales<-lit_fichier_naiades(paste0(tmp_dir,"/ConditionsEnvironnementales.csv"))

  # on ne conserve que les colonnes indispensables
  ConditionsEnvironnementales<-ConditionsEnvironnementales%>%dplyr::select("CdStationMesureEauxSurface", "DatePrel", "CdParametreEnv", "RsParEnv", "CdUniteMesure", "CdRqParEn",
                                                                    "CdStatutParEn", "CdQualParEnv", "ComParEnv", "DateParEnv", "HeureParEnv", "CdMethodeParEnv", "CdProducteur", "CdPreleveur")


  ##### lecture des fichiers Operations #####

  # lecture du fichier Operations
  Operations<-lit_fichier_naiades(paste0(tmp_dir,"/Operations.csv"))

  # on ne conserve que les colonnes indispensables
  Operations<-Operations%>%dplyr::select("CdStationMesureEauxSurface", "CdPrelevement", "CoordXPrel", "CoordYPrel", "ProjectPrel",
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

