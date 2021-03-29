#' import_QESU_PHY_v2
#'
#' fonction pour importer un objet xml de type QUESU_PHY_V2 sous forme d'objet R
#'
#' @param x un fichier xml conforme au scénario d'échange QUESU_PHY_v2 du SANDRE
#'
#' @return la fonction renvoie une liste de data.frame avec les résultats contenus dans le fichier
#'
#' @examples x<-"C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\R_Anthony\\données qualité hors naiades\\04208570.xml"
#' @examples import_QESU_PHY_v2(x)
#' @export
import_QESU_PHY_v2 <- function(x) {
  # lecture et tests sur fichier d'entrée
  if (file_ext(x) != "xml") {
    stop("le fichier d'entrée n'est pas de type xml")
  }
  file <- read_xml(x)
  if (xml_ns(file)[1] != "http://xml.sandre.eaufrance.fr/scenario/quesu/2") {
    stop("le scénario du fichier n'est pas de type QUESU_PHY_V2")
  }

  # extraction des résultats
  divs <- file %>%  xml_child(2) %>% xml_contents()

  for (i in 1:length(divs))
  {
    print(paste0("Traitement ligne ", i, " sur ", length(divs)))

    # extraction de la station de mesure
    if (any(grepl("CdStationMesureEauxSurface", divs[i]))) {
      CdStationMesureEauxSurface <- divs[i] %>% xml_contents()
      CdStationMesureEauxSurface <-
        CdStationMesureEauxSurface[1] %>% as.character()
    }

    # noeuds prelevement
    if (grepl("PrelevementsPhysicoChimie", divs[i])) {
      valeurs <- divs[i] %>% xml_children()

      # recuperation des caracteristiques du prelevement
      DatePrel <-
        valeurs[grep("<DatePrel>", valeurs)] %>% xml_contents()
      DatePrel <- DatePrel[1] %>% as.character() %>% as.Date
      HeurePrel <-
        valeurs[grep("<HeurePrel>", valeurs)] %>% xml_contents()
      HeurePrel <-
        HeurePrel[1] %>% as.character() %>% substr(12, 19)
      if (length(HeurePrel) == 0) {
        HeurePrel <- NA
      }
      CdSupport <-
        valeurs[grep("<CdSupport>", valeurs)] %>% xml_contents() %>% xml_contents()
      CdSupport <- CdSupport[1] %>% as.character
      if (length(CdSupport) == 0) {
        CdSupport <- NA
      }
      CdProducteur <-
        valeurs[grep("<ProducteurPrelevement>", valeurs)] %>% xml_contents() %>% xml_contents()
      CdProducteur <- CdProducteur[1] %>% as.character
      if (length(CdProducteur) == 0) {
        CdProducteur <- NA
      }
      CdPreleveur <-
        valeurs[grep("<Preleveur>", valeurs)] %>% xml_contents() %>% xml_contents()
      CdPreleveur <- CdPreleveur[1] %>% as.character
      if (length(CdPreleveur) == 0) {
        CdPreleveur <- NA
      }
      CdPointEauxSurf <-
        valeurs[grep("<PointPrel>", valeurs)] %>% xml_contents() %>% xml_contents()
      CdPointEauxSurf <- CdPointEauxSurf[1] %>% as.character
      if (length(CdPointEauxSurf) == 0) {
        CdPointEauxSurf <- NA
      }
      CdRdd1 <-
        valeurs[grep("<CodeSandreRdd>", valeurs)] %>% xml_contents() %>% xml_contents()
      if (length(CdRdd1) > 0) {
        for (k in 1:length(CdRdd1)) {
          CdRdd0 <- CdRdd1[k] %>% as.character
          ifelse(k == 1,
                 CdRdd <- CdRdd0,
                 CdRdd <- paste0(CdRdd, "/", CdRdd0))
        }
      }


      if (length(CdRdd) == 0) {
        CdRdd <- NA
      }
      CdPrelevement <-
        valeurs[grep("<CdPrelevement>", valeurs)] %>% xml_contents()
      CdPrelevement <- CdPrelevement[1] %>% as.character
      if (length(CdPrelevement) == 0) {
        CdPrelevement <-
          paste0(DatePrel,
                 "-",
                 HeurePrel,
                 "-",
                 CdStationMesureEauxSurface,
                 "-",
                 CdPreleveur)
      }
      CoordXPrel <-
        valeurs[grep("<CoordXPrel>", valeurs)] %>% xml_contents() %>% xml_contents()
      CoordXPrel <- CoordXPrel[1] %>% as.character %>% as.numeric
      if (length(CoordXPrel) == 0) {
        CoordXPrel <- NA
      }
      CoordYPrel <-
        valeurs[grep("<CoordYPrel>", valeurs)] %>% xml_contents() %>% xml_contents()
      CoordYPrel <- CoordYPrel[1] %>% as.character %>% as.numeric
      if (length(CoordYPrel) == 0) {
        CoordYPrel <- NA
      }
      ProjectPrel <-
        valeurs[grep("<ProjectPrel>", valeurs)] %>% xml_contents() %>% xml_contents()
      ProjectPrel <- ProjectPrel[1] %>% as.character
      if (length(ProjectPrel) == 0) {
        ProjectPrel <- NA
      }
      CdMethode <-
        valeurs[grep("<CdMethode>", valeurs)]
      CdMethode <- CdMethode[1] %>% xml_contents()
      CdMethode <-
        CdMethode[grep("<CdMethode>", CdMethode)] %>% xml_contents() %>% xml_contents()
      CdMethode <- CdMethode[1] %>% as.character
      if (length(CdMethode) == 0) {
        CdMethode <- NA
      }
      DateFinPrel <-
        valeurs[grep("<DateFinPrel>", valeurs)] %>% xml_contents()
      DateFinPrel <- DateFinPrel[1] %>% as.character() %>% as.Date
      if (length(DateFinPrel) == 0) {
        DateFinPrel <- DatePrel
      }
      HeureFinPrel <-
        valeurs[grep("<HeureFinPrel>", valeurs)] %>% xml_contents()
      HeureFinPrel <-
        HeureFinPrel[1] %>% as.character() %>% substr(12, 19)
      if (length(HeureFinPrel) == 0) {
        HeureFinPrel <- NA
      }
      CommentairesPrel <-
        valeurs[grep("<CommentairesPrel>", valeurs)] %>% xml_contents()
      CommentairesPrel <- CommentairesPrel[1] %>% as.character()
      if (length(CommentairesPrel) == 0) {
        CommentairesPrel <- ""
      }
      CdZoneVerticaleProspectee <-
        valeurs[grep("<CdZoneVerticaleProspectee>", valeurs)] %>% xml_contents()
      CdZoneVerticaleProspectee <-
        CdZoneVerticaleProspectee[1] %>% as.character()
      if (length(CdZoneVerticaleProspectee) == 0) {
        CdZoneVerticaleProspectee <- NA
      }
      ProfondeurPrel <-
        valeurs[grep("<ProfondeurPrel>", valeurs)] %>% xml_contents()
      ProfondeurPrel <-
        ProfondeurPrel[1] %>% as.character() %>% as.numeric
      if (length(ProfondeurPrel) == 0) {
        ProfondeurPrel <- NA
      }
      CdDifficultePrel <-
        valeurs[grep("<CdDifficultePrel>", valeurs)] %>% xml_contents()
      CdDifficultePrel <- CdDifficultePrel[1] %>% as.character()
      if (length(CdDifficultePrel) == 0) {
        CdDifficultePrel <- NA
      }
      AgrePrel <-
        valeurs[grep("<AgrePrel>", valeurs)] %>% xml_contents()
      AgrePrel <- AgrePrel[1] %>% as.character()
      if (length(AgrePrel) == 0) {
        AgrePrel <- NA
      }
      CdFinalitePrel <-
        valeurs[grep("<CdFinalitePrel>", valeurs)] %>% xml_contents()
      CdFinalitePrel <- CdFinalitePrel[1] %>% as.character()
      if (length(CdFinalitePrel) == 0) {
        CdFinalitePrel <- NA
      }

      CdAccredPrel <-
        valeurs[grep("<AccredPrel>", valeurs)] %>% xml_contents()
      CdAccredPrel <- CdAccredPrel[1] %>% as.character()
      if (length(CdAccredPrel) == 0) {
        CdAccredPrel <- NA
      }


      # sauvegarde de la table operations
      ajout_operations <-
        data.frame(
          CdStationMesureEauxSurface = CdStationMesureEauxSurface,
          CdPrelevement = CdPrelevement,
          CoordXPrel = CoordXPrel,
          CoordYPrel = CoordYPrel,
          ProjectPrel = ProjectPrel,
          CdSupport = CdSupport,
          CdMethode = CdMethode,
          DatePrel = DatePrel,
          HeurePrel = HeurePrel,
          DateFinPrel = DateFinPrel,
          HeureFinPrel = HeureFinPrel,
          CdZoneVerticaleProspectee = CdZoneVerticaleProspectee,
          ProfondeurPrel = ProfondeurPrel,
          CdDifficultePrel = CdDifficultePrel,
          CdAccredPrel = CdAccredPrel,
          AgrePrel = AgrePrel,
          CdFinalitePrel = CdFinalitePrel,
          CommentairesPrel = CommentairesPrel,
          CdRdd = CdRdd
        )

      ifelse(
        !exists("operations_global"),
        operations_global <-
          ajout_operations,
        operations_global <-
          bind_rows(ajout_operations, operations_global)
      )


      # on extrait les résultats d'analyses
      Analyses <- valeurs[grep("<Analyse>", valeurs)]
      lit_analyses <- function(Analys)
      {
        # valeurs2 <- Analyses[j] %>% xml_children()
        valeurs2 <- Analys %>% xml_children()
        CdFractionAnalysee <-
          valeurs2[grep("<CdFractionAnalysee>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdFractionAnalysee <- CdFractionAnalysee[1] %>% as.character
        if (length(CdFractionAnalysee) == 0) {
          CdFractionAnalysee <- NA
        }
        DateAna <-
          valeurs2[grep("<DateAna>", valeurs2)] %>% xml_contents()
        DateAna <- DateAna[1] %>% as.character %>% as.Date
        if (length(DateAna) == 0) {
          DateAna <- NA
        }
        HeureAna <-
          valeurs2[grep("<HeureAna>", valeurs2)] %>% xml_contents()
        HeureAna <- HeureAna[1] %>% as.character %>% substr(12, 19)
        if (length(HeureAna) == 0) {
          HeureAna <- NA
        }
        CdParametre <-
          valeurs2[grep("<CdParametre>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdParametre <- CdParametre[1] %>% as.character
        if (length(CdParametre) == 0) {
          CdParametre <- NA
        }
        RsAna <-
          valeurs2[grep("<RsAna>", valeurs2)] %>% xml_contents()
        RsAna <- RsAna[1] %>% as.character %>% as.numeric
        if (length(RsAna) == 0) {
          RsAna <- NA
        }
        CdUniteMesure <-
          valeurs2[grep("<CdUniteReference>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdUniteMesure <- CdUniteMesure[1] %>% as.character
        if (length(CdUniteMesure) == 0) {
          CdUniteMesure <- NA
        }
        CdRqAna <-
          valeurs2[grep("<RqAna>", valeurs2)] %>% xml_contents()
        CdRqAna <- CdRqAna[1] %>% as.character
        if (length(CdRqAna) == 0) {
          CdRqAna <- NA
        }
        CdInsituAna <-
          valeurs2[grep("<CdInsituAna>", valeurs2)] %>% xml_contents()
        CdInsituAna <- CdInsituAna[1] %>% as.character
        if (length(CdInsituAna) == 0) {
          CdInsituAna <- NA
        }
        ProfondeurPrel <-
          valeurs2[grep("<ProfondeurPrel>", valeurs2)] %>% xml_contents()
        ProfondeurPrel <-
          ProfondeurPrel[1] %>% as.character %>% as.numeric
        if (length(ProfondeurPrel) == 0) {
          ProfondeurPrel <- NA
        }
        CdDifficulteAna <-
          valeurs2[grep("<DifficulteAna>", valeurs2)] %>% xml_contents()
        CdDifficulteAna <- CdDifficulteAna[1] %>% as.character
        if (length(CdDifficulteAna) == 0) {
          CdDifficulteAna <- NA
        }
        LdAna <-
          valeurs2[grep("<LDAna>", valeurs2)] %>% xml_contents()
        LdAna <- LdAna[1] %>% as.character %>% as.numeric
        if (length(LdAna) == 0) {
          LdAna <- NA
        }
        LqAna <-
          valeurs2[grep("<LQAna>", valeurs2)] %>% xml_contents()
        LqAna <- LqAna[1] %>% as.character %>% as.numeric
        if (length(LqAna) == 0) {
          LqAna <- NA
        }
        LsAna <-
          valeurs2[grep("<LSAna>", valeurs2)] %>% xml_contents()
        LsAna <- LsAna[1] %>% as.character %>% as.numeric
        if (length(LsAna) == 0) {
          LsAna <- NA
        }
        IncertAna <-
          valeurs2[grep("<IncertAna>", valeurs2)] %>% xml_contents()
        IncertAna <- IncertAna[1] %>% as.character
        if (length(IncertAna) == 0) {
          IncertAna <- NA
        }
        CdMetFractionnement <-
          valeurs2[grep("<MetFractionnement>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdMetFractionnement <-
          CdMetFractionnement[1] %>% as.character
        if (length(CdMetFractionnement) == 0) {
          CdMetFractionnement <- NA
        }
        CdMethode <-
          valeurs2[grep("<Methode>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdMethode <- CdMethode[1] %>% as.character
        if (length(CdMethode) == 0) {
          CdMethode <- NA
        }
        RdtExtraction <-
          valeurs2[grep("<RdtExtraction>", valeurs2)] %>% xml_contents()
        RdtExtraction <-
          RdtExtraction[1] %>% as.character %>% as.numeric
        if (length(RdtExtraction) == 0) {
          RdtExtraction <- NA
        }
        CdMethodeExtraction <-
          valeurs2[grep("<MetExtraction>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdMethodeExtraction <-
          CdMethodeExtraction[1] %>% as.character
        if (length(CdMethodeExtraction) == 0) {
          CdMethodeExtraction <- NA
        }
        CdAccreAna <-
          valeurs2[grep("<AccreAna>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdAccreAna <- CdAccreAna[1] %>% as.character
        if (length(CdAccreAna) == 0) {
          CdAccreAna <- NA
        }
        AgreAna <-
          valeurs2[grep("<AgreAna>", valeurs2)] %>% xml_contents() %>% xml_contents()
        AgreAna <- AgreAna[1] %>% as.character
        if (length(AgreAna) == 0) {
          AgreAna <- "0"
        }
        AgreAna <- ifelse(AgreAna == "1", T, F)
        CdStatutAna <-
          valeurs2[grep("<StatutAna>", valeurs2)] %>% xml_contents()
        CdStatutAna <- CdStatutAna[1] %>% as.character
        if (length(CdStatutAna) == 0) {
          CdStatutAna <- NA
        }
        CdQualAna <-
          valeurs2[grep("<QualAna>", valeurs2)] %>% xml_contents()
        CdQualAna <- CdQualAna[1] %>% as.character
        if (length(CdQualAna) == 0) {
          CdQualAna <- "0"
        }
        CommentairesAna <-
          valeurs2[grep("<CommentairesAna>", valeurs2)] %>% xml_contents()
        CommentairesAna <- CommentairesAna[1] %>% as.character
        if (length(CommentairesAna) == 0) {
          CommentairesAna <- ""
        }
        ComResultatAna <-
          valeurs2[grep("<ComResultatAna>", valeurs2)] %>% xml_contents()
        ComResultatAna <- ComResultatAna[1] %>% as.character
        if (length(ComResultatAna) == 0) {
          ComResultatAna <- ""
        }
        CdLaboratoire <-
          valeurs2[grep("<Laboratoire>", valeurs2)] %>% xml_contents() %>% xml_contents()
        CdLaboratoire <- CdLaboratoire[1] %>% as.character
        if (length(CdLaboratoire) == 0) {
          CdLaboratoire <- ""
        }

        # creation de la ligne à ajouter à la table analyses
        ajout_analyses <-
          data.frame(
            CdStationMesureEauxSurface = CdStationMesureEauxSurface,
            CdSupport = CdSupport,
            CdFractionAnalysee = CdFractionAnalysee,
            CdPrelevement = CdPrelevement,
            DatePrel = DatePrel,
            HeurePrel = HeurePrel,
            DateAna = DateAna,
            HeureAna = HeureAna,
            CdParametre = CdParametre,
            RsAna = RsAna,
            CdUniteMesure = CdUniteMesure,
            CdRqAna = CdRqAna,
            CdInsituAna = CdInsituAna,
            ProfondeurPrel = ProfondeurPrel,
            CdDifficulteAna = CdDifficulteAna,
            LdAna = LdAna,
            LqAna = LqAna,
            LsAna = LsAna,
            IncertAna = IncertAna,
            CdMetFractionnement = CdMetFractionnement,
            CdMethode = CdMethode,
            RdtExtraction = RdtExtraction,
            CdMethodeExtraction = CdMethodeExtraction,
            CdAccreAna = CdAccreAna,
            AgreAna = AgreAna,
            CdStatutAna = CdStatutAna,
            CommentairesAna = CommentairesAna,
            ComResultatAna = ComResultatAna,
            CdRdd = CdRdd,
            CdProducteur = CdProducteur,
            CdPreleveur = CdPreleveur,
            CdLaboratoire = CdLaboratoire
          )
        return(ajout_analyses)

      }
      output <- lapply(Analyses, lit_analyses)

      # enregistrement des conditions environnementales
      ajout_analyses <- do.call(rbind, output)

      ifelse(
        !exists("analyses_global"),
        analyses_global <-
          ajout_analyses,
        analyses_global <-
          bind_rows(analyses_global, ajout_analyses)

      )

      # on extrait les résultats de conditions environnementales
      Cond_Env <- valeurs[grep("<MesureEnvironnementale>", valeurs)]
      if (length(Cond_Env) > 0) {
        output <- lapply(Cond_Env, function(j) {
          valeurs2 <- j %>% xml_children()
          CdParametreEnv <-
            valeurs2[grep("<ParametreEnv>", valeurs2)] %>% xml_contents() %>% xml_contents()
          CdParametreEnv <- CdParametreEnv[1] %>% as.character
          if (length(CdParametreEnv) == 0) {
            CdParametreEnv <- NA
          }
          RsParEnv <-
            valeurs2[grep("<RsParEnv>", valeurs2)] %>% xml_contents()
          RsParEnv <- RsParEnv[1] %>% as.character %>% as.numeric
          if (length(RsParEnv) == 0) {
            RsParEnv <- NA
          }
          CdUniteMesure <-
            valeurs2[grep("<CdUniteReference>", valeurs2)] %>% xml_contents() %>% xml_contents()
          CdUniteMesure <- CdUniteMesure[1] %>% as.character
          if (length(CdUniteMesure) == 0) {
            CdUniteMesure <- NA
          }
          CdRqParEn <-
            valeurs2[grep("<RqParEn>", valeurs2)] %>% xml_contents()
          CdRqParEn <- CdRqParEn[1] %>% as.character
          if (length(CdRqParEn) == 0) {
            CdRqParEn <- NA
          }
          CdStatutParEn <-
            valeurs2[grep("<StatutParEn>", valeurs2)] %>% xml_contents()
          CdStatutParEn <- CdStatutParEn[1] %>% as.character
          if (length(CdStatutParEn) == 0) {
            CdStatutParEn <- NA
          }
          CdQualParEnv <-
            valeurs2[grep("<QualParEnv>", valeurs2)] %>% xml_contents()
          CdQualParEnv <- CdQualParEnv[1] %>% as.character
          if (length(CdQualParEnv) == 0) {
            CdQualParEnv <- "0"
          }
          ComParEnv <-
            valeurs2[grep("<ComParEnv>", valeurs2)] %>% xml_contents()
          ComParEnv <- ComParEnv[1] %>% as.character
          if (length(ComParEnv) == 0) {
            ComParEnv <- ""
          }
          DateParEnv <-
            valeurs2[grep("<DateParEnv>", valeurs2)] %>% xml_contents()
          DateParEnv <- DateParEnv[1] %>% as.character %>% as.Date
          if (length(DateParEnv) == 0) {
            DateParEnv <- NA
          }
          HeureParEnv <-
            valeurs2[grep("<HeureParEnv>", valeurs2)] %>% xml_contents()
          HeureParEnv <-
            HeureParEnv[1] %>% as.character %>% substr(12, 19)
          if (length(HeureParEnv) == 0) {
            HeureParEnv <- NA
          }
          CdMethodeParEnv <-
            valeurs2[grep("<Methode>", valeurs2)] %>% xml_contents() %>% xml_contents()
          CdMethodeParEnv <- CdMethodeParEnv[1] %>% as.character
          if (length(CdMethodeParEnv) == 0) {
            CdMethodeParEnv <- NA
          }
          CdProducteur <-
            valeurs2[grep("<Producteur>", valeurs2)] %>% xml_contents() %>% xml_contents()
          CdProducteur <- CdProducteur[1] %>% as.character
          if (length(CdProducteur) == 0) {
            CdProducteur <- ""
          }
          CdPreleveur <-
            valeurs2[grep("<Preleveur>", valeurs2)] %>% xml_contents() %>% xml_contents()
          CdPreleveur <- CdPreleveur[1] %>% as.character
          if (length(CdPreleveur) == 0) {
            CdPreleveur <- ""
          }


          # creation de la ligne à ajouter à la table Cond_Env
          ajout_cond_env <-
            data.frame(
              CdStationMesureEauxSurface = CdStationMesureEauxSurface,
              CdPrelevement = CdPrelevement,
              DatePrel = DatePrel,
              CdParametreEnv = CdParametreEnv,
              RsParEnv = RsParEnv,
              CdUniteMesure = CdUniteMesure,
              CdRqParEn = CdRqParEn,
              CdStatutParEn = CdStatutParEn,
              CdQualParEnv = CdQualParEnv,
              ComParEnv = ComParEnv,
              DateParEnv = DateParEnv,
              HeureParEnv = HeureParEnv,
              CdMethodeParEnv = CdMethodeParEnv,
              CdProducteur = CdProducteur,
              CdPreleveur = CdPreleveur
            )
          return(ajout_cond_env)

        })

        # enregistrement des conditions environnementales
        ajout_cond_env <- do.call(rbind, output)
        ifelse(
          !exists("cond_env_global"),
          cond_env_global <-
            ajout_cond_env,
          cond_env_global <-
            bind_rows(cond_env_global, ajout_cond_env)
        )

      }
    }

  }

<<<<<<< HEAD
  if(!exists("cond_env_global")){cond_env_global<-NULL}
=======
  if(!exists("cond_env_global"){cond_env_global<-NULL})
>>>>>>> 4877561de4d4640df3973c7d2df88a236883c28a


  return(
    list(
      analyses = analyses_global,
      cond_env = cond_env_global,
      operations = operations_global
    )
  )
}
