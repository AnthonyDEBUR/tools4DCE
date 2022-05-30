#' charge_shp_ICPE
#'
#' charge sous forme d'objet le shape des ICPE depuis https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles
#' et les émissions polluantes depuis https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles-rejetant-des-polluants
#'
#' @param shp_emprise vecteur (optionnel) objet sf dans lequel on va récupérer les ICPE
#' @param crs valeur du code de projection dans lequel renvoyer le résultat (par défaut Lambert 93, indiquer 4326 pour du wgs84)
#' @pram rubrique booléen : si vrai (valeur par défaut), le script appelle chaque page internet d'ICPE pour récupérer les rubriques de classement.
#'
#' @return la fonction renvoie une liste avec un slot shp = objet sf avec les ICPE de l'emprise indiquée,
#' @return avec un slot rejets = table avec les rejets référencés dans le registre national des émissions polluantes,
#' @return et un slot prelevements avec les prélèvements d'eau issus de ce registre
#' @examples ICPE<-charge_shp_ICPE(shp_emprise=charge_shp_SAGE(nom_sage="Vilaine"))
#' @export
charge_shp_ICPE <-
  function(crs = 2154,
           shp_emprise = NULL,
           rubrique = TRUE) {
    # on charge le shp des ICPE de France à partir de https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles
    url <- "https://mapsref.brgm.fr/wxs/georisques/georisques_dl"

    sf_prov <- url %>%
      parse_url() %>%
      list_merge(
        query = list(
          service = "wfs",
          version = "2.0.0",
          # optional
          request = "getfeature",
          typeName = "InstallationsClassees",
          #   srsname = 'EPSG:4326',
          outputFormat = "SHAPEZIP"
        )
      ) %>%
      build_url()

    tmp <- tempfile()
    download.file(sf_prov, destfile = tmp, mode = "wb")

    tmp2 <- tempdir()

    unzip(tmp, exdir = tmp2)

    bel_regions <-
      read_sf(paste0(tmp2, "\\InstallationsClassees.shp"), crs = 4326)

    # sélection des ICPE dans l'emprise de découpe
    if (!is.null(shp_emprise)) {
      # on reprojette dans en Lambert 93 avant decoupage
      bel_regions <- st_transform(bel_regions, crs = 2154)
      shp_emprise <- st_transform(shp_emprise, crs = 2154)

      # on découpe par rapport à l'emprise de l'objet shp_emprise
      bel_regions <- bel_regions[shp_emprise,]
    }

    # on projette dans le crs de sortie
    bel_regions <- st_transform(bel_regions, crs = crs)

    # mise en forme de la table attributaire
    bel_regions$LIEN_FICHE <-
      paste0("<a href='",
             bel_regions$url_fiche,
             "' target='_blank'>Lien georisques</a>")


    # parcours des rubriques ICPE par site pour préciser
    if (rubrique == TRUE) {
      bel_regions$actif <- ""
      bel_regions$piscicultures <- 0
      bel_regions$autre_elevages <- ""
      bel_regions$nomenclature_IC <- ""

      for (i in 1:nrow(bel_regions)) {
        print(paste0("recuperation donnees site", i, " sur ", nrow(bel_regions)))
        webpage <- rvest::read_html(bel_regions$url_fiche[i])
        etat_activite <-
          webpage %>% html_nodes('.mb-5') %>% html_nodes("p") %>% html_text()
        seveso <-
          sub('.*SEVESO : ', "", etat_activite[grepl("Statut SEVESO ", etat_activite)][1])
        actif <-
          sub('.*activité : ', "", etat_activite[grepl("activité", etat_activite)][1])
        regime <-
          sub('.* : ', "", etat_activite[grepl("Régime en vigueur", etat_activite)][1])
        SIRET <-
          sub('.* : ', "", etat_activite[grepl("SIRET", etat_activite)][1])
        Prio_FR <-
          sub('.* : ', "", etat_activite[grepl("Priorité nationale", etat_activite)][1])
        IED_MTD <-
          sub('.* : ', "", etat_activite[grepl("IED - MTD", etat_activite)][1])

        bel_regions$lib_seveso[i] <- seveso
        bel_regions$num_siret[i] <- SIRET
        bel_regions$actif[i] <- actif
        bel_regions$lib_regime[i] <- regime
        bel_regions$priorite_n[i] <- ifelse(Prio_FR == "Oui", 1, 0)
        bel_regions$ied[i] <- ifelse(IED_MTD == "Oui", 1, 0)


        tableau_autorisation <-
          webpage %>% rvest::html_elements(xpath = '//*[@id="situation-administrative"]') %>%
          rvest::html_table()
        tableau_autorisation<-tableau_autorisation[[1]]


        if (!all(is.na(tableau_autorisation))) {
          bovins<-NA
          porcs<-NA
          volailles<-NA
          pisciculture<-NA
          autre_elevage<-NA
          eolienne<-NA

          try(bovins <-
            sum(tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2101"),]$Volume %>%
                  parse_number(),
                na.rm = T))
          try(porcs <-
            sum(tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2102"),]$Volume %>%
                  parse_number(),
                na.rm = T))
          try(volailles <-
            sum(tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2111"),]$Volume %>%
                  parse_number(),
                na.rm = T))
          try(pisciculture <-
            sum(tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2130"),]$Volume %>%
                  parse_number(),
                na.rm = T))
          try(autre_elevage <-
            ifelse(
              sum(
                tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2110", "2113", "2120", "2140", "2150"),]$Volume %>%
                  parse_number(),
                na.rm = T
              ) > 0,
              "oui",
              "non"
            ))
          try(carriere <-
            ifelse(sum(
              tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2510"),]$Volume %>%
                parse_number(),
              na.rm = T
            ) > 0,
            1,
            0))
          try(eolienne <-
                ifelse(sum(
                  tableau_autorisation[tableau_autorisation$`Code rubrique` %in% c("2980"),]$Volume %>%
                    parse_number(),
                  na.rm = T
                ) > 0,
                1,
                0))


          bel_regions$bovins[i] <- bovins
          bel_regions$porcs[i] <- porcs
          bel_regions$volailles[i] <- volailles
          bel_regions$piscicultures[i] <- pisciculture
          bel_regions$autre_elevages[i] <- autre_elevage
          bel_regions$eolienne[i]<-eolienne
        }

        rm(bovins, porcs, volailles, pisciculture, autre_elevage, eolienne)

        bel_regions$nomenclature_IC[i] <- tableau_autorisation %>%
          tableHTML() %>% as.character

      }
    }


    bel_regions <-
      bel_regions %>% dplyr::select(
        -c(
          "x",
          "y",
          "code_epsg",
          "num_dep":"code_naf",
          "cd_regime",
          "seveso",
          "url_fiche"
        )
      )



    # création d'une colonne famille_ic à partir des types d'activités (pour remettre en conformité avec ancien format de fichiers)
    bel_regions$famille_ic <- "Industries"

    bel_regions <- bel_regions %>% mutate(
      famille_ic = case_when(
        bovins >
          0 ~
          "Bovins",
        porcs > 0 ~ "Porcs",
        volailles > 0 ~ "Volailles",
        carriere > 0 ~ "Carrières",
        T ~ famille_ic
      )
    )




    # téléchargement des inventaires des émissions polluantes https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles-rejetant-des-polluants
    dates <-
      seq(2003, Sys.Date() %>% format("%Y") %>% as.numeric(), by = 1)

    emissions <- data.frame()
    prelevements <- data.frame()

    for (i in 1:length(dates))
    {
      fichier <-
        paste0("https://files.georisques.fr/irep/",
               dates[i],
               ".zip")

      # on dezip le fichier et on le lit dans via un dossier temporaire
      tmp <- tempfile()
      try(download.file(fichier, destfile = tmp, mode = "wb"))
      tmp2 <- tempdir()
      unzip(tmp, exdir = tmp2)
      ajout <- data.frame()
      try(ajout <-
            read.csv2(paste0(tmp2, "/", dates[i] , "/emissions.csv"), encoding =
                        "UTF-8"))
      try(ajout$identifiant <- as.character(ajout$identifiant))
      if (nrow(ajout) > 0) {
        ifelse((i == 1 | nrow(emissions) == 0),
               emissions <- ajout,
               emissions <-
                 bind_rows(ajout, emissions))
      }
      rm(ajout)

      ajout <- data.frame()
      try(ajout <-
            read.csv2(paste0(tmp2, "/", dates[i], "/prelevements.csv"), encoding =
                        "UTF-8"))
      try(ajout <-
            ajout %>% dplyr::mutate(across(starts_with("prelevements"), ~ as.numeric(.))))
      try(ajout$identifiant <- as.character(ajout$identifiant))
      if (nrow(ajout) > 0) {
        ifelse(
          i == 1 |
            nrow(prelevements) == 0,
          prelevements <- ajout,
          prelevements <-
            bind_rows(ajout, prelevements)
        )
      }
      rm(ajout)
    }

    # mise en forme des tableaux d'emissions et de prélèvements
    emissions$identifiant <-
      gsub("[^0-9]", "", emissions$identifiant)

    emissions$identifiant <-
      paste0(
        substr(emissions$identifiant, 1, nchar(emissions$identifiant) - 5),
        #   ".",
        substr(
          emissions$identifiant,
          nchar(emissions$identifiant) - 4,
          nchar(emissions$identifiant)
        )
      )

    emissions$identifiant <-
      paste0(sapply(
        emissions$identifiant,
        FUN = function(x) {
          ifelse(10 - nchar(x) > 0, paste0(rep("0", 10 - nchar(x)), collapse = ""), "")
        }
      ),
      emissions$identifiant)

    emissions <-
      emissions %>% subset(
        milieu %in% c("Eau (indirect)", "Eau (direct)") &
          identifiant %in% bel_regions$code_aiot
      )

    emissions <-
      emissions %>%  group_by(identifiant, polluant) %>%
      pivot_wider(names_from = annee_emission, values_from = quantite)

    emissions <- emissions %>% dplyr::select(-nom_etablissement)


    prelevements$identifiant <-
      gsub("[^0-9]", "", prelevements$identifiant)

    prelevements$identifiant <-
      paste0(
        substr(
          prelevements$identifiant,
          1,
          nchar(prelevements$identifiant) - 5
        ),
        #   ".",
        substr(
          prelevements$identifiant,
          nchar(prelevements$identifiant) - 4,
          nchar(prelevements$identifiant)
        )
      )

    prelevements$identifiant <-
      paste0(sapply(
        prelevements$identifiant,
        FUN = function(x) {
          ifelse(10 - nchar(x) > 0, paste0(rep("0", 10 - nchar(x)), collapse = ""), "")
        }
      ),
      prelevements$identifiant)
    prelevements <-
      prelevements %>% subset(identifiant %in% bel_regions$code_aiot)

    prelevements <-
      prelevements %>% pivot_longer(
        cols = starts_with("prelevements"),
        names_to = "milieu",
        names_prefix = "prelevements_",
        values_drop_na = T
      )
    prelevements <-
      prelevements %>% group_by(identifiant, milieu) %>%
      pivot_wider(names_from = annee, values_from = value)

    prelevements <-
      prelevements %>% dplyr::select(-nom_etablissement)



    # ajout d'un tableau avec les emissions dans bel_regions

    bel_regions$emissions <- NA

    id_em <- unique(emissions$identifiant)
    if (length(id_em) > 0)
    {
      for (i in 1:length(id_em))
      {
        print(paste0(i, " sur ", length(id_em)))

        # on choisit les emissions du site concerné.
        tmp <- emissions %>% subset(identifiant == id_em[i])

        # On ne conserve que les colonnes de date non totalement vides
        tmp <-
          tmp %>% dplyr::select_if( ~ !all(is.na(.))) %>% ungroup

        # selection des 3 dernières années renseignées
        tmp <-
          tmp %>% dplyr::select(milieu:unite,
                                names(tmp) %>% as.numeric %>% sort %>% tail(4) %>% as.character)

        # On supprime les lignes composées exclusivement de NA
        tmp <-
          tmp %>% filter(if_any(starts_with("20"), ~ !is.na(.)))

        bel_regions[bel_regions$code_aiot == id_em[i], ]$emissions <-
          tmp %>%
          tableHTML() %>% as.character

      }
    }


    # ajout d'un tableau avec les prélèvements dans bel_regions

    bel_regions$prelevements <- NA

    id_em <- unique(prelevements$identifiant)

    if (length(id_em) > 0) {
      for (i in 1:length(id_em))
      {
        print(paste0(i, " sur ", length(id_em)))

        # on choisit les prelevements du site concerné.
        tmp <- prelevements %>% subset(identifiant == id_em[i])

        # On ne conserve que les colonnes de date non totalement vides
        tmp <-
          tmp %>% dplyr::select_if( ~ !all(is.na(.))) %>% ungroup

        # selection des 3 dernières années renseignées
        tmp <-
          tmp %>% dplyr::select(milieu,
                                names(tmp) %>% as.numeric %>% sort %>% tail(4) %>% as.character)

        # On supprime les lignes composées exclusivement de NA
        tmp <-
          tmp %>% filter(if_any(starts_with("20"), ~ !is.na(.)))

        bel_regions[bel_regions$code_aiot == id_em[i], ]$prelevements <-
          tmp %>%
          tableHTML() %>% as.character

      }
    }


    return(bel_regions)
  }
