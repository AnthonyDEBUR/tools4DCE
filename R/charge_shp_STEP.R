#' charge_shp_STEP
#'
#' charge sous forme d'objet sf le shape des stations d'épurations en France (ou un extrait) à partir du flux wms du SANDRE
#'
#' @param crs valeur du code de projection dans lequel renvoyer le résultat (par défaut Lambert 93, indiquer 4326 pour du wgs84)
#' @param shp_emprise objet SF qui délimite le périmètre sur lequel il faut renvoyer les stations
#' @param recupere_XY_pts_rejets si ce paramètre vaut vrai, alors le script interroge le site assainissement-durable.gouv.fr pour récupérer les XY des pts de rejet
#'
#' @return la fonction renvoie un objet sf des STEP tel que disponible sous l'atlas cartographique du SANDRE
#' @examples StepFR<-charge_shp_STEP()
#' @export
charge_shp_STEP <-
  function(crs = 2154,
           shp_emprise = NULL,
           recupere_XY_pts_rejets = F) {
    # on charge le shp des SAGE de France à partir de l'atlas carto du SANDRE

    url <- "https://services.sandre.eaufrance.fr/geo/odp"

    sf_prov <- url %>%
      parse_url() %>%
      list_merge(
        query = list(
          service = "wfs",
          version = "2.0.0",
          # optional
          request = "GetFeature",
          typeName = "SysTraitementEauxUsees",
          srsname = 'EPSG:4326',
          outputFormat = "application/json; subtype=geojson"
        )
      ) %>%
      build_url()

    bel_regions <- read_sf(sf_prov, crs = 4326)

    # sélection des STEP dans l'emprise de découpe
    if (!is.null(shp_emprise)) {
      # on reprojette dans en Lambert 93 avant decoupage
      bel_regions <- st_transform(bel_regions, crs = 2154)
      shp_emprise <- st_transform(shp_emprise, crs = 2154)

      # on découpe par rapport à l'emprise de l'objet shp_emprise
      bel_regions <- bel_regions[shp_emprise, ]
    }

    # on projette dans le crs de sortie
    bel_regions <- st_transform(bel_regions, crs = crs)

    tmp <-
      data.frame(
        code = seq(0, 5, by = 1) %>% as.character(),
        Type_station = c(
          "Inconnue",
          "Urbaine",
          "Industrielle",
          "Agricole",
          "Privé",
          "Mixte"
        )
      )
    bel_regions$CdNatureSystTraitementEauxUsees <-
      bel_regions$CdNatureSystTraitementEauxUsees %>% as.character()
    bel_regions <-
      left_join(bel_regions,
                tmp,
                by = c("CdNatureSystTraitementEauxUsees" = "code"))



    # sélection des colonnes d’intérêt
    bel_regions <-
      bel_regions %>% select(
        -c(
          "gid",
          "CdNatureSystTraitementEauxUsees",
          "CoordXOuvrageDepollution":"CdProjCoordOuvrageDepollution",
          "CdExistAutosurv":"CdAgglomerationAssainissement",
          "CdCommunePrincipale":"LbCommune",
          "SomChrgMaxEntree":"CdTypeOuvrageDepollution"
        )
      )


    # chargement des performances épuratoires et autres attributs SISPEA depuis https://www.services.eaufrance.fr/donnees/telechargement
    # boucle depuis 2008 jusqu'à l'année en cours (dans l'attente API hub eau interrogeable sur le code SANDRE station)

    dates <-
      seq(2008, Sys.Date() %>% format("%Y") %>% as.numeric() - 1, by = 1)

    sispea <- data.frame()

    for (i in 1:length(dates))
    {
      fichier <-
        paste0(
          "https://www.services.eaufrance.fr/telechargement/donnees/SISPEA_FR_",
          dates[i],
          "_AC.zip"
        )

      # on dezip le fichier et on le lit dans via un dossier temporaire
      tmp <- tempfile()
      try(download.file(fichier, destfile = tmp, mode = "wb"))
      tmp2 <- tempdir()
      unzip(tmp, exdir = tmp2)
      try(ajout <-
            read_excel(path = paste0(tmp2, "/SISPEA_FR_", dates[i], "_AC.xls"),
                       sheet = "Ouvrages"))
      try(ajout$ANNEE <- dates[i])
      ifelse(i == 1 | nrow(sispea) == 0,
             sispea <- ajout,
             sispea <-
               bind_rows(ajout, sispea))
      rm(ajout)
    }

    # pour chaque ouvrage épuratoire on conserve la dernière année saisie
    sispea <- sispea %>% subset(Statut != "En attente de saisie")
    sispea <-
      sispea %>% group_by(`Code SANDRE ouvrage`) %>% filter(ANNEE == max(ANNEE))

    sispea$VP.176 <- round(sispea$VP.176 * 1000 / 60, 0)

    # changement de noms de colonnes avec noms explicites
    sispea <-
      sispea %>% dplyr::rename(
        "qte_boues_t.MS_an" = "D203.0",
        "Collecte_conforme_DERU_%" = "P203.3",
        "Equipements_conformes_DERU_%" = "P204.3",
        "Perf._epuratoires_conformes_DERU_%" = "P205.3",
        "Taux_boues_conforme_DERU_%" = "P206.3",
        "Perf._epuratoires_conformes_AP_%" = "P254.3",
        "EH_entree" = "VP.176",
        "t_boues_evacuees" = "VP.209",
        "nb_bilans_24h_conformes" = "VP.210",
        "nb_bilans_24h_effectues" = "VP.211"
      )

    # ajout d'une url qui pointe vers le site eau france performance des services épuratoires
    sispea$url_sispea <-
      paste0(
        "<a href='https://www.services.eaufrance.fr/donnees/service/",
        sispea$`Id SISPEA de l'entité de gestion`,
        "' target='_blank'>lien SISPEA</a>"
      )

    # suppression des informations non nécessaires
    sispea <-
      sispea %>% select(-c("DPT du siège de la coll.":"Id SISPEA ouvrage", -"Nom ouvrage"))

    # on ajoute les infos SISPEA au fichier SANDRE
    bel_regions <-
      left_join(bel_regions,
                sispea,
                by = c("CdOuvrageDepollution" = "Code SANDRE ouvrage"))

    bel_regions$url_sandre <-
      paste0(
        "<a href='https://www.sandre.eaufrance.fr/geo/SysTraitementEauxUsees/",
        bel_regions$CdOuvrageDepollution,
        "' target='_blank'>lien SANDRE</a>"
      )

    bel_regions$url_portail_assainissement <-    paste0(
      "<a href='http://assainissement.developpement-durable.gouv.fr/fiche.php?code=",
      bel_regions$CdOuvrageDepollution,
      "' target='_blank'>lien portail assainissement</a>"
    )

    # récupération des XY des pts de rejet
    if (recupere_XY_pts_rejets)
    {
      bel_regions$Xrejet <- NA
      bel_regions$Yrejet <- NA
      liste_step <- bel_regions$CdOuvrageDepollution %>% unique()
      for (i in 1:length(liste_step))
      {
        webpage <-
          read_html(
            paste0(
              "http://assainissement.developpement-durable.gouv.fr/fiche.php?code=",
              liste_step[i]
            )
          )
        tmp <- webpage %>%
          rvest::html_element("body") %>% rvest::html_children() %>% rvest::html_children()
        tmp <- tmp[[5]] %>% rvest::html_children()
        tmp <- tmp[[7]] %>% as.character %>% str_split(",", simplify = T)
        bel_regions[bel_regions$CdOuvrageDepollution == liste_step[i], ]$Xrejet <-
          tmp[1, 1] %>% gsub("[^0-9.-]", "", .) %>% as.numeric
        bel_regions[bel_regions$CdOuvrageDepollution == liste_step[i], ]$Yrejet <-
          tmp[1, 2] %>% as.numeric
      }

    }


    return(bel_regions)
  }
