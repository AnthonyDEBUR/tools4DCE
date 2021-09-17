#' charge_shp_STEP
#'
#' charge sous forme d'objet sf le shape des stations d'épurations en France (ou un extrait) à partir du flux wms du SANDRE
#'
#' @param crs valeur du code de projection dans lequel renvoyer le résultat (par défaut Lambert 93, indiquer 4326 pour du wgs84)
#' @param shp_emprise objet SF qui délimite le périmètre sur lequel il faut renvoyer les stations
#'
#' @return la fonction renvoie un objet sf des STEP tel que disponible sous l'atlas cartographique du SANDRE
#' @examples StepFR<-charge_shp_STEP()
#' @export
charge_shp_STEP <-
  function(crs = 2154,
           shp_emprise = NULL) {
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
      bel_regions <- bel_regions[shp_emprise,]
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
      sispea %>% select(-c("DPT du siège de la coll.":"Id SISPEA ouvrage",-"Nom ouvrage"))

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

    rejets <-
      data.frame(
        CdOuvrageDepollution = bel_regions$CdOuvrageDepollution,
        Xrejet = NA,
        Yrejet = NA
      )


    for (i in 1:length(bel_regions$CdOuvrageDepollution))
    {
      webpage <-
        read_html(
          paste0(
            "http://assainissement.developpement-durable.gouv.fr/fiche.php?code=",
            bel_regions$CdOuvrageDepollution[i]
          )
        )
      tmp <- webpage %>%
        rvest::html_element("body") %>% rvest::html_children() %>% rvest::html_children()
      tmp <- tmp[[5]] %>% rvest::html_children()
      tmp <-
        tmp[[7]] %>% as.character %>% str_split(",", simplify = T)
      rejets[i,]$Yrejet <-
        tmp[1, 1] %>% gsub("[^0-9.-]", "", .) %>% as.numeric
      rejets[i,]$Xrejet <-
        tmp[1, 2] %>% as.numeric
    }

    # conversion des points de rejet en shp
    rejets <-
      st_as_sf(rejets[!is.na(rejets$Xrejet), ],
               coords = c("Xrejet", "Yrejet"),
               crs = 4326)


    # reprojection dans le crs demandé
    rejets <- st_transform(rejets, crs = crs)

    # creation shp_liaison STEP / pt rejet

    coord_rejets <- rejets %>%
      mutate(lat2 = unlist(map(rejets$geometry, 1)),
             lon2 = unlist(map(rejets$geometry, 2))) %>% as.data.frame

    coord_STEP <- bel_regions %>%
      mutate(lat1 = unlist(map(bel_regions$geometry, 1)),
             lon1 = unlist(map(bel_regions$geometry, 2))) %>% as.data.frame


    tmp <-
      left_join(coord_rejets, coord_STEP, by = "CdOuvrageDepollution") %>%
      select("CdOuvrageDepollution", "lon1", "lat1", "lon2", "lat2")

    make_line <- function(lon1, lat1, lon2, lat2) {
      st_linestring(matrix(c(lat1, lat2,lon1, lon2), 2, 2))
    }


    tmp<-tmp %>%
      select(-CdOuvrageDepollution) %>%
      pmap(make_line) %>%
      st_as_sfc(crs = crs) %>%
      {
        tibble(CdOuvrageDepollution = tmp$CdOuvrageDepollution, geometry = .)
      } %>%
      st_sf()



  return(list(shp=bel_regions, shp_rejets=rejets, liaison_STEP_rejet=tmp))
  }
