#' charge_shp_ICPE
#'
#' charge sous forme d'objet le shape des ICPE depuis https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles
#' et les émissions polluantes depuis https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles-rejetant-des-polluants
#'
#' @param shp_emprise vecteur (optionnel) objet sf dans lequel on va récupérer les ICPE
#' @param crs valeur du code de projection dans lequel renvoyer le résultat (par défaut Lambert 93, indiquer 4326 pour du wgs84)
#'
#' @return la fonction renvoie une liste avec un slot shp = objet sf avec les ICPE de l'emprise indiquée,
#' @return avec un slot rejets = table avec les rejets référencés dans le registre national des émissions polluantes,
#' @return et un slot prelevements avec les prélèvements d'eau issus de ce registre
#' @examples ICPE<-charge_shp_ICPE(nom_sage="Vilaine")
#' @export
charge_shp_ICPE <- function(crs = 2154, shp_emprise = NULL) {
  # on charge le shp des SAGE de France à partir de https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles
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
    bel_regions <- bel_regions[shp_emprise, ]
  }

  # on projette dans le crs de sortie
  bel_regions <- st_transform(bel_regions, crs = crs)

  # mise en forme de la table attributaire
  bel_regions$LIEN_FICHE <-
    paste0("<a href='",
           bel_regions$url_fiche,
           "' target='_blank'>Lien georisques</a>")

  bel_regions<-bel_regions%>%select(-c("x", "y", "epsg", "num_dep":"code_naf", "regime", "seveso", "rayon", "precis_loc", "url_fiche"))

  # On recode les élevages soumis à autorisation dans les familles d'élevage et non dans la rubrique "Industries"
  bel_regions <- bel_regions %>% mutate(
    famille_ic = case_when(
      lib_naf ==
        "Élevage de volailles" ~
        "Volailles",
      lib_naf == "Élevage de porcins" ~ "Porcs",
      lib_naf == "Élevage d'autres bovins et de buffles" ~ "Bovins",
      lib_naf == "Élevage de vaches laitières" ~ "Bovins",

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
  emissions$identifiant <- paste0("0", emissions$identifiant)
  emissions <-
    emissions %>% subset(
      milieu %in% c("Eau (indirect)", "Eau (direct)") &
        identifiant %in% bel_regions$code_s3ic
    )

  emissions <-
    emissions %>%  group_by(identifiant, polluant) %>%
    pivot_wider(names_from = annee_emission, values_from = quantite)

  emissions <- emissions %>% select(-nom_etablissement)


  prelevements$identifiant <- paste0("0", prelevements$identifiant)
  prelevements <-
    prelevements %>% pivot_longer(
      cols = starts_with("prelevements"),
      names_to = "milieu",
      names_prefix = "prelevements_",
      values_drop_na = T
    )
  prelevements <- prelevements %>% group_by(identifiant, milieu) %>%
    pivot_wider(names_from = annee, values_from = value)

  prelevements <- prelevements %>% select(-nom_etablissement)


  # ajout d'un tableau avec les emissions dans bel_regions

  bel_regions$emissions<-NA

  id_em<-unique(emissions$identifiant)
  for(i in 1:length(id_em))
  {
    print(paste0(i, " sur ", length(id_em)))

    # on choisit les emissions du site concerné.
    tmp<-emissions%>%subset(identifiant==id_em[i])

    # On ne conserve que les colonnes de date non totalement vides
    tmp<-tmp%>% select_if(~!all(is.na(.)))%>%ungroup

    # selection des 3 dernières années renseignées
    tmp<-tmp%>%select(milieu:unite, names(tmp)%>%as.numeric%>%sort%>%tail(4)%>%as.character)

    # On supprime les lignes composées exclusivement de NA
    tmp<-tmp%>% filter(if_any(starts_with("20"), ~ !is.na(.)))

    bel_regions[bel_regions$code_s3ic==id_em[i],]$emissions<-tmp%>%
      tableHTML()%>%as.character

  }


  # ajout d'un tableau avec les prélèvements dans bel_regions

  bel_regions$prelevements<-NA

  id_em<-unique(prelevements$identifiant)
  for(i in 1:length(id_em))
  {
    print(paste0(i, " sur ", length(id_em)))

    # on choisit les prelevements du site concerné.
    tmp<-prelevements%>%subset(identifiant==id_em[i])

    # On ne conserve que les colonnes de date non totalement vides
    tmp<-tmp%>% select_if(~!all(is.na(.)))%>%ungroup

    # selection des 3 dernières années renseignées
    tmp<-tmp%>%select(milieu, names(tmp)%>%as.numeric%>%sort%>%tail(4)%>%as.character)

    # On supprime les lignes composées exclusivement de NA
    tmp<-tmp%>% filter(if_any(starts_with("20"), ~ !is.na(.)))

    bel_regions[bel_regions$code_s3ic==id_em[i],]$prelevements<-tmp%>%
      tableHTML()%>%as.character

  }


  return(bel_regions)
}
