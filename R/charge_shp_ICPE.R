#' charge_shp_ICPE
#'
#' charge sous forme d'objet sf le shape des ICPE depuis https://www.georisques.gouv.fr/donnees/bases-de-donnees/installations-industrielles
#'
#' @param nom_sage vecteur (optionnel) listant les périmètres de SAGE à retenir
#' @param crs valeur du code de projection dans lequel renvoyer le résultat (par défaut Lambert 93, indiquer 4326 pour du wgs84)
#'
#' @return la fonction renvoie un objet sf avec l'emprise des SAGE sélectionnés et les attributs correspondants
#' @examples Vilaine<-charge_shp_SAGE(nom_sage="Vilaine")
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

  tmp<-tempfile()
  download.file(sf_prov, destfile = tmp, mode="wb")

  tmp2<-tempdir()

  unzip(tmp, exdir=tmp2)



  bel_regions <- read_sf(paste0(tmp2,"\\InstallationsClassees.shp"),crs = 4326)

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
  return(bel_regions)
}
