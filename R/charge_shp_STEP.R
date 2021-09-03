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
charge_shp_STEP <- function(crs = 2154, shp_emprise = NULL) {
  # on charge le shp des SAGE de France à partir de l'atlas carto du SANDRE
<<<<<<< HEAD
  url <- "https://services.sandre.eaufrance.fr/geo/odp"
=======
  url <- " https://services.sandre.eaufrance.fr/geo/odp"
>>>>>>> d77751fc882076e6e8224628bf63026514d7fb7c

  sf_prov <- url %>%
    parse_url() %>%
    list_merge(
      query = list(
        service = "wfs",
<<<<<<< HEAD
        version = "2.0.0",
=======
        version = "1.1.0",
>>>>>>> d77751fc882076e6e8224628bf63026514d7fb7c
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


  return(bel_regions)
}
