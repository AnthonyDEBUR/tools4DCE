#' charge_shp_SAGE
#'
#' charge sous forme d'objet sf le shape des SAGE en France (ou un extrait) à partir du flux wms du SANDRE
#'
#' @param nom_sage vecteur (optionnel) listant les périmètres de SAGE à retenir
#' @param crs valeur du code de projection dans lequel renvoyer le résultat (par défaut Lambert 93, indiquer 4326 pour du wgs84)
#'
#' @return la fonction renvoie un objet sf avec l'emprise des SAGE sélectionnés et les attributs correspondants
#' @examples Vilaine<-charge_shp_SAGE(nom_sage="Vilaine")
#' @export
charge_shp_SAGE<-function(nom_sage=NULL, crs=2154){

  # on charge le shp des SAGE de France à partir de l'atlas carto du SANDRE
  url <-"https://services.sandre.eaufrance.fr/geo/zpl"

  sf_prov <- url %>%
    parse_url() %>%
    list_merge(query = list(service = "wfs",
                            version = "1.1.0", # optional
                            request = "GetFeature",
                            typeName = "SAGE",
                            srsname='EPSG:4326',
                            outputFormat = "application/json; subtype=geojson")) %>%
    build_url()

  bel_regions <- read_sf(sf_prov, crs=4326)

  # On se limite aux SAGE sélectionnés
  if(!is.null(nom_sage)){bel_regions<-bel_regions%>%subset(NomZone%in%nom_sage)}

  # on reprojette dans le crs d'entrée
  bel_regions<-st_transform(bel_regions, crs = crs)


return(bel_regions)}


