#' import_hubeau_staq_hbio
#'
#' Importe depuis Hubeau une liste de station avec des indices hydrobiologiques
#' https://hubeau.eaufrance.fr/page/api-hydrobiologie
#'
#' Si les paramètres emprise et liste stations sont présents tous les 2, seul le paramètre liste_stations est pris en compte.
#'
#' @param emprise objet sf optionnel décrivant l'emprise des stations à récupérer (sans précision on récupère sur la france entière)
#' @param liste_stations vecteur de character listant les codes sandre des stations à exporter. Longueur maximum de ce vecteur : 100 stations (restrictions inhérentes à l'API)
#' @param an_debut début de la période d'export (par défaut 1900)
#' @param an_fin fin de la période d'export (par défaut l'année en court)
#' @param indice vecteur avec la liste des indices à recherche (dia = diatomées, mphy = macrophytes, inv = macroinvertébrés, poi = poisson, oli = oligochètes). Par défaut la totalité des indices est sélectionnée.
#'
#' @return la fonction renvoie un tibble avec la liste des stations de mesures concernées
#' @examples Vilaine<-charge_shp_SAGE(nom_sage="Vilaine")
#' @examples import_hubeau_staq_hbio(emprise=Vilaine, an_debut=2010)
#' @examples import_hubeau_staq_hbio(liste_stations=c("03174000", "04216000"))
#' @export
import_hubeau_staq_hbio <-
  function(emprise = NULL,
           liste_stations = NULL,
           an_debut = 1900,
           an_fin = format(Sys.Date(), "%Y") %>% as.numeric(),
           indice = c("dia", "mphy", "inv", "poi", "oli")) {
    bbox_emprise <- NULL

    if (class(an_debut) != "numeric") {
      stop("an_debut doit être un objet de classe numérique")
    }
    if (class(an_fin) != "numeric") {
      stop("an_fin doit être un objet de classe numérique")
    }
    if (an_debut > an_fin) {
      stop("an_debut doit être inférieur ou égale à an_fin")
    }


    # la liste stations l'emporte sur l'emprise
    if (!is.null(liste_stations)) {
      if (!class(liste_stations) %in% c("character", "factor")) {
        stop("liste_stations doit être de classe character ou factor")
      }
      if (length(liste_stations) > 100) {
        stop(
          "en raison de restrictions de l'API hub-eau, le paramètre liste_stations ne doit pas comporter plus de 100 codes stations."
        )
      }
      if (!is.null(emprise)) {
        emprise <- NULL
      }
    }


    # reprojection de l'emprise en WGS84
    if (!is.null(emprise)) {
      if (!("sf" %in% class(emprise))) {
        stop("l'emprise doit être un objet de classe sf")
      }
      if (is.na(st_crs(emprise))) {
        stop("système de projection (crs) de l'emprise non défini (?st_crs).")
      }

      emprise <- st_transform(emprise, crs = 4326)

      bbox_emprise <- st_bbox(emprise)

    }

    if (!(class(indice) %in% c("character", "factor"))) {
      stop("La liste des indices à rechercher doit être de classe character ou factor")
    }

    support <-
      data.frame(
        ind = c("dia", "mphy", "inv", "poi", "oli"),
        codes_supports = c(10, 27, 13, 4, 29)
      )
    support <- support[support$ind %in% indice,]$codes_supports

    if (length(support)==0) {
      stop("La liste des supports fournie n'est pas valide.")
    }

    url_base <-
      "https://hubeau.eaufrance.fr/api/v1/hydrobio/stations_hydrobio?"

    if (!is.null(emprise)) {
      data <- httr::GET(url_base,
                        query = list(
                          bbox = paste0(bbox_emprise, collapse = ","),
                          code_support = paste0(support, collapse = ","),
                          date_debut_prelevement=paste0(an_debut, "-01-01"),
                          date_fin_prelevement=paste0(an_fin, "-12-31"),
                          size = 10000
                        ))
    } else
    {
      data <- httr::GET(
        url_base,
        query = list(
          code_station_hydrobio = paste0(liste_stations, collapse = ","),
          date_debut_prelevement=paste0(an_debut, "-01-01"),
          date_fin_prelevement=paste0(an_fin, "-12-31"),
          code_support = paste0(support, collapse = ","),
          size = 10000
        )
      )
    }

    httr::warn_for_status(data)
    httr::stop_for_status(data)


    data <- data %>%
      httr::content(as = 'text') %>%
      jsonlite::fromJSON() %>%
      .$data

    data<-data%>%dplyr::select(-geometry)

    # # on sélectionne les stations qui ont des données sur la période d'intérêt
    # data$date_premier_prelevement <-
    #   data$date_premier_prelevement %>% as.Date()
    # data$date_dernier_prelevement <-
    #   data$date_dernier_prelevement %>% as.Date()
    #
    #
    # data <-
    #   data %>% subset(
    #     date_dernier_prelevement >= paste0(an_debut, "-01-01") %>% as.Date() &
    #       date_premier_prelevement <= paste0(an_fin, "-12-31") %>% as.Date()
    #   )


    # si on souhaite un découpage selon une emprise géographique alors on découpe le tableau de résultat selon cette emprise
    if (!is.null(emprise))
    {
      data$X <- data$longitude
      data$Y <- data$latitude
      data <- st_as_sf(data, coords = c("X", "Y"), crs = 4326)
      data <- data[emprise, ]
      data <- st_drop_geometry(data)

    }


    return(data)
  }
