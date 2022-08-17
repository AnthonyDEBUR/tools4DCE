#' import_hubeau_indices_hbio
#'
#' Importe depuis Hubeau les indices hydrobiologiques d'une sélection de station sur une période donnée
#' https://hubeau.eaufrance.fr/page/api-hydrobiologie
#' Les champs sont remis en forme pour être directement compatibles avec les paramétrages par défaut des fonctions graphiques du package.
#'
#' En raison d'un bug sur l'API hub-eau (version bêta du 09/02/2022), les options an_debut an_fin ne sont pas fonctionnelles.
#'
#' @param liste_stations vecteur de character listant les codes sandre des stations à exporter. Longueur maximum de ce vecteur : 100 stations (restrictions inhérentes à l'API)
#' @param an_debut début de la période d'export (par défaut 1900)
#' @param an_fin fin de la période d'export (par défaut l'année en court)
#' @param indice vecteur avec la liste des indices à recherche (dia = diatomées, mphy = macrophytes, inv = macroinvertébrés, poi = poisson, oli = oligochètes). Par défaut la totalité des indices est sélectionnée.
#' @param alarme Comportement à avoir si la requête renvoi un code erreur : "stop" : renvoi une erreur bloquante "warning" : envoi un simple message d'alerte
#'
#' @return la fonction renvoie un tibble avec la liste des stations de mesures concernées
#' @examples import_hubeau_indices_hbio(liste_stations=c("03174000", "04216000"))
#' @export
import_hubeau_indices_hbio <-
  function(liste_stations = NULL,
           an_debut = 1900,
           an_fin = format(Sys.Date(), "%Y") %>% as.numeric(),
           indice = c("dia", "mphy", "inv", "poi", "oli"),
           alarme = "stop") {
    if (!(alarme %in% c("stop", "warning"))) {
      stop("Le paramètre alarme doit valoir soit 'stop', soit 'warning'.")
    }

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
    }

    if (!(class(indice) %in% c("character", "factor"))) {
      stop("La liste des indices à rechercher doit être de classe character ou factor")
    }

    support <-
      data.frame(
        ind = c("dia", "mphy", "inv", "poi", "oli"),
        codes_supports = c(10, 27, 13, 4, 29)
      )
    support <- support[support$ind %in% indice, ]$codes_supports

    if (length(support) == 0) {
      stop("La liste des supports fournie n'est pas valide.")
    }

    url_base <-
      "https://hubeau.eaufrance.fr/api/v1/hydrobio/indices?"


    data <- httr::GET(
      url_base,
      query = list(
        code_station_hydrobio = paste0(liste_stations, collapse = ","),
        code_support = paste0(support, collapse = ","),
        size = 10000,
        date_debut_prelevement=paste0(an_debut, "-01-01"),
        date_fin_prelevement=paste0(an_fin, "-12-31")
      )
    )


    httr::warn_for_status(data)
    httr::stop_for_status(data)

    statut <- data$status_code
    if (statut != 200) {
      if (alarme == "stop") {
        if (statut == 206) {
          stop(
            "Il y a plus de 10000 résultats correspondant à la requête. Merci de préciser les critères."
          )
        }
        else {
          stop(
            paste0(
              "Erreur dans la requête Hub-eau indices hydrobiologiques https://hubeau.eaufrance.fr/page/api-hydrobiologie#/hydrobio/indices - code erreur =",
              statut
            )
          )
        }
      }
      if (alarme == "warning") {
        if (statut == 206) {
          warning(
            "Il y a plus de 10000 résultats correspondant à la requête. Seuls les 10000 premiers sont fournis."
          )
        }
        else {
          stop(
            paste0(
              "Erreur dans la requête Hub-eau indices hydrobiologiques https://hubeau.eaufrance.fr/page/api-hydrobiologie#/hydrobio/indices - code erreur =",
              statut
            )
          )
        }
      }
    }


    data <- data %>%
      httr::content(as = 'text') %>%
      jsonlite::fromJSON() %>%
      .$data


# mise en forme des données pour respecter le modèle issu de Naïades (et être directement compatible avec les fonctions graphiques du package)

    data <-
      rename(
        data,
        CdParametre = code_indice,
        NomParametre = libelle_indice,
        CdStationMesureEauxSurface = code_station_hydrobio,
        RsAna = resultat_indice,
        CdUniteMesure = unite_indice,
        CdSupport = code_support,
        CdQualAna = code_qualification,
        CdMethode = code_methode,
        CdPrelevement=code_prelevement
      )

    data$DatePrel <- as.Date(data$date_prelevement %>% substr(1, 10))
    data$HeurePrel <- data$date_prelevement %>% substr(12, 19)

    data$CdUniteMesure<-"X"

    data$CdAccreAna <-
      ifelse(
        data$libelle_accreditation == "Analyse réalisée sous accréditation",
        "1",
        ifelse(
          data$libelle_accreditation == "Analyse réalisée hors accréditation",
          "2",
          "0"
        )
      )


    return(data)
  }
