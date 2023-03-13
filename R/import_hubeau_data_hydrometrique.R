#' import_hubeau_data_hydrometrique
#'
#' Importe depuis Hubeau les débits journaliers moyens d'une station
#' https://hubeau.eaufrance.fr/page/api-hydrometrie#/hydrometrie/observationsElaborees
#'
#'
#' @param code_entite character ou vecteur avec le code de la ou des stations hydrométriques dont on souhaite récupérer les données (obligatoire)
#' @param date_debut_obs_elab date de début des observations à récupérer (optionnel)
#' @param date_fin_obs_elab début de fin des observations à récupérer (optionnel)
#'
#' @return la fonction renvoie un tibble avec le code de la station hydrométrique, la date et le débit journalier moyen en L/s
#' @examples Vilaine<-charge_shp_SAGE(nom_sage="Vilaine")
#' @examples import_hubeau_data_hydrometrique(code_entite="J930061101", date_debut_obs_elab=as.Date("2000-01-01"))
#' @export
import_hubeau_data_hydrometrique <-
  function(code_entite,
           date_debut_obs_elab = NULL,
           date_fin_obs_elab = NULL) {
    if (class(code_entite) != "character") {
      stop("code_entite doit être un objet de classe character")
    }
    if (!is.null(date_debut_obs_elab)) {
      if (class(date_debut_obs_elab) != "Date") {
        stop("date_debut_obs_elab doit être un objet de classe date")
      }
    } else {
      date_debut_obs_elab <- as.Date("1900-01-01")
    }


    if (!is.null(date_fin_obs_elab)) {
      if (class(date_fin_obs_elab) != "Date") {
        stop("date_fin_obs_elab doit être un objet de classe date")
      }
    } else {
      date_fin_obs_elab <- as.Date(Sys.Date())
    }



    url_base <-
      "https://hubeau.eaufrance.fr/api/v1/hydrometrie/obs_elab?"

    data <- httr::GET(
      url_base,
      query = list(
        code_entite = paste0(code_entite, collapse = ","),
        date_debut_obs_elab = date_debut_obs_elab,
        date_fin_obs_elab = date_fin_obs_elab,
        size = 5000,
        grandeur_hydro_elab="QmJ",
        fields = "code_station,date_obs_elab,resultat_obs_elab"
      )
    )


    httr::warn_for_status(data)
    httr::stop_for_status(data)

    # si la requête renvoie l'ensemble des données en 1 fois
    if (data$status_code == 200)
    {
      data0 <- data %>%
        httr::content(as = 'text', encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        .$data
    }

    # si la requête nécessite plusieurs pages de résultats alors on parcours les pages de résultat
    if (data$status_code == 206)
    {
      data0 <- data %>%
        httr::content(as = 'text', encoding = "UTF-8") %>%
        jsonlite::fromJSON() %>%
        .$data

      while (data$status_code == 206)
      {
        # on recupere url page suivante
        url <- data$headers$link %>% str_split("<")
        url <- lapply(url, function(x)
          str_split(x, ">"))
        url <- unlist(url)
        url <- url[grep("; rel=\"next\"", url) - 1]

        data <- httr::GET(url)

        data1 <- data %>%
          httr::content(as = 'text', encoding = "UTF-8") %>%
          jsonlite::fromJSON() %>%
          .$data

        if (!is.null(nrow(data1)))
        {
          data0 <- rbind(data0, data1)
        }
      }


    }

    if (data$status_code == 400){stop("requête incorrecte")}


    return(data0)
  }
