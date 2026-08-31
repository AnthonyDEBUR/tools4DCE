#' P90glissant
#'
#' fonction pour calculer le percentile 90 glissant sur un nombre de mois donnés
#'
#' @param analyses = dataframe de données
#' @param col_date = nom de la colonne avec la date de prélèvement, par défaut Dateprel
#' @param col_rsana = nom de la colonne avec les résultats d'analyses, par défaut RsAna
#' @param nb_mois = nombre de mois sur lesquels faire le calcul
#'
#' @return la fonction renvoie un dataframe avec une colonne date avec les mois et une colonne P90 avec
#' le percentile DCE 90 sur la période entre la date et la date - nb_mois
#'
#' @examples
#' set.seed(123)
#'
#' df <- data.frame(
#'   DatePrel = sample(
#'     seq(
#'       as.Date("2015-01-01"),
#'       as.Date("2023-12-31"),
#'       by = "day"
#'     ),
#'     500
#'   ),
#'   RsAna = runif(500, 0, 100)
#' ) |>
#'   dplyr::arrange(DatePrel)
#'
#' P90glissant(df, nb_mois = 36)
#' @export
P90glissant <- function(analyses,
                        col_date="DatePrel",
                        col_rsana="RsAna",
                        nb_mois) {
  # 1. Regrouper les données par mois
  df_monthly <- df %>%
    filter(!is.na(DatePrel)) %>%                        # Supprimer les lignes sans date
    mutate(moisannee = floor_date(DatePrel, "month")) %>%
    group_by(moisannee) %>%
    dplyr::summarise(Values = list(RsAna), .groups = "drop")  # Liste de toutes les valeurs du mois


  # 2. Compléter les mois manquants
  if (nrow(df_monthly) > 0) {  # Vérifiez qu'il y a des données avant de créer la séquence
    full_months <- data.frame(moisannee = seq(
      from = min(df_monthly$moisannee, na.rm = TRUE),
      to = max(df_monthly$moisannee, na.rm = TRUE),
      by = "month"
    ))

    df_monthly <- full_months %>%
      left_join(df_monthly, by = "moisannee") %>%
      mutate(Values = ifelse(is.na(Values), list(numeric(0)), Values))  # Remplir les mois sans données
  } else {
    stop("Aucune donnée valide dans df_monthly après regroupement.")
  }

  # 3. Créer une fenêtre glissante de 36 mois
  # On veut une fenêtre glissante basée sur les mois et non les valeurs
  window_size <- 36

  # Calcul du percentile glissant sur 36 mois
  percentiles <- sapply(1:(nrow(df_monthly) - window_size + 1), function(i) {
    # Sélectionner les valeurs pour les 36 mois à partir de la position i
    window_values <- unlist(df_monthly$Values[i:(i + window_size - 1)])
    PercentileDCE(window_values)
  })

  # Ajouter le résultat à df_monthly en alignant les indices
  df_monthly$percentile_glissant <- c(rep(NA, window_size - 1), percentiles)

  # supprimer les NA
  df_monthly<-df_monthly%>%
    subset(!is.na(percentile_glissant))%>%
    select(-Values)


  df_monthly$percentile_glissant<-sapply(df_monthly$percentile_glissant, function(x) if (is.null(x)) NA else x)
  df_monthly$moisannee<-as.POSIXct(df_monthly$moisannee)

  return(df_monthly)
}
