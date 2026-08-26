#' graphDCE_points
#'
#' Affiche les graphiques des paramètres physico-chimiques en couleurs (mode point),
#' avec fond coloré selon les classes de qualité DCE.
#'
#' @param data         Tableau de données (data.frame ou tibble).
#' @param col_dates    Nom de la colonne contenant les dates de prélèvements
#'                     (formats Date ou POSIXct). Par défaut : \code{"DatePrel"}.
#' @param col_valeurs  Nom de la colonne contenant les valeurs d'analyses.
#'                     Par défaut : \code{"RsAna"}.
#' @param col_LQ       Nom de la colonne contenant la limite de quantification.
#'                     Par défaut : \code{"LqAna"}.
#' @param seuils       Objet de classe \code{seuil} (facultatif).
#' @param affiche_legende  Booléen : afficher la légende ? (par défaut \code{TRUE}).
#' @param nom_legende  Titre de la légende. Si absent et si \code{seuils} est renseigné,
#'                     le slot \code{nom_seuil} est utilisé par défaut.
#' @param titre        Titre du graphique. Si absent et si \code{seuils} est renseigné,
#'                     le slot \code{nom_parametre} est utilisé par défaut.
#' @param taille_titre Taille de la police du titre (par défaut : 12).
#' @param sous_titre   Sous-titre du graphique.
#' @param taille_sous_titre Taille de la police du sous-titre (par défaut : 11).
#' @param unite        Unité du paramètre. Par défaut, si \code{seuils} est renseigné :
#'                     le symbole SANDRE correspondant au slot \code{code_unite}.
#' @param bilan_annuel Booléen : cadrer le graphique sur des années civiles complètes ?
#'                     (par défaut \code{FALSE}).
#' @param xmini        Année minimale de l'axe X (format \code{"2010"}). Calculée
#'                     automatiquement si absente.
#' @param xmaxi        Année maximale de l'axe X (format \code{"2024"}). Calculée
#'                     automatiquement si absente.
#' @param ymini        Borne inférieure forcée de l'axe Y (numérique).
#' @param ymaxi        Borne supérieure forcée de l'axe Y (numérique).
#' @param auto_ymaxi   Booléen : si \code{TRUE} et que \code{ymaxi} n'est pas renseigné,
#'                     calcule automatiquement un \code{ymaxi} lisible (par défaut \code{TRUE}).
#' @param format_date  Format des étiquettes de dates (ex. \code{"\%Y"}, \code{"\%b\%Y"}).
#'                     Si absent, format adapté automatiquement à la durée de la chronique.
#' @param lignes       Vecteur de valeurs pour ajouter des lignes horizontales
#'                     (ex. \code{c(10, 25)}).
#' @param echelleLog   Booléen : afficher en échelle logarithmique ? (par défaut \code{FALSE}).
#' @param alpha_points Transparence des points (entre 0 et 1 ; par défaut : 1).
#' @param taille_points Taille des points (par défaut : 2).
#' @param taille_legende Taille de la police de la légende (par défaut : 12).
#' @param taille_axes  Taille de la police des axes (par défaut : 11).
#' @param liaison      Booléen : tracer des lignes pointillées entre les points ?
#'                     (par défaut \code{TRUE}).
#' @param affiche_LQ   Booléen : afficher une zone grisée pour la LQ ?
#'                     (par défaut \code{FALSE}).
#' @param separ_stations Nom de la colonne utilisée pour distinguer les stations par
#'                     des formes de points différentes (ex. \code{"CdStationMesureEauxSurface"}).
#' @param alpha        Transparence des aplats de couleurs de fond (par défaut : 0.8).
#' @param coef_axes_date Coefficient pour le nombre de graduations de l'axe des dates
#'                     (par défaut : 1 ; une valeur de 2 double le nombre de repères).
#'
#' @return Un objet \code{ggplot}.
#'
#' @examples
#' data <- data.frame(
#'   DatePrel = as.Date(c("2020-01-01", "2020-05-03", "2020-10-25", "2021-03-25")),
#'   RsAna    = c(12, 15.5, 67, 18.3)
#' )
#' graphDCE_points(data, seuils = makeSeuils(CdParametre = "1340", type_seuil = "DCE"),
#'                 taille_points = 5)
#' graphDCE_points(data, seuils = makeSeuils(CdParametre = "1340", type_seuil = "DCE"),
#'                 ymaxi = 51)
#'
#' @export
graphDCE_points <- function(
    data,
    col_dates        = "DatePrel",
    col_valeurs      = "RsAna",
    col_LQ           = "LqAna",
    seuils           = NULL,
    affiche_legende  = TRUE,
    nom_legende      = NULL,
    titre            = NULL,
    taille_titre     = 12,
    sous_titre       = NULL,
    taille_sous_titre = 11,
    unite            = NULL,
    bilan_annuel     = FALSE,
    xmini            = NULL,
    xmaxi            = NULL,
    ymini            = NULL,
    ymaxi            = NULL,
    auto_ymaxi       = TRUE,
    format_date      = NULL,
    lignes           = NULL,
    echelleLog       = FALSE,
    alpha_points     = 1,
    taille_points    = 2,
    taille_legende   = 12,
    taille_axes      = 11,
    liaison          = TRUE,
    affiche_LQ       = FALSE,
    separ_stations   = NULL,
    alpha            = 0.8,
    coef_axes_date   = 1
) {

  # ── 0. Validation des paramètres ───────────────────────────────────────────
  if (!is.numeric(coef_axes_date) || coef_axes_date <= 0)
    stop("coef_axes_date doit être un nombre strictement positif.")
  if (!is.numeric(taille_points) || taille_points <= 0)
    stop("taille_points doit être un nombre strictement positif.")
  if (!is.null(ymaxi) && !is.null(ymini) && ymaxi <= ymini)
    stop("ymaxi doit être strictement supérieur à ymini.")
  if (!is.null(xmaxi) && !is.null(xmini) && !bilan_annuel && xmaxi <= xmini)
    stop("xmaxi doit être strictement supérieur à xmini.")

  # ── 1. Préparation du tableau de données ───────────────────────────────────
  data1        <- data.frame(data)
  seuils1      <- if (!is.null(seuils)) seuils[[1]]@seuils else NULL

  # Données vides → graphique "Pas de données"
  donnees_vides <- nrow(data1) == 0 || all(is.na(data1[[col_valeurs]]))
  if (donnees_vides) {
    titre_vide <- titre %||% if (!is.null(seuils)) seuils[[1]]@nom_parametre else ""
    graph_vide <- ggplot() +
      annotate("text", label = "PAS DE DONNEES\nA AFFICHER", x = 1, y = 1)
    if (!is.null(seuils))
      graph_vide <- graph_vide + ggtitle(titre_vide) +
        theme(axis.title = element_blank(),
              axis.text  = element_blank(),
              axis.ticks = element_blank())
    return(graph_vide)
  }

  # Renommage des colonnes de travail
  data1$DatePrel <- as.POSIXct(data1[[col_dates]])
  data1$RsAna    <- data1[[col_valeurs]]
  if (affiche_LQ)
    data1$LqAna  <- data[[col_LQ]]

  # Sauvegarde de la valeur originale AVANT tout écrêtage (correction du bug)
  data1$RsAna_original <- data1$RsAna

  # Timezone homogène pour éviter les décalages avec scale_x_datetime
  attr(data1$DatePrel, "tzone") <- "Europe/Paris"

  # ── 2. Calcul automatique de ymaxi si demandé ──────────────────────────────
  if (is.null(ymaxi) && isTRUE(auto_ymaxi))
    ymaxi <- tools4DCE::calcule_ymaxi(data1$RsAna)

  # ── 3. Paramétrage de l'axe temporel (xmini / xmaxi) ──────────────────────
  .as_posix_paris <- function(date_str, suffix) {
    as.POSIXct(
      strptime(paste0(date_str, suffix), "%Y-%m-%d %H:%M:%S"),
      tz = "Europe/Paris"
    )
  }

  if (bilan_annuel) {
    xmini <- if (is.null(xmini)) {
      .as_posix_paris(format(min(data1$DatePrel, na.rm = TRUE) - 1, "%Y"), "-01-01 00:00:00")
    } else {
      .as_posix_paris(xmini, "-01-01 00:00:00")
    }
    xmaxi <- if (is.null(xmaxi)) {
      .as_posix_paris(format(max(data1$DatePrel, na.rm = TRUE) + 1, "%Y"), "-12-31 23:59:59")
    } else {
      .as_posix_paris(xmaxi, "-12-31 23:59:59")
    }
    data1 <- subset(data1, DatePrel >= xmini & DatePrel <= xmaxi)
  } else {
    xmini <- if (is.null(xmini)) {
      min(data1$DatePrel, na.rm = TRUE)
    } else {
      .as_posix_paris(xmini, "-01-01 00:00:00")
    }
    xmaxi <- if (is.null(xmaxi)) {
      max(data1$DatePrel, na.rm = TRUE)
    } else {
      .as_posix_paris(xmaxi, "-12-31 23:59:59")
    }
    data1 <- subset(data1, DatePrel >= xmini & DatePrel <= xmaxi)

    # Marge de 5 % pour que les points extrêmes ne soient pas tronqués
    marge <- abs(diff(range(data1$DatePrel))) * 0.05
    xmini <- xmini - marge
    xmaxi <- xmaxi + marge

    # Cas d'un seul point : marge de ±1 jour
    if (xmini == xmaxi) {
      xmini <- xmini - 3600 * 24
      xmaxi <- xmaxi + 3600 * 24
    }
  }
  attr(xmini, "tzone") <- "Europe/Paris"

  # ── 4. Format et pas des étiquettes de l'axe des dates ────────────────────
  duree_jours <- as.numeric(xmaxi - xmini)

  .date_params <- function(duree, coef, format_date) {
    fmt <- function(defaut) if (!is.null(format_date)) format_date else defaut
    if      (duree <= 1)              list(breaks = paste0(ceiling(60  * coef), " minutes"), fmt = fmt("%d%b%y %Hh"))
    else if (duree <= 15)             list(breaks = paste0(ceiling(24  * coef), " hours"),   fmt = fmt("%d%b%y"))
    else if (duree <= 31)             list(breaks = paste0(ceiling(48  * coef), " hours"),   fmt = fmt("%d%b%y"))
    else if (duree <= 365)            list(breaks = if (coef >= 1) paste0(ceiling(coef), " months") else paste0(ceiling(4 * coef), " weeks"), fmt = fmt("%d%b%y"))
    else if (duree <= 2 * 365)        list(breaks = paste0(ceiling(3   * coef), " months"),  fmt = fmt("%d%b%y"))
    else if (duree %/% 365 <= 3)      list(breaks = paste0(ceiling(6   * coef), " months"),  fmt = fmt("%d%b%y"))
    else if (duree %/% 365 <= 14)     list(breaks = if (coef >= 1) paste0(ceiling(coef), " year") else paste0(ceiling(12 * coef), " months"), fmt = fmt("%b%Y"))
    else if (duree %/% 365 <= 29)     list(breaks = paste0(ceiling(5   * coef), " years"),   fmt = fmt("%b%Y"))
    else                              list(breaks = paste0(ceiling(10  * coef), " years"),   fmt = fmt("%b%Y"))
  }

  dp         <- .date_params(duree_jours, coef_axes_date, format_date)
  dateformat <- dp$fmt
  break_date_max <- dp$breaks
  break_date_min <- waiver()

  # ── 5. Couleur des points selon la classe de qualité ──────────────────────
  if (!is.null(seuils)) {
    data1 <- data1 %>%
      mutate(classe_pt = affecte_une_classe(RsAna, seuil = seuils[[1]])) %>%
      left_join(
        seuils[[1]]@seuils %>% dplyr::select(CLASSE, NOM_COULEUR),
        by = c("classe_pt" = "CLASSE")
      ) %>%
      dplyr::rename(couleur_pt = NOM_COULEUR) %>%
      mutate(couleur_pt = replace_na(couleur_pt, "white"))
  } else {
    data1$couleur_pt <- "white"
  }

  # ── 6. Construction de seuils1minmax (bornes de l'axe Y + graduation) ─────
  seuils1minmax <- as.numeric(unique(c(ymini, seuils1$SEUILMIN, seuils1$SEUILMAX, ymaxi)))

  # Nombre de décimales pour l'arrondi des bornes auto
  nb_decim <- if (!is.null(seuils1)) {
    max(sapply(seuils1$SEUILMIN, compte_decimales), na.rm = TRUE)
  } else {
    max(sapply(data1$RsAna, compte_decimales), na.rm = TRUE)
  }

  # Range des données (avec jitter si toutes les valeurs sont identiques)
  rangedata <- abs(diff(range(data1$RsAna, na.rm = TRUE)))
  if (rangedata == 0 && !is.null(seuils1))
    rangedata <- abs(diff(range(jitter(data1$RsAna), na.rm = TRUE)))

  prec      <- 10^(-nb_decim)
  min_data  <- round_any(min(data1$RsAna, na.rm = TRUE) - 0.1 * rangedata, prec, f = floor)
  max_data  <- round_any(max(data1$RsAna, na.rm = TRUE) + 0.1 * rangedata, prec, f = ceiling)

  # Si toutes les valeurs sont positives, on force min_data >= 0
  if (all(sign(data1$RsAna) == 1, na.rm = TRUE) && min_data < 0)
    min_data <- 0

  # Intégration des bornes auto dans seuils1minmax
  if (is.null(ymini))
    seuils1minmax <- c(min_data, seuils1minmax[seuils1minmax >= min_data])
  if (is.null(ymaxi))
    seuils1minmax <- c(seuils1minmax[seuils1minmax <= max_data], max_data)

  # Filtrage selon ymini / ymaxi explicites
  if (!is.null(ymini)) seuils1minmax <- seuils1minmax[seuils1minmax >= ymini]
  if (!is.null(ymaxi)) seuils1minmax <- seuils1minmax[seuils1minmax <= ymaxi]

  # En échelle log : on élimine les valeurs <= 0
  if (echelleLog)
    seuils1minmax <- seuils1minmax[seuils1minmax > 0]

  seuils1minmax <- sort(unique(seuils1minmax))

  y_min_eff <- min(seuils1minmax[is.finite(seuils1minmax)])
  y_max_eff <- max(seuils1minmax[is.finite(seuils1minmax)])

  # ── 7. Mise en forme du tableau de seuils pour les rectangles de fond ─────
  if (!is.null(seuils1)) {
    seuils1$NOM_COULEUR <- as.character(seuils1$NOM_COULEUR)

    # Recadrage des bornes sur l'étendue effective du graphique
    seuils1 <- seuils1 %>%
      mutate(
        SEUILMIN = pmax(SEUILMIN, y_min_eff),
        SEUILMAX = pmin(SEUILMAX, y_max_eff)
      ) %>%
      filter(SEUILMIN != SEUILMAX)  # suppression des classes de largeur nulle

    # Vecteur nommé couleurs → utilisé dans scale_fill_manual
    couleurs_vec <- setNames(seuils1$NOM_COULEUR, seuils1$CLASSE)

    seuils1$xmini <- xmini
    seuils1$xmaxi <- xmaxi
    attr(seuils1$xmini, "tzone") <- "Europe/Paris"
    attr(seuils1$xmaxi, "tzone") <- "Europe/Paris"
  }

  # ── 8. Détection et écrêtage des valeurs hors plage ───────────────────────
  # On compare RsAna_original pour ne pas être affecté par d'éventuelles
  # modifications antérieures. Les étiquettes afficheront également RsAna_original.
  data1 <- data1 %>%
    mutate(
      depassementSUP = if_else(RsAna_original > y_max_eff, y_max_eff, NA_real_),
      depassementINF = if_else(RsAna_original < y_min_eff, y_min_eff, NA_real_),
      # Écrêtage de RsAna pour le positionnement graphique
      RsAna = case_when(
        !is.na(depassementSUP) ~ y_max_eff,
        !is.na(depassementINF) ~ y_min_eff,
        TRUE                   ~ RsAna
      )
    )

  depassSUP <- if (any(!is.na(data1$depassementSUP))) filter(data1, !is.na(depassementSUP)) else NULL
  depassINF <- if (any(!is.na(data1$depassementINF))) filter(data1, !is.na(depassementINF)) else NULL

  # ── 9. Métadonnées (légende, unité, titre) ─────────────────────────────────
  nom_legende <- nom_legende %||% (if (!is.null(seuils)) seuils[[1]]@nom_seuil else "")

  unite <- unite %||% if (!is.null(seuils)) {
    tools4DCE::unites_sandre[
      tools4DCE::unites_sandre$CdUniteMesure == seuils[[1]]@code_unite,
    ]$SymUniteMesure[1]
  } else ""

  titre <- titre %||% if (!is.null(seuils)) seuils[[1]]@nom_parametre else NULL

  # ── 10. Construction du graphique ggplot ───────────────────────────────────
  graph1 <- ggplot()

  # Rectangles de fond colorés selon les classes de qualité
  if (!is.null(seuils1) && nrow(seuils1) > 0) {
    graph1 <- graph1 +
      geom_rect(
        data = seuils1,
        aes(xmin = xmini, xmax = xmaxi, ymin = SEUILMIN, ymax = SEUILMAX, fill = CLASSE),
        alpha = alpha,
        show.legend = TRUE
      ) +
      scale_fill_manual(name = nom_legende, drop = FALSE, values = couleurs_vec)
  }

  # Échelle de l'axe Y
  scale_y_fn <- if (echelleLog) scale_y_log10 else scale_y_continuous
  graph1 <- graph1 +
    scale_y_fn(
      breaks = seuils1minmax,
      expand = c(0, 0),
      limits = c(y_min_eff, y_max_eff)
    )

  # Échelle de l'axe X (dates)
  graph1 <- graph1 +
    scale_x_datetime(
      labels            = date_format(dateformat, tz = "Europe/Paris"),
      date_breaks       = break_date_max,
      date_minor_breaks = break_date_min,
      limits            = c(xmini, xmaxi),
      expand            = c(0, 0)
    )

  # Lignes pointillées entre les points
  if (liaison) {
    if (is.null(separ_stations)) {
      graph1 <- graph1 +
        geom_line(data = data1, aes(x = DatePrel, y = RsAna), linetype = "dashed")
    } else {
      graph1 <- graph1 +
        geom_line(data = data1,
                  aes(x = DatePrel, y = RsAna, linetype = .data[[separ_stations]]))
    }
  }

  # Zone LQ grisée
  if (affiche_LQ) {
    if (any(is.na(data1$LqAna)))
      warning("Certaines analyses n'ont pas de valeur de LQ : LQ manquantes remplacées par 0.")
    data1$LqAna <- replace_na(data1$LqAna, 0)
    graph1 <- graph1 +
      geom_ribbon(
        data     = data1,
        aes(x = DatePrel, ymin = y_min_eff, ymax = LqAna),
        fill     = "grey30",
        stat     = "identity",
        position = "identity",
        show.legend = FALSE
      )
  }

  # Points
  if (is.null(separ_stations)) {
    graph1 <- graph1 +
      geom_point(data = data1, aes(x = DatePrel, y = RsAna),
                 size = taille_points, alpha = alpha_points)
  } else {
    graph1 <- graph1 +
      geom_point(data = data1,
                 aes(x = DatePrel, y = RsAna, shape = .data[[separ_stations]]),
                 size = taille_points, alpha = alpha_points)
  }

  # Titre et sous-titre
  if (!is.null(titre))
    graph1 <- graph1 + ggtitle(titre)
  if (!is.null(sous_titre))
    graph1 <- graph1 + labs(subtitle = sous_titre)

  graph1 <- graph1 + xlab("") + ylab(unite)

  # Lignes horizontales libres
  if (length(lignes) > 0)
    graph1 <- graph1 + geom_hline(yintercept = lignes, linetype = "dashed")

  # Étiquettes des valeurs hors plage
  # label = RsAna_original : affiche la VRAIE valeur mesurée, pas la valeur écrêtée
  if (!is.null(depassSUP)) {
    graph1 <- graph1 +
      geom_label(
        data  = depassSUP,
        aes(x = DatePrel, y = y_max_eff, label = RsAna_original),
        fill  = depassSUP$couleur_pt,
        size  = 2.7,
        vjust = "top"
      )
  }
  if (!is.null(depassINF)) {
    graph1 <- graph1 +
      geom_label(
        data  = depassINF,
        aes(x = DatePrel, y = y_min_eff, label = RsAna_original),
        fill  = depassINF$couleur_pt,
        size  = 2.7,
        vjust = "bottom"
      )
  }

  # Thème général
  graph1 <- graph1 + theme_light() +
    theme(
      legend.position  = ifelse(affiche_legende, "right", "none"),
      legend.title     = element_text(colour = "black", size = taille_legende),
      axis.line        = element_line(colour = "black", linewidth = 1),
      panel.grid.major = element_line(colour = "black"),
      panel.grid.minor = element_blank(),
      panel.spacing    = unit(2, "lines"),
      axis.text.x      = element_text(angle = 90, vjust = 0.5, size = taille_axes),
      axis.text.y      = element_text(size = taille_axes),
      axis.title       = element_text(size = taille_axes),
      plot.title       = element_text(size = taille_titre),
      plot.subtitle    = element_text(size = taille_sous_titre)
    )

  graph1
}
