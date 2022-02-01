#' graphDCE_points
#'
#' fonction pour afficher les graphiques des paramètres en couleurs (mode point)
#'
#' @param data tableau de données (dataframe)
#' @param col_dates nom de la colonne qui contient les dates de prélèvements. Par défaut : "DatePrel" formats Date ou POSIXct
#' @param col_valeurs nom de la colonne qui contient les valeurs d'analyses.Par défaut : "RsAna"
#' @param col_LQ nom de la colonne qui contient la valeur de LqAna. Par défaut : "LqAna"
#' @param seuils objet de classe seuil (factultatif)
#' @param nom_legende titre de la légende (si absent et si seuils et renseigné, la légende par défaut sera le slot nom_seuil de la légende, si seuil n'est pas renseigné, la légende sera "Légende")
#' @param affiche_legende booléen indiquant s'il faut afficher la légende (par défaut TRUE)
#' @param titre titre du graphique (si absent et si seuils et renseigné, la légende par défaut sera le slot nom_parametre)
#' @param taille_titre taille police du titre (par défaut 12)
#' @param sous_titre sous titre du graphique
#' @param taille_sous_titre taille police du sous titre (par défaut 10)
#' @param unite unité du paramètre (par défaut si seuils est renseigné : le label SANDRE correspondant au slot code_unite du paramètre seuils)
#' @param xmini année mini (si non précisé, sera calculé automatiquement comme étant le 1er janvier de l'année correspondant au minimum des dates du jeu de données). Format : "2010"
#' @param xmaxi année maxi (si non précisé, sera calculé automatiquement comme étant le 31 décembre de l'année correspondant au maximum des dates du jeu de données). Format : "2010"
#' @param ymini force l'échelle des valeurs (si non précisé, échelle automatique de ggplot2)
#' @param ymaxi force l'échelle des valeurs (si non précisé, échelle automatique de ggplot2)
#' @param auto_ymaxi booléen. Si le paramètre vaut TRUE et que ymaxi n'est pas renseigné alors ce dernier est calculé automatiquement de manière à rendre le graph aussi lisible que possible
#' @param bilan_annuel si le paramètre vaut TRUE : on ajuste le graphique sur les années civiles. S'il vaut FALSE (valeur par défaut) : on laisse l'échelle graphique libre
#' @param lignes vecteur permettant d'ajouter des lignes horizontales au graphique. ex c(10, 25)
#' @param echelleLog booléen : si vrai le graphique est affiché en échelle logarithmique (FALSE par défaut)
#' @param taille_legende taille police caractères de la légende
#' @param taille_axes taille police de caractères des axes
#' @param liaison option faisant afficher ou pas des lignes pointillées entre les points (défaut = TRUE)
#' @param affiche_LQ option qui affiche une zone grisée correspondant à la LqAna (par défaut non affiché)
#' @param separ_stations nom de la colonne par rapport à laquelle séparer les données avec différents shape (par exemple pour distinguer les résultats de plusieurs stations de mesures)
#' @param alpha transparence des applats de couleurs
#'
#'
#' @return la fonction renvoie un graphique de classe ggplot
#'
#' @examples data<-data.frame(DatePrel=c("2020-01-01", "2020-05-03","2020-10-25", "2021-03-25")%>%as.Date, RsAna=c(12,15.5,67,18.3))
#' @examples graphDCE_points(data, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"))
#' @examples graphDCE_points(data, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), ymaxi=51)
#'
#' @export
#'
graphDCE_points <-
  function(data,
           col_dates = "DatePrel",
           col_valeurs = "RsAna",
           col_LQ = "LqAna",
           seuils = NULL,
           affiche_legende = T,
           nom_legende = NULL,
           titre = NULL,
           taille_titre = 12,
           sous_titre = NULL,
           taille_sous_titre = 11,
           unite = NULL,
           bilan_annuel = FALSE,
           xmini = NULL,
           xmaxi = NULL,
           ymini = NULL,
           ymaxi = NULL,
           auto_ymaxi = TRUE,
           lignes = NULL,
           echelleLog = FALSE,
           taille_legende = 12,
           taille_axes = 11,
           liaison = T,
           affiche_LQ = F,
           separ_stations = NULL,
           alpha = 0.8)
  {
    #browser()
    data1 <- data.frame(data)
    if (!is.null(seuils)) {
      seuils1 <- seuils[[1]]@seuils
    } else{
      seuils1 <- NULL
    }

    if (!is.null(ymaxi) & !is.null(ymini)) {
      if (ymaxi <= ymini) {
        stop("ymaxi doit être strictement supérieur à ymini")
      }
    }
    if (!is.null(xmaxi) & !is.null(xmini)) {
      if (xmaxi <= xmini) {
        stop("xmaxi doit être strictement supérieur à xmini")
      }
    }


    if ((nrow(data1) > 0) &
        (!all(is.na(data1[[col_valeurs]]))))
      # on ne traite les données que si le tableau de données n'est pas vide
    {
      data1$DatePrel <- data1[[col_dates]] %>% as.POSIXct
      data1$RsAna <- data1[[col_valeurs]]
      if (affiche_LQ) {
        data1$LqAna <- data[[col_LQ]]
      }
      if (is.null(ymaxi) &
          (auto_ymaxi == T)) {
        ymaxi <- tools4DCE::calcule_ymaxi(data1$RsAna)
      }

      ##### on homogénéise les times zones pour l'ensemble des données (conserver GMT qui évite des bugs de décallage avec scale_x_datetime#####
      attr(data1$DatePrel, "tzone") <-
        "Europe/Paris" # "Europe/Paris"
      ##### paramétrage x mini et maxi du graph #####
      if (bilan_annuel)
      {
        # paramètrage xmini - maxi pour les échelles temporelles
        if (is.null(xmini)) {
          xmini <-
            as.POSIXct(strptime(
              paste0(format((
                min(data1$DatePrel, na.rm = T) - 1
              ), "%Y"), "-01-01 00:00:00"),
              "%Y-%m-%d %H:%M:%S"
            ), tz = "Europe/Paris")
        }
        else {
          xmini <-
            as.POSIXct(strptime(paste0(xmini, "-01-01 00:00:00"), "%Y-%m-%d %H:%M:%S"), tz =
                         "Europe/Paris")
        }
        if (is.null(xmaxi)) {
          xmaxi <-
            as.POSIXct(strptime(
              paste0(format((
                max(data1$DatePrel, na.rm = T) + 1
              ), "%Y"), "-12-31 23:59:59"),
              "%Y-%m-%d %H:%M:%S"
            ), tz = "Europe/Paris")
        }
        else {
          xmaxi <-
            as.POSIXct(strptime(paste0(xmaxi, "-12-31 23:59:59"), "%Y-%m-%d %H:%M:%S"), tz =
                         "Europe/Paris")
        }
        # on ne conserve que les données entre les dates xmini et xmaxi
        data1 <-
          subset(data1, DatePrel >= xmini & DatePrel <= xmaxi)
      }
      else
      {
        # paramètrage xmini - maxi pour les échelles temporelles
        if (is.null(xmini)) {
          xmini <- min(data1$DatePrel, na.rm = T)
        }
        else {
          xmini <-
            as.POSIXct(strptime(paste0(xmini, "-01-01 00:00:00"), "%Y-%m-%d %H:%M:%S"), tz =
                         "Europe/Paris")
        }
        if (is.null(xmaxi)) {
          xmaxi <- max(data1$DatePrel, na.rm = T)
        }
        else {
          xmaxi <-
            as.POSIXct(strptime(paste0(xmaxi, "-12-31 23:59:59"), "%Y-%m-%d %H:%M:%S"), tz =
                         "Europe/Paris")
        }
        # on ne conserve que les données entre les dates xmini et xmaxi
        data1 <-
          subset(data1, DatePrel >= xmini & DatePrel <= xmaxi)
        # on élargi les échelles min et max pour que les premiers points soient totalement dans la zone colorée
        xmini <-
          xmini - abs(range(data1$DatePrel)[2] - range(data1$DatePrel)[1]) * 0.05
        xmaxi <-
          xmaxi + abs(range(data1$DatePrel)[2] - range(data1$DatePrel)[1]) * 0.05
        # cas d'un seul point (xmini et xmaxi confondus : on élargi l'échelle de 14 jours)
        if (xmini == xmaxi) {
          xmini <- xmini - 3600 * 24
          xmaxi <- xmaxi + 3600 * 24
        }

      }
      attr(xmini, "tzone") <- "Europe/Paris"

      ##### échelle du break des dates (quelles dates sont indiquées en abscisse selon la chronique)
      duree_jours <- as.numeric(xmaxi - xmini)

      # durée <= 1 jour
      if (duree_jours <= 1) {
        break_date_max <- "1 hour"
        dateformat <- "%d%b%y %Hh"
      }
      # durée <= 15 jours
      else if (duree_jours <= 15) {
        break_date_max <- "1 day"
        dateformat <- "%d%b%y"
      }
      # durée <= 31 jours
      else if (duree_jours <= 31) {
        break_date_max <- "2 days"
        dateformat <- "%d%b%y"
      }
      # durée <= 1 an
      else if (duree_jours <= 365) {
        break_date_max <- "1 month"
        dateformat <- "%d%b%y"
      }
      # durée <= 2 ans
      else if (duree_jours <= 2 * 365) {
        break_date_max <- "3 months"
        dateformat <- "%d%b%y"
      }
      # durée entre 2 et 3 ans
      else if (duree_jours %/% 365 <= 3) {
        break_date_max <- "6 months"
        dateformat <- "%d%b%y"
      }
      # durée entre 4 et 15 ans
      else if (duree_jours %/% 365 <= 14) {
        break_date_max <- "1 year"
        dateformat <- "%b%Y"
      }
      # durée entre 15 et 30 ans
      else if (duree_jours %/% 365 <= 29) {
        break_date_max <- "5 years"
        dateformat <- "%b%Y"
      }
      # durée >30 ans
      else {
        break_date_max <- "10 years"
        dateformat <- "%b%Y"
      }

      # échelle des breaks de dates secondaires
      break_date_min <- waiver()

      # ajout d'une colonne couleur correspondant aux seuils
      # la fonction est ajustée selon si les limites de classes inclue ou pas les bornes inférieures

      # on ajoute la couleur pour chaque seuil selon la classe de qualité
      if (!is.null(seuils)) {
        data1 <-
          data1 %>% mutate(classe_pt = affecte_une_classe(data1$RsAna, seuil = seuils[[1]]))
        data1 <-
          left_join(
            data1,
            seuils[[1]]@seuils %>% select(CLASSE, NOM_COULEUR),
            by = c("classe_pt" = "CLASSE")
          )
        names(data1)[names(data1) == "NOM_COULEUR"] <-
          "couleur_pt"
        data1$couleur_pt <- replace_na(data1$couleur_pt, "white")
      }
      else {
        data1$couleur_pt <-
          "white"
      } # par défaut les étiquettes des valeurs hors gamme sont blanches


      # calcul et ajout des bornes min_max du graph
      seuils1minmax <-
        as.numeric(unique(c(
          ymini, seuils1$SEUILMIN, seuils1$SEUILMAX, ymaxi
        )))

      # calcul du nb de décimales max dans les seuils1 (à défaut dans les données) pour choisir le nb de décimales à afficher dans légende
      if (!is.null(seuils1))
      {
        nb_decim <-
          max(sapply(seuils1$SEUILMIN, compte_decimales), na.rm = T)
      }
      else
      {
        nb_decim <- max(sapply(data1$RsAna, compte_decimales), na.rm = T)
      }
      # calcul du RANGE entre min et max des données ainsi que des bornes mini des valeurs et maxi des valeurs +/- x%
      rangedata <-
        abs(max(data1$RsAna, na.rm = T) - min(data1$RsAna, na.rm = T))
      # if(rangedata==0 & !is.null(seuils1)){rangedata<-range.default(seuils1[,c("SEUILMIN", "SEUILMAX")], na.rm=T, finite=T)[2]-range.default(seuils1[,c("SEUILMIN", "SEUILMAX")], na.rm=T, finite=T)[1]} # si toutes les valeurs de données sont égales alors on retient la valeur entre les seuils1 comme range
      if (rangedata == 0 &
          !is.null(seuils1)) {
        rangedata <-
          abs(max(jitter(data1$RsAna), na.rm = T) - min(data1$RsAna, na.rm = T))
      } # si toutes les valeurs de données sont égales alors on calcul un range autour des valeurs mesurées en ajoutant un bruit artificiel dans le calcul du range (jitter)
      # calcul des valeurs min -5% du range et max +5% du range pour avoir l'échelle affichée si les paramètres ymini ou ymaxi ne sont pas renseignés
      min_data <-
        round_any(min(data1$RsAna, na.rm = T) - 0.1 * rangedata, 10 ^ -nb_decim, f =
                    floor)
      max_data <-
        round_any(max(data1$RsAna, na.rm = T) + 0.1 * rangedata, 10 ^ -nb_decim, f =
                    ceiling)
      # dans le cas où toutes les valeurs sont positives et min_data<0 alors min_data<-0
      if ((all(sign(data1$RsAna) == 1, na.rm = T) &
           min_data < 0)) {
        min_data <- 0
      }

      # si ymini et ymaxi non définis alors on zoom le graphique autour de la plage de données disponible
      if (is.null(ymini)) {
        seuils1minmax <-
          c(min_data, seuils1minmax[seuils1minmax >= min_data])
      }
      if (is.null(ymaxi)) {
        seuils1minmax <-
          c(seuils1minmax[seuils1minmax <= max_data], max_data)
      }


      # on ne conserve que les seuils1 d'affichage entre ymini et ymaxi
      if (!is.null(ymini)) {
        seuils1minmax <- seuils1minmax[seuils1minmax >= ymini]
      }
      if (!is.null(ymaxi)) {
        seuils1minmax <- seuils1minmax[seuils1minmax <= ymaxi]
      }

      # dans le cas d'une échelle log on supprime tous les seuils1 <=0
      if (echelleLog &
          length(seuils1minmax[seuils1minmax <= 0]) > 0) {
        seuils1minmax <- seuils1minmax[seuils1minmax > 0]
      }

      # on supprime les doublons
      seuils1minmax <- unique(seuils1minmax)

      ##### mise en forme du tableau de seuils1 pour affichage des couleurs en fond de graph
      if (!is.null(seuils1)) {
        # les couleurs sont en character et non facteurs
        seuils1$NOM_COULEUR <- as.character(seuils1$NOM_COULEUR)
        # on corrige le tableau de couleurs pour l'adapter aux min-max
        if (nrow(seuils1[seuils1$SEUILMIN < min(seuils1minmax, na.rm = T), ]) >
            0) {
          seuils1[seuils1$SEUILMIN < min(seuils1minmax, na.rm = T), ]$SEUILMIN <-
            min(seuils1minmax, na.rm = T)
        }
        if (nrow(seuils1[seuils1$SEUILMAX < min(seuils1minmax, na.rm = T), ]) >
            0) {
          seuils1[seuils1$SEUILMAX < min(seuils1minmax, na.rm = T), ]$SEUILMAX <-
            min(seuils1minmax, na.rm = T)
        }
        if (nrow(seuils1[seuils1$SEUILMIN > max(seuils1minmax, na.rm = T), ]) >
            0) {
          seuils1[seuils1$SEUILMIN > max(seuils1minmax, na.rm = T), ]$SEUILMIN <-
            max(seuils1minmax, na.rm = T)
        }
        if (nrow(seuils1[seuils1$SEUILMAX > max(seuils1minmax, na.rm = T), ]) >
            0) {
          seuils1[seuils1$SEUILMAX > max(seuils1minmax, na.rm = T), ]$SEUILMAX <-
            max(seuils1minmax, na.rm = T)
        }


        # préparation de la commande sur les couleurs des rectangles
        couleurs <- ""
        for (k in 1:nrow(seuils1))
        {
          couleurs <- paste0(
            couleurs,
            '"',
            seuils1$CLASSE[k],
            '"="',
            seuils1$NOM_COULEUR[k],
            '"',
            ifelse(k != nrow(seuils1), ",", "")
          )
        }
        couleurs <- paste0("c(", couleurs, ")")
        # suppression des classes de qualité avec les seuils1 min et max égaux
        seuils1 <-
          seuils1[which(seuils1$SEUILMIN != seuils1$SEUILMAX), ]
        # ajout des xmini et maxi
        seuils1$xmini <- xmini
        seuils1$xmaxi <- xmaxi
        attr(seuils1$xmini, "tzone") <- "Europe/Paris"
        attr(seuils1$xmaxi, "tzone") <- "Europe/Paris"

      }

      # table des valeurs hors range qui seront étiquettées. Pour ces valeurs on remplace la valeur par le max (ou min) de l'échelle (pour afficher un point)
      data1$depassementSUP <-
        ifelse(data1$RsAna > max(seuils1minmax, na.rm = T),
               max(seuils1minmax[seuils1minmax < Inf], na.rm = T) ,
               NA)
      data1$depassementINF <-
        ifelse(data1$RsAna < min(seuils1minmax, na.rm = T),
               min(seuils1minmax[seuils1minmax > -Inf], na.rm = T),
               NA)
      if (any(!is.na(data1$depassementSUP))) {
        depassSUP <- subset(data1,!is.na(depassementSUP))
        data1[which(!is.na(data1$depassementSUP)), ]$RsAna <-
          max(seuils1minmax[seuils1minmax < Inf], na.rm = T)
      }
      if (any(!is.na(data1$depassementINF))) {
        depassINF <- subset(data1,!is.na(depassementINF))
        data1[which(!is.na(data1$depassementINF)), ]$RsAna <-
          min(seuils1minmax[seuils1minmax > -Inf], na.rm = T)
      }

      # ajout du nom de la légende en automatique si cette dernière n'est pas renseignée
      if (is.null(nom_legende))
      {
        # si les seuils ont été définis parmi les paramètres alors on affecte au nom de la légende celui du seuil
        # si ce n'est pas le cas, nom_legende est un character vide
        if (!is.null(seuils)) {
          nom_legende <- seuils[[1]]@nom_seuil
        } else{
          nom_legende <- ""
        }
      }

      ##### Cas des unites #####
      # si les unités ne sont pas renseignés mais que le seuil est renseigné alors on renseigne l'étiquette unités à partir de l'information contenue dans l'objet seuil
      # si seuils est également null alors on affecte une étiquette vide à unité
      if (is.null(unite)) {
        if (!is.null(seuils)) {
          unite <-
            tools4DCE::unites_sandre[tools4DCE::unites_sandre$CdUniteMesure ==
                                       seuils[[1]]@code_unite, ]$SymUniteMesure[1]
        }
        else {
          unite <- ""
        }
      }
      #browser()

      ##### Cas du titre de graphique #####
      # si le titre n'est pas renseigné mais que le seuil est renseigné alors on renseigne le titre à partir de l'information contenue dans l'objet seuil
      # si seuils est également null alors on affecte une étiquette vide à unité
      if (is.null(titre)) {
        if (!is.null(seuils)) {
          titre <- seuils[[1]]@nom_parametre
        }
      }


      # texte commandant la création du graph
      graph1 <- ggplot()
      # ajout des couleurs
      if (!is.null(seuils1))
      {
        graph1 <-
          graph1 + geom_rect(
            data = seuils1,
            aes(
              xmin = xmini,
              xmax = xmaxi,
              ymin = SEUILMIN,
              ymax = SEUILMAX,
              fill = CLASSE
            ),
            alpha = alpha,
            show.legend = T
          )
        graph1 <-
          graph1 + scale_fill_manual(name = nom_legende,
                                     drop = FALSE,
                                     values = eval(parse(text = couleurs)))
      }

      # échelle log ou pas
      if (echelleLog) {
        graph1 <-
          graph1 + scale_y_log10(
            breaks = seuils1minmax,
            expand = c(0, 0),
            limits = c(min(seuils1minmax[seuils1minmax > -Inf]), max(seuils1minmax[seuils1minmax <
                                                                                     Inf]))
          )
      }
      if (!echelleLog) {
        graph1 <-
          graph1 + scale_y_continuous(
            breaks = seuils1minmax,
            expand = c(0, 0),
            limits = c(min(seuils1minmax[seuils1minmax > -Inf]), max(seuils1minmax[seuils1minmax <
                                                                                     Inf]))
          )
      }

      # échelle temporelle
      graph1 <-
        graph1 + scale_x_datetime(
          labels = date_format(dateformat, tz = "Europe/Paris"),
          date_breaks = break_date_max,
          date_minor_breaks = break_date_min,
          limits = c(xmini, xmaxi),
          expand = c(0, 0)
        )

      # ajout de ligne entre les points si option retenue
      if (liaison) {
        if (is.null(separ_stations)) {
          graph1 <-
            graph1 + geom_line(data = data1,
                               aes(x = DatePrel, y = RsAna),
                               linetype = "dashed")
        }
        else {
          eval(parse(
            text = paste0(
              'graph1<-graph1 + geom_line(data=data1, aes(x=DatePrel, y=RsAna, linetype = ',
              separ_stations,
              '))'
            )
          ))
        }
      }
      # ajout des indications de limites de LQ si option retenue
      if (affiche_LQ)
      {
        if (any(is.na(data1$LqAna))) {
          warning(
            "Attention certaines analyses ne comportent pas de valeurs de LQ. Les LQ manquantes ont été remplacées par 0"
          )
        }
        # on remplace les LQ manquantes par 0
        data1$LqAna <- replace_na(data1$LqAna, 0)
        graph1 <-
          graph1 + geom_ribbon(
            data = data1,
            aes(
              x = DatePrel,
              ymin = min(seuils1minmax[seuils1minmax > -Inf]),
              ymax = LqAna
            ),
            fill = "grey30",
            stat = "identity",
            position = "identity",
            show.legend = F
          )
      }

      # ajout des points pour les données si ils sont tous confondus ou si on sépare selon un facteur (ex. codes stations)
      # cas où on ne distingue pas les codes stations
      if (is.null(separ_stations))
      {
        graph1 <-
          graph1 + geom_point(data = data1, aes(x = DatePrel, y = RsAna))
      }
      else
      {
        eval(parse(
          text = paste0(
            "graph1<-graph1 + geom_point(data=data1, aes(x=DatePrel, y=RsAna, shape = ",
            separ_stations,
            "), size=3)"
          )
        ))
      }

      # ajout du titre et des noms d'axes
      if (!is.null(titre)) {
        graph1 <-
          graph1 + ggtitle(titre)
      }

      if (!is.null(sous_titre)) {
        graph1 <-
          graph1 + labs(subtitle = sous_titre)
      }

      graph1 <- graph1 + xlab('') + ylab(unite)
      # ajout des lignes au niveau des seuils1
      if (length(lignes) > 0) {
        graph1 <-
          graph1 + geom_hline(yintercept = lignes, linetype = "dashed")
      }

      # ajout des indications de valeurs hors plage de données
      if (any(!is.na(data1$depassementSUP))) {
        graph1 <-
          graph1 + geom_label(
            data = depassSUP,
            aes(
              x = DatePrel,
              y = max(seuils1minmax[seuils1minmax < Inf]),
              label = RsAna
            ),
            fill = depassSUP$couleur_pt,
            size = 2.7,
            vjust = "top"
          )
      }
      if (any(!is.na(data1$depassementINF))) {
        graph1 <-
          graph1 + geom_label(
            data = depassINF,
            aes(
              x = DatePrel,
              y = min(seuils1minmax[seuils1minmax > -Inf]),
              label = RsAna
            ),
            fill = depassINF$couleur_pt,
            size = 2.7,
            vjust = "bottom"
          )
      }

      # gestion de l'échelle et des indications sur les axes
      graph1 <- graph1 + theme_light() +
        theme(
          legend.position = ifelse(affiche_legende, 'right', 'none'),
          legend.title = element_text(colour = "black", size = taille_legende),
          axis.line = element_line(colour = "black", size = 1),
          panel.grid.major = element_line(colour = "black"),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            size = taille_axes
          ),
          axis.text.y = element_text(size = taille_axes),
          axis.title = element_text(size = taille_axes),
          plot.title = element_text(size = taille_titre),
          plot.subtitle = element_text(size = taille_sous_titre)
        )

    } # fin du traitement s'il existe bien des données à traiter

    # si le tableau de données initial est vide alors on renvoi un graph "Pas de données"

    if ((nrow(data1) == 0) | (all(is.na(data1[[col_valeurs]]))))
    {
      graph1 <-
        ggplot() + annotate("text",
                            label = "PAS DE DONNEES\nA AFFICHER",
                            x = 1,
                            y = 1)

      if (is.null(titre)) {
        if (!is.null(seuils)) {
          titre <- seuils[[1]]@nom_parametre
        } else{
          titre <- ""
        }
      }

      if (!is.null(seuils)) {
        graph1 <- graph1 + ggtitle(titre) + theme(
          axis.title = element_blank(),
          axis.text =
            element_blank(),
          axis.ticks =
            element_blank()
        )
      }

    }


    # exécution du graph
    graph1
  }
