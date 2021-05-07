#' graphDCE_bar
#'
#' fonction pour afficher les graphiques des paramètres en couleurs (mode histogramme)
#'
#' @param data tableau de données (dataframe)
#' @param col_annee nom de la colonne qui contient les années. Par défaut : "annee" formats character ou numeric
#' @param col_valeurs nom de la colonne qui contient les valeurs d'analyses.Par défaut : "RsAna"
#' @param seuils objet de classe seuil (factultatif)
#' @param nom_legende titre de la légende (si absent et si seuils et renseigné, la légende par défaut sera le slot nom_seuil de la légende, si seuil n'est pas renseigné, la légende sera "Légende")
#' @param affiche_legende booléen indiquant s'il faut afficher la légende (par défaut TRUE)
#' @param titre titre du graphique (si absent et si seuils et renseigné, la légende par défaut sera le slot nom_parametre)
#' @param taille_titre taille police du titre (par défaut 12)
#' @param unite unité du paramètre (par défaut si seuils est renseigné : le label SANDRE correspondant au slot code_unite du paramètre seuils)
#' @param ymaxi force l'échelle des valeurs (si non précisé, échelle automatique de ggplot2)
#' @param auto_ymaxi booléen. Si le paramètre vaut TRUE et que ymaxi n'est pas renseigné alors ce dernier est calculé automatiquement de manière à rendre le graph aussi lisible que possible
#' @param lignes vecteur permettant d'ajouter des lignes horizontales au graphique. ex c(10, 25)
#' @param taille_legende taille police caractères de la légende
#' @param alpha transparence des applats de couleurs
#'
#'
#' @return la fonction renvoie un graphique de classe ggplot
#'
#' @examples data <- data.frame(annee=seq(2010,2013), RsAna=c(12,15.5,67,18.3))
#' @examples graphDCE_bar(data, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"))
#'
#' @export
#'
graphDCE_bar <-
  function(data,
           col_annee = "annee",
           col_valeurs = "RsAna",
           ymaxi = NULL,
           auto_ymaxi = TRUE,
           unite = NULL,
           titre = NULL,
           taille_titre = 12,
           affiche_legende = T,
           nom_legende = NULL,
           taille_legende = 12,
           lignes = NULL,
           alpha = 0.8,
           seuils = NULL)
  {
    data1 <- as.data.frame(data)
		if (is.factor(data1$annee)) {
			data1$annee <- as.numeric(as.character(data1$annee))
			cat("data$annee transformée de facteur en numerique")
		}
    ymini <- 0

    if (!is.null(seuils)) {
      color_seuils <- seuils[[1]]@seuils
    } else{
			color_seuils <- NULL
    }


    if ((nrow(data1) > 0) &
        (!all(is.na(data1[[col_valeurs]]))))
      # on ne traite les données que si le tableau de données n'est pas vide
    {
      data1 <- data1 %>% dplyr::rename(ANNEE = annee, RsAna = col_valeurs)


      if (is.null(ymaxi) &
          (auto_ymaxi == T)) {
        ymaxi <- tools4DCE::calcule_ymaxi(data1$RsAna)
      }

      # séquence débutant au min des années avec données - 1 et se terminant au max des années avec données + 1 (pour déborder les applats de couleurs avant et après les années min/max)
      Xdo <- seq((min(data1$ANNEE) - 1), (max(data1$ANNEE) + 1))


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
            tools4DCE::unites_sandre[tools4DCE::unites_sandre$`Code de l'unite de reference` ==
                                       seuils[[1]]@code_unite, ]$`Symbole de l'unite de reference`[1]
        }
        else {
          unite <- ""
        }
      }

      ##### Cas du titre de graphique #####
      # si le titre n'est pas renseigné mais que le seuil est renseigné alors on renseigne le titre à partir de l'information contenue dans l'objet seuil
      # si seuils est également null alors on affecte une étiquette vide à unité
      if (is.null(titre)) {
        if (!is.null(seuils)) {
          titre <- seuils[[1]]@nom_parametre
        }
      }

      # ajout d'une colonne couleur correspondant aux seuils
      # la fonction est ajustée selon si les limites de classes inclue ou pas les bornes inférieures
      data1$couleur_pt <-
        rep("", nrow(data1)) # par défaut les étiquettes des valeurs hors gamme sont blanches

      # on ajoute la couleur pour chaque seuil
      if (!is.null(seuils)) {
        if (seuils[[1]]@bornesinfinclue) {
          f_couleur <- function(x, i) {
            if (!is.na(x)) {
              if (x >= color_seuils[i, ]$SEUILMIN &
                  x < color_seuils[i, ]$SEUILMAX) {
                as.character(color_seuils[i, ]$NOM_COULEUR)
              } else{
                ""
              }
            } else{
              ""
            }
          }
        }
        else {
          f_couleur <-
            function(x, i) {
              if (!(is.na(x))) {
                if (x > color_seuils[i, ]$SEUILMIN &
                    x <= color_seuils[i, ]$SEUILMAX) {
                  as.character(color_seuils[i, ]$NOM_COULEUR)
                } else{
                  ""
                }
              } else{
                ""
              }
            }
        }


        for (i in 1:nrow(color_seuils))
        {
          tmp <- sapply(
            data1$RsAna,
            FUN = function(x)
              f_couleur(x, i)
          )
          data1$couleur_pt <- paste0(data1$couleur_pt, tmp)
        }
      }
      if (any(which(data1$couleur_pt == ""))) {
        data1[which(data1$couleur_pt == ""), ]$couleur_pt <-
          "white"
      } # les points qui n'ont pas de couleurs sont considérés blancs

      # calcul et ajout des bornes min_max du graph
      color_seuilsminmax <-
        as.numeric(unique(c(
          ymini, color_seuils$SEUILMIN, color_seuils$SEUILMAX, ymaxi
        )))

      # calcul du nb de décimales max dans les color_seuils (à défaut dans les données) pour choisir le nb de décimales à afficher dans légende
      if (!is.null(color_seuils))
      {
        nb_decim <- max(sapply(color_seuils$SEUILMIN, compte_decimales), na.rm = T)
      }
      else
      {
        nb_decim <- max(sapply(data1$RsAna, compte_decimales), na.rm = T)
      }
      # calcul du RANGE entre min et max des données ainsi que des bornes mini des valeurs et maxi des valeurs +/- x%
      rangedata <-
        abs(max(data1$RsAna, na.rm = T) - min(data1$RsAna, na.rm = T))
      # if(rangedata==0 & !is.null(color_seuils)){rangedata<-range.default(color_seuils[,c("SEUILMIN", "SEUILMAX")], na.rm=T, finite=T)[2]-range.default(color_seuils[,c("SEUILMIN", "SEUILMAX")], na.rm=T, finite=T)[1]} # si toutes les valeurs de données sont égales alors on retient la valeur entre les color_seuils comme range
      if (rangedata == 0 &
          !is.null(color_seuils)) {
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
      if (all(sign(data1$RsAna) == 1, na.rm = T) &
          min_data < 0) {
        min_data <- 0
      }

      # si ymini et ymaxi non définis alors on zoom le graphique autour de la plage de données disponible
      if (is.null(ymini)) {
        color_seuilsminmax <- c(min_data, color_seuilsminmax[color_seuilsminmax >= min_data])
      }
      if (is.null(ymaxi)) {
        color_seuilsminmax <- c(color_seuilsminmax[color_seuilsminmax <= max_data], max_data)
      }

      # on ne conserve que les color_seuils d'affichage entre ymini et ymaxi
      if (!is.null(ymini)) {
        color_seuilsminmax <- color_seuilsminmax[color_seuilsminmax >= ymini]
      }
      if (!is.null(ymaxi)) {
        color_seuilsminmax <- color_seuilsminmax[color_seuilsminmax <= ymaxi]
      }

      # on supprime les doublons
      color_seuilsminmax <- unique(color_seuilsminmax)

      ##### mise en forme du tableau de color_seuils pour affichage des couleurs en fond de graph
      if (!is.null(color_seuils)) {
        # les couleurs sont en character et non facteurs
        color_seuils$NOM_COULEUR <- as.character(color_seuils$NOM_COULEUR)
        # on corrige le tableau de couleurs pour l'adapter aux min-max
        if (nrow(color_seuils[color_seuils$SEUILMIN < min(color_seuilsminmax, na.rm = T), ]) >
            0) {
          color_seuils[color_seuils$SEUILMIN < min(color_seuilsminmax, na.rm = T), ]$SEUILMIN <-
            min(color_seuilsminmax, na.rm = T)
        }
        if (nrow(color_seuils[color_seuils$SEUILMAX < min(color_seuilsminmax, na.rm = T), ]) >
            0) {
          color_seuils[color_seuils$SEUILMAX < min(color_seuilsminmax, na.rm = T), ]$SEUILMAX <-
            min(color_seuilsminmax, na.rm = T)
        }
        if (nrow(color_seuils[color_seuils$SEUILMIN > max(color_seuilsminmax, na.rm = T), ]) >
            0) {
          color_seuils[color_seuils$SEUILMIN > max(color_seuilsminmax, na.rm = T), ]$SEUILMIN <-
            max(color_seuilsminmax, na.rm = T)
        }
        if (nrow(color_seuils[color_seuils$SEUILMAX > max(color_seuilsminmax, na.rm = T), ]) >
            0) {
          color_seuils[color_seuils$SEUILMAX > max(color_seuilsminmax, na.rm = T), ]$SEUILMAX <-
            max(color_seuilsminmax, na.rm = T)
        }


        # préparation de la commande sur les couleurs des rectangles
        couleurs <- ""
        for (k in 1:nrow(color_seuils))
        {
          couleurs <- paste0(
            couleurs,
            '"',
            color_seuils$CLASSE[k],
            '"="',
            color_seuils$NOM_COULEUR[k],
            '"',
            ifelse(k != nrow(color_seuils), ",", "")
          )
        }
        couleurs <- paste0("c(", couleurs, ")")
        # suppression des classes de qualité avec les color_seuils min et max égaux
        color_seuils <- color_seuils[which(color_seuils$SEUILMIN != color_seuils$SEUILMAX), ]
      }

      # table des valeurs hors range qui seront étiquettées. Pour ces valeurs on remplace la valeur par le max (ou min) de l'échelle (pour afficher un point)
      data1$depassementSUP <-
        ifelse(data1$RsAna > max(color_seuilsminmax, na.rm = T),
               max(color_seuilsminmax[color_seuilsminmax < Inf], na.rm = T) ,
               NA)
      data1$depassementINF <-
        ifelse(data1$RsAna < min(color_seuilsminmax, na.rm = T),
               min(color_seuilsminmax[color_seuilsminmax > -Inf], na.rm = T),
               NA)
      if (any(!is.na(data1$depassementSUP))) {
        depassSUP <- subset(data1,!is.na(depassementSUP))
        data1[which(!is.na(data1$depassementSUP)), ]$RsAna <-
          max(color_seuilsminmax[color_seuilsminmax < Inf], na.rm = T)
      }
      if (any(!is.na(data1$depassementINF))) {
        depassINF <- subset(data1,!is.na(depassementINF))
        data1[which(!is.na(data1$depassementINF)), ]$RsAna <-
          min(color_seuilsminmax[color_seuilsminmax > -Inf], na.rm = T)
      }



      ##### REALISATION DU GRAPH #####
      graph1 <- ggplot()


      # ajout des aplats de couleur
      if (!is.null(color_seuils)) {
        graph1 <-
          graph1 + geom_rect(
            data = color_seuils,
            aes(
              xmin = min(Xdo),
              xmax = max(Xdo),
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

      # définition des y mini et maxi pour le graph
      graph1 <-
        graph1 + scale_y_continuous(
          breaks = color_seuilsminmax,
          expand = c(0, 0),
          limits = c(min(color_seuilsminmax[color_seuilsminmax > -Inf]), max(color_seuilsminmax[color_seuilsminmax <
                                                                                   Inf]))
        )

      # ajout des étiquettes sur les barres (x)
      lblXdo <- c("", Xdo[2:(length(Xdo) - 1)], "")
      graph1 <- graph1 + scale_x_continuous(breaks = Xdo, labels = lblXdo)

      # ajout des barres
      graph1 <-
        graph1 + geom_bar(
          data = data1,
          aes(x = ANNEE, y = RsAna),
          stat = "identity",
          colour = "black",
          alpha = 0.2,
          width = 1
        )


      # ajout des indications de valeurs hors plage de données
      if (any(!is.na(data1$depassementSUP))) {
        graph1 <-
          graph1 + geom_label(
            data = depassSUP,
            aes(
              x = ANNEE,
              y = max(color_seuilsminmax[color_seuilsminmax < Inf]),
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
              x = ANNEE,
              y = min(color_seuilsminmax[color_seuilsminmax > -Inf]),
              label = RsAna
            ),
            fill = depassINF$couleur_pt,
            size = 2.7,
            vjust = "bottom"
          )
      }


      # ajout du titre et des noms d'axes
      if (!is.null(titre)) {
        graph1 <-
          graph1 + ggtitle(titre) + theme(plot.title = element_text(size = taille_titre))
      }
      graph1 <- graph1 + xlab('') + ylab(unite)

      # ajout des lignes au niveau des color_seuils
      if (length(lignes) > 0) {
        graph1 <- graph1 + geom_hline(yintercept = lignes, linetype = "dashed")
      }


      # gestion de l'échelle et des indications sur les axes
      graph1 <- graph1 + theme_light() +
        theme(
          legend.position = ifelse(affiche_legende, 'right', 'none'),
          legend.title = element_text(colour = "black", size = taille_legende),
          axis.line = element_line(colour = "black", size = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"),
          axis.text.x = element_text(angle = 90, vjust = 0.5),

        )


    }

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

    return(graph1)
  }
