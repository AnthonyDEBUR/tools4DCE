#' graphDCE_boxplot
#'
#' fonction pour afficher les graphiques des paramètres en couleurs (mode boxplot).
#' Utilisé par exemple pour visualiser les mois avec les plus hautes concentrations.
#'
#' @param data tableau de données (dataframe)
#' @param col_mois nom de la colonne qui contient les mois. Par défaut : "mois" formats character ou numeric
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
#' @examples data0<-data.frame(DatePrel=Sys.Date() + sort(sample(1:2000, 100)), RsAna=c(round(runif(100,0,100), 0)), LqAna=c(3))
#' @examples data0$mois<-format(data0$DatePrel, "%m")%>%factor
#' @examples graphDCE_boxplot(data0, seuils=makeSeuils(CdParametre = "1340", type_seuil="DCE"), affiche_LQ = T, ymini = 0)
#'
#' @export
#'
graphDCE_boxplot <-
  function(data,
           col_mois = "mois",
           col_valeurs = "RsAna",
           ymaxi = NULL,
           auto_ymaxi = TRUE,
           seuils = NULL,
           unite = NULL,
           titre = NULL,
           taille_titre = 12,
           affiche_legende = T,
           nom_legende = NULL,
           taille_legende = 12,
           lignes = NULL,
           alpha = 0.8)
  {
    data1 <- data.frame(data)
    ymini <- 0
    if (!is.null(seuils)) {
      seuils1 <- seuils[[1]]@seuils
    } else{
      seuils1 <- NULL
    }


    if ((nrow(data1) > 0) &
        (!all(is.na(data1[[col_valeurs]]))))
      # on ne traite les données que si le tableau de données n'est pas vide
    {
      if(is.factor(data1$mois)){Xdo<-levels(data1$mois)}else{Xdo<-unique(data1$mois)%>%sort}
      data1$mois <- data1[[col_mois]] %>% as.character
      data1$RsAna <- data1[[col_valeurs]]
      if (is.null(ymaxi) &
          (auto_ymaxi == T)) {
        ymaxi <- tools4DCE::calcule_ymaxi(data1$RsAna)
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
            tools4DCE::unites_sandre[tools4DCE::unites_sandre$`Code de l'unité de référence` ==
                                       seuils[[1]]@code_unite, ]$`Symbole de l'unité de référence`[1]
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
              if (x >= seuils1[i, ]$SEUILMIN &
                  x < seuils1[i, ]$SEUILMAX) {
                as.character(seuils1[i, ]$NOM_COULEUR)
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
                if (x > seuils1[i, ]$SEUILMIN &
                    x <= seuils1[i, ]$SEUILMAX) {
                  as.character(seuils1[i, ]$NOM_COULEUR)
                } else{
                  ""
                }
              } else{
                ""
              }
            }
        }


        for (i in 1:nrow(seuils1))
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
      seuils1minmax <-
        as.numeric(unique(c(
          ymini, seuils1$SEUILMIN, seuils1$SEUILMAX, ymaxi
        )))

      # calcul du nb de décimales max dans les seuils1 (à défaut dans les données) pour choisir le nb de décimales à afficher dans légende
      if (!is.null(seuils1))
      {
        nb_decim <- max(sapply(seuils1$SEUILMIN, compte_decimales), na.rm = T)
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
      if (all(sign(data1$RsAna) == 1, na.rm = T) &
          min_data < 0) {
        min_data <- 0
      }

      # si ymini et ymaxi non définis alors on zoom le graphique autour de la plage de données disponible
      if (is.null(ymini)) {
        seuils1minmax <- c(min_data, seuils1minmax[seuils1minmax >= min_data])
      }
      if (is.null(ymaxi)) {
        seuils1minmax <- c(seuils1minmax[seuils1minmax <= max_data], max_data)
      }

      # on ne conserve que les seuils1 d'affichage entre ymini et ymaxi
      if (!is.null(ymini)) {
        seuils1minmax <- seuils1minmax[seuils1minmax >= ymini]
      }
      if (!is.null(ymaxi)) {
        seuils1minmax <- seuils1minmax[seuils1minmax <= ymaxi]
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
        seuils1 <- seuils1[which(seuils1$SEUILMIN != seuils1$SEUILMAX), ]
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



      ##### REALISATION DU GRAPH #####
      graph1 <- ggplot()


      # ajout des aplats de couleur
      if (!is.null(seuils1)) {
        graph1 <-
          graph1 + geom_rect(
            data = seuils1,
            aes(
              xmin=min(as.numeric(Xdo))-0.5,
              xmax=max(as.numeric(Xdo))+0.5,
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
          breaks = seuils1minmax,
          expand = c(0, 0),
          limits = c(min(seuils1minmax[seuils1minmax > -Inf]), max(seuils1minmax[seuils1minmax <
                                                                                   Inf]))
        )

      # ajout des étiquettes sur les boxplot (x)
      graph1 <- graph1 + scale_x_discrete(labels = Xdo)

      # ajout des boxplot
      graph1 <-
        graph1 + geom_boxplot(
          data = data1,
          aes(x = mois, y = RsAna),
          colour = "black",
          alpha = 0.2,
          width = 0.9
        )


      # ajout des indications de valeurs hors plage de données
      if (any(!is.na(data1$depassementSUP))) {
        graph1 <-
          graph1 + geom_label(
            data = depassSUP,
            aes(
              x = mois,
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
              x = mois,
              y = min(seuils1minmax[seuils1minmax > -Inf]),
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

      # ajout des lignes au niveau des seuils1
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
