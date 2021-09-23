#' graphDCE_distribution
#'
#' fonction pour réaliser un graphique bilan de quantifications
#'
#' @param donnees : dataframe contenant les analyses dont il faut faire le bilan dans un format du type de celui rendu par la fonction groupe_tableau_distribution
#' @param titre : titre du graphique
#' @param legende_LQ : vecteur de character de longueur 2 qui précise l'étiquette pour les données inférieures et supérieures à la LQ
#' @param affiche_valeurs : booléen qui indique d'il faut afficher les valeurs ou pas dans les barres
#' @param min_affiche : pourcentage minimum à atteindre pour que la valeur soit affiché (ex si min_affiche=0.3, seules le nb de prélèvements représentant au moins 30% pour une categorie et un paramètre sera affiché)
#' @param taille_titre taille police du titre (par défaut 12)
#' @param tri_donnees booleen vrai par défaut. Si il est vrai, les résultats sont affichés par ordre du plus dégradé au moins dégradé (au sens des dépassement de seuils). Si non, les résultats sont affichés dans l'ordre alphabétique des noms de paramètres.
#' @param tri_croissant booleen vrai par défaut. Si faux, l'ordre du tri est inversé. Paramètre sans effet si tri_donnees=FALSE
#' @param nb_top : optionnel. Si renseigné, le graph n'affiche que le nb de paramètres indiqués en affichant en priorité les + dégradés
#' @return la fonction renvoie un objet ggplot avec le graphique de distribution
#'
#' @examples donnees<-data.frame(parametres=rep(c("1301", "1340", "1335"), 100), RsAna=sample(0.1:100, 300, replace=TRUE), LqAna=c(0.5,1,6))
#' @examples donnees<-donnees%>%mutate(RsAna=ifelse(RsAna<LqAna, LqAna, RsAna))
#' @examples donnees<-donnees%>%mutate(CdRqAna=ifelse(RsAna>LqAna, "1", ifelse(sample(1:100,5)>10,"10","1")))
#' @examples seuils<-makeSeuils(CdParametre=donnees$parametres%>%unique, specificites=c("CYPRINICOLE", rep(NA,2)), type_seuil = "DCE")
#' @examples tableau<-groupe_tableau_distribution(donnees, col_CdParametre="parametres", col_CdSupport=NULL, col_CdFraction=NULL, col_CdUnite=NULL, seuils = seuils)
#' @examples graphDCE_distribution(tableau)
#' @export
graphDCE_distribution <-
  function(donnees,
           titre = "",
           taille_titre = 12,
           legende_LQ = c("NON QUANTIFIE", "QUANTIFIE"),
           affiche_valeurs = T,
           min_affiche = 0.1,
           tri_donnees = T,
           tri_croissant = T,
           nb_top = NULL) {

    sous_titre<-""

    # creation d'un tableau de correspondance CATEGORIE - LEGENDE
    cat_leg <-
      donnees %>% select(CLASSE, NOM_COULEUR) %>% distinct %>% arrange(CLASSE)

    # ajout d'une colonne pour savoir si on doit afficher les étiquettes de valeurs
    seuil_param <-
      donnees %>% group_by(parametre) %>% dplyr::summarise(total = sum(nb))
    donnees <- donnees %>% left_join(seuil_param, by = "parametre")

    # si tri_donnees, on tri les résultats en mettant en 1er le plus haut pourcentage du 1er facteur de niveau
    donnees$pc <- donnees$nb / donnees$total
    donnees$ordre_tri <-
      donnees$CLASSE %>% as.character %>% factor(levels = sort(levels(cat_leg$CLASSE), decreasing = tri_croissant))
    donnees$CdRqAna <-
      donnees$CdRqAna %>% as.character %>% factor(levels = c("3", "1", "10", "2", "7"))
    donnees <- donnees %>% arrange(CdRqAna, ordre_tri,-pc)
    if (tri_donnees) {
      donnees$parametre <-
        donnees$parametre %>% as.character %>% factor(levels = unique(donnees$parametre))
    } else{
      donnees$parametre <- factor(donnees$parametre)
    }

    # si nb_top n'est pas nul, on retient les nb_top 1ers paramètres (soit par ordre de tri, soit par ordre alphabétique)
    if(!is.null(nb_top))
    {
      if(!is.numeric(nb_top)){stop("le parametre nb_top n'est pas un nombre entier positif.")}
      if(nb_top%%1!=0 | nb_top<1){stop("le parametre nb_top n'est pas un nombre entier positif.")}
      donnees<-donnees%>%subset(parametre%in%levels(donnees$parametre)[1:nb_top])
      sous_titre<-paste0("Top ", nb_top," des paramètres")
    }

    graph <-
      ggplot(donnees,
             aes(
               x = nb,
               y = parametre,
               fill = CLASSE,
               alpha = ALPHA %>% as.factor
             )) +
      geom_bar(position = "fill", stat = "identity") + scale_alpha_manual(values =
                                                                            c(0.20, 1), labels = legende_LQ) +
      scale_fill_manual(labels = cat_leg$CLASSE, values = cat_leg$NOM_COULEUR) +
      scale_x_continuous(labels = scales::percent) +
      labs(
        title = titre,
        subtitle = sous_titre,
        x = "",
        y = "",
        alpha = "QUANTIFICATION"
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = taille_titre),
        legend.box = "vertical",
        legend.margin = margin()
      )
    # guides(fill=guide_legend(nrow=2,byrow=TRUE))


    # ajout des étiquettes si demandé
    if (affiche_valeurs) {
      graph <-
        graph + geom_text(
          data = donnees %>% subset(nb > min_affiche * total),
          aes(x = nb, y = parametre, label = nb),
          stat = 'identity',
          position = position_fill(vjust = 0.5)
        )
    }


    return(graph)

  }

