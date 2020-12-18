#' legend_LQ
#'
#' fonction pour afficher un objet cowplot avec la légende des graphiques DCE pour les valeurs inférieuresà la LQ
#'
#'
#' @return la fonction renvoie un objet cowplot avec la légende
#'
#'@param taille_legende taille de la police de la légende
#'
#' @examples plot_grid(legend_LQ())
#'
#' @export
#'
legend_LQ<-function(taille_legende=10){get_legend(ggplot(data=data.frame(LQ=c(0), x=c(1), LQ1=c("Limite de quantification")), aes(x=x, y=LQ, fill=LQ1, ymin=0, ymax=1))+
                        geom_ribbon(show.legend=T) +  scale_fill_manual("",values="grey30") +
                        theme(legend.position = "bottom", legend.title = element_text(colour="black", size=taille_legende)))}

