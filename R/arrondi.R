#' arrondi
#'
#' fonction pour calculer un arrondi conforme aux usages en France (les nombres qui se terminent par 0.5 sont arrondis à l'unité supérieure)
#'
#' @param nb nombre à arrondir
#' @param digits nombre de chiffres significatifs
#'
#' @return l'arrondi du nombre selon les règles usuelles en France
#'
#' @examples arrondi(8.5)
#' @export

arrondi<-function(nb, digits=0)
{
  if(class(digits)!="numeric"){stop("digits doit être un nombre")}
  if(digits %% 1 != 0 ){stop("digits doit être un entier")}
  if(digits < 0 ){stop("digits doit être un nombre positif")}

  nb<-nb*(10^digits)

  if(nb>=0)
  {
    if(abs(nb-floor(nb))<0.5)
    {nb<-floor(nb)}
    else
    {nb<-floor(nb)+1}
  }
  else
  {if(abs(nb-floor(nb)<=0.5))
  {nb<-floor(nb)}
    else
    {nb<-floor(nb)+1}
  }

  nb<-nb/(10^digits)

  return(nb)

}

