#' arrondi
#'
#' fonction pour calculer un arrondi conforme aux usages en France (les nombres qui se terminent par 0.5 sont arrondis à l'unité supérieure)
#'
#' @param nb nombre à arrondir
#'
#' @return l'arrondi du nombre selon les règles usuelles en France
#'
#' @examples arrondi(8.5)
#' @export

arrondi<-function(nb)
{
  if(nb>=0)
  {
    if(abs(nb-floor(nb))<0.5)
    {floor(nb)}
    else
    {floor(nb)+1}
  }
  else
  {if(abs(nb-floor(nb)<=0.5))
  {floor(nb)}
    else
    {floor(nb)+1}
  }
}

