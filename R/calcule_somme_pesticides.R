#' calcule_somme_pesticides
#'
#' fonction pour calculer la somme des concentrations en pesticides.
#' Si pour une même station et une même date il y a plusieurs résultats du même paramètre alors on retient la valeur max de ces résultats.
#' Si pour une même station et une même date on trouve un paramètre inclus dans un deuxième (ex. S-métolachlore et métolachlore total), alors seule la valeur du paramètre qui englobe l'autre (métolachore total par ex.) est retenu.
#' paramètres incluant d'autres paramètres :
#' - métolachlore total (somme des isomères R et S) + exclure le paramètre S-métolachlore (mélange des 2 isomères avec au moins 80% d'isomère S) si le métolachlore total est renseigné
#' - mecoprop (inclus Mécoprop-P)
#' - diméthénamide (inclus diméthénamide-P (= isomère S))
#' - dichlorprop (inclus dichlorprop-P)
#' - Uniconizole (inclus uniconizole-P)
#' - Fluazifop (inclus Fluazifop-P)
#' - Somme des Hexachlorocyclohexanes
#' - Somme Heptachlore époxyde cis/trans
#' - Somme du DDE 44' et de la dieldrine
#' - Somme des metabolites des dithiocarbamates (6235) = Ethylenethiouree (5648) + Ethyluree (5484) + Propylene thiouree (6214)
#' - Somme de Ethylamine + Diméthylamine (7887) = Ethylamine (6993) + Diméthylamine (2773)
#' - Somme du Fenvalerate RR et Esfenvalerate SS (6613) = Fenvalerate RR 6606 + Esfenvalerate SS 6608
#' - Somme des chloroanilines (m+p) 5502	= Chloroaniline-4 1591 + Chloroaniline-3 1592
#' - Somme Acétochlore ESA + Alachlore ESA (7750) = Acétochlore ESA 6856 + Alachlore ESA 6800
#' - Somme du DDD 44' et du DDT 24'6496 = DDD 44' 1144 + DDT 24' 1147
#' - Somme Metacresol, Orthocresol et Paracrésol 6341 = ortho-crésol 1640 + méta-crésol 1639 + para-crésol 1638
#' - Somme parathion ethyl+methyl 6947	= parathion éthyl 1232 + parathion methyl 1233
#' - Somme du DDD 24', DDE 24', DDT 24', DDT 44' 7170 =  DDD 24' 1143 + DDE 24' 1145 + DDT 24' 1147 + DDT 44' 1148
#' - Somme du DDDpp', DDEpp', DDTop', DDTpp' 7146 = DDDpp' 1144 + DDEpp' 1146 + DDTop' 1147 + DDTpp' 1148
#' - Somme DDT (3268) = DDT 24' (1147) + DDT 44' (1148)
#' - Somme DDT et métabolites DDE DDD (6497) = DDDop' (1143) + DDDpp' (1144) + DDEop' (1145) + DDEpp' (1146) + DDTop' (1147) + DDTpp' (1148)
#' - Somme DDT et métabolites DDE DDD (6497) = DDDpp' (1144) + DDEpp' (1146) + Somme du DDD 24', DDE 24', DDT 24', DDT 44' (7170)
#' - Somme de l'Alachlor OXA et de l'Acetochlor OXA 8101	= Alachlor OXA 6855 + Acetochlor OXA 6862
#' - Somme de Fluazifop-P-butyl (1404) et de Fluazifop-butyl (1825)
#'  - Métalaxyl (1706) (inclus les formes métalaxyl-M = Méfénoxam (2987) )
#' - (Somme du Fenvalerate RR et Esfenvalerate SS ( 6613 ))= Fenvalerate RR (6606) + Esfenvalerate SS (6608)
#' -  (Somme du Fenvalerate RS et Esfenvalerate SR (6614)) = Fenvalerate RS (6607) + Esfenvalerate SR (6609)
#'-  (Somme du Fenvalerate et du Esfenvalerate 	(8592)) = Fenvalerate (1701) + Esfenvalerate (1809)
#' -  =  Fenvalerate RR (6606) + Esfenvalerate SS (6608) + Fenvalerate RS (6607) + Esfenvalerate SR (6609)
#'-   = Somme du Fenvalerate RR et Esfenvalerate SS ( 6613 ) + Somme du Fenvalerate RS et Esfenvalerate SR (6614)
#'  - HCH alpha+beta+delta+gamma (5537) = HCH delta (1202) + HCH alpha (1200)+ HCH béta (1201)+ HCH gamma (lindane) (1203)
#'  - Permethrin (somme)(1523)= Permethrin cis (5682) +Permethrin trans (5683)
#'- Sulfosate (2077) = sel du glyphosate (1506).
#' Pour calculer la concentration en glyphosate à partir du sulfosate, il faut multiplier cette dernière par 0.690
#' - Endosulfan (1743) = Endosulfan alpha (1178) + Endosulfan bêta (1179)
#' - Somme de Endosulfan alpha, Endosulfan bêta et Endosulfan sulfate (8129) = Endosulfan alpha (1178) + Endosulfan bêta (1179) + Endosulfan sulfate (1742)
#' - Chlordane (1132) = Chlrodane alpha (7010) + chlordane bêta (1757)
#' - Déméton (1550) = Déméton-O (1150) + Déméton-S (1152)
#' - Mépiquat chlorure (2089) = sel du mepiquat (1969).
#' Pour calculer la concentration en mepiquat à partir du Mépiquat chlorure, il faut multiplier cette dernière par 0.7631.
#' - Chlordane (cis + trans) (1132) = chlordane alpha (7010) +  chlordane bêta (1757)
#' - Demeton O+S (1550) = déméton-O (code Sandre n°1150) + déméton-S (code Sandre n°1152)
#' - Heptachlore époxyde (cis+trans) (1198) = Heptachlore époxyde exo cis (1748) + Heptachlore époxyde endo trans (1749)
#' - Spinosad (A+D) (spinosyne) (5610) = Spinosad A (7438) + Spinosad D (7439)
#'
#' @param data tableau de données avec les résultats d'analyse
#' @param liste_pesticides vecteur qui contient les identifiants des pesticides à prendre en compte. Si NULL, toutes les molécules du tableau sont prises en compte.
#' @param col_parametre nom de la colonne qui identifie les pesticides. Par défaut CdParametre
#' @param col_date nom de la colonne avec la date du prélèvement. Par defaut DatePrel.
#' @param col_valeur nom de la colonne avec les résultats d'analyse. Par défaut RsAna.
#' @param col_CdRq nom de la colonne avec le code remarque d'analyse. Par défaut CdRqAna.
#' @param col_LQ nom de la colonne avec les limites de quantification des analyses. Par défaut LqAna.
#' @param col_station nom de la colonne qui renseigne sur où se trouve les différentes stations. Par défaut CdStationMesureEauxSurface
#' @param col_unite nom de la colonne avec les unités/ Par défaut CdUniteMesure
#' @param valeur_inf_LQ stratégie à appliquer pour les valeurs inférieures à LQ. Par défaut "0" : on remplace les valeurs inférieures à la LQ par 0. Autre possibilité : "LQ/2" : on remplace les valeurs inférieures à LQ par LQ/2. "LQ" : on remplace les valeurs inférieures à LQ par LQ.
#' @param resultat_seul booléen. Si il vaut TRUE, la fonction ne renvoie que la colonne somme pesticides. Si il vaut false, la fonction renvoie une colonne par paramètre pris en compte
#'
#' @return la fonction renvoie une dataframe avec les informations sur la station, la date, l'unité et la valeur de la somme des pesticides ainsi qu'une colonne avec chaque pesticide constituant la somme.
#'
#'@examples data<-data.frame(DatePrel=Sys.Date() + rep(sort(sample(1:500, 10)),3), RsAna=c(round(runif(60,0,0.5), 2)), LqAna=c(0.1), CdStationMesureEauxSurface=c("A","B","C"), CdParametre=c("1200","1201"), CdUniteMesure="133")
#'@examples data$CdRqAna<-ifelse(data$RsAna>=data$LqAna, "1","10")
#' @examples calcule_somme_pesticides(data)
#' @export
calcule_somme_pesticides <-
  function(data,
           liste_pesticides = NULL,
           col_parametre = "CdParametre",
           col_date = "DatePrel",
           col_valeur = "RsAna",
           col_CdRq = "CdRqAna",
           col_LQ = "LqAna",
           col_station = "CdStationMesureEauxSurface",
           col_unite = "CdUniteMesure",
           valeur_inf_LQ = "0",
           resultat_seul = T) {
    # teste si le format en entrée est correct
    if (!valeur_inf_LQ %in% c("0", "LQ/2", "LQ")) {
      stop("valeur_inf_LQ ne correspond pas à une valeur possible ('0','LQ/2','LQ')")
    }

    # mise en forme du tableau de donnees
    data1 <- data
    data1$CdParametre <- data[[col_parametre]]
    data1$DatePrel <- data[[col_date]]
    data1$RsAna <- data[[col_valeur]]
    data1$CdRqAna <- data[[col_CdRq]]
    data1$LqAna <- data[[col_LQ]]
    data1$CdStationMesureEauxSurface <- data[[col_station]]
    data1$CdUniteMesure <- data[[col_unite]]

    # s'il existe le paramètre somme des pesticides (code SANDRE 6276) dans le jeu de données, on supprime ces lignes car on va recalculer le paramètre
    try(data1 <- data1 %>% subset(CdParametre != "6276"))


    # si liste des pesticides n'est pas nulle, on ne retient que les pesticides de la liste
    if (!is.null(liste_pesticides)) {
      data1 <- data1 %>% subset(CdParametre %in% liste_pesticides)
      if (nrow(data) != nrow(data1)) {
        warning(
          paste0(
            "Attention ",
            nrow(data) - nrow(data1),
            " lignes de donnees ne correspondent pas a des pesticides de la liste. Ces lignes ont ete ignorees dans le calcul de la somme des pesticides."
          )
        )
      }
    }
    if (is.null(liste_pesticides)) {
      liste_pesticides <- data1$CdParametre %>% unique()
    }

    # si la liste des données est nulle, on renvoie un message d'erreur
    if (nrow(data1) == 0) {
      stop("Aucune donnée pesticides parmi le tableau de données.")
    }

    # si les unites sont differentes, on renvoie un message d'erreur
    if (length(unique(data1$CdUniteMesure)) > 1) {
      stop(
        "Toutes les analyses doivent être dans la même unité pour pouvoir calculer les sommes de pesticides."
      )
    }

    unite <- data1$CdUniteMesure[1]

    # on applique la strategie LQ
    if (valeur_inf_LQ == "0") {
      try(data1[data1$CdRqAna != "1",]$RsAna <- 0, silent = T)
    }
    if (valeur_inf_LQ == "LQ/2") {
      try(data1[data1$CdRqAna != "1",]$RsAna <-
            data1[data1$CdRqAna != "1",]$LqAna / 2, silent = T)
    }
    if (valeur_inf_LQ == "LQ") {
      try(data1[data1$CdRqAna != "1",]$RsAna <-
            data1[data1$CdRqAna != "1",]$LqAna, silent = T)
    }

    # on fait un tableau croise par date / code station
    data2 <-
      pivot_wider(
        data1,
        id_cols = c("CdStationMesureEauxSurface", "DatePrel", "CdUniteMesure"),
        names_from = "CdParametre",
        names_prefix = "par_",
        values_from = "RsAna",
        values_fn = max,
        values_fill = NA
      )

    # Fonction de remplacement d'une substance par une qui la contient
    # exemple Mécoprop (1214) > Mécoprop-P (2084)
    # exemple f_remplace_inclus_dedans(2084, 1214)
    # l'énantiomère peut aussi être un sel
    # coef conversion = coef pour convertir un sel en la molécule mère (ex sulfosate en glyphosate)
    # ex f_remplace_inclus_dedans("2077", "1506", 0.690)
    f_remplace_inclus_dedans <-
      function(cd_sandre_enantiomere,
               cd_sandre_molecule_supra,
               coef_conversion = 1)
      {
        if (cd_sandre_molecule_supra %in% liste_pesticides) {
          if (!(paste0("par_", cd_sandre_molecule_supra) %in% names(data2))) {
            data2 <-
              data2 %>% add_column(!!paste0("par_", cd_sandre_molecule_supra) := NA)
          }
        }
        if (paste0("par_", cd_sandre_molecule_supra) %in% names(data2)) {
          if (!(paste0("par_", cd_sandre_enantiomere) %in% names(data2))) {
            data2 <-
              data2 %>% add_column(!!paste0("par_", cd_sandre_enantiomere) := 0)
          }

          # cas de sels : on converti le paramètre sel
          nom_col_enamt <- paste0("par_", cd_sandre_enantiomere)

          data2 <-
            data2 %>% mutate({
              {
                nom_col_enamt
              }
            }  :=  .data[[nom_col_enamt]] * coef_conversion)

          # on prend le max entre l'énantiomère et la molécule totale
          data2$maxi_par <-
            apply(data2[, c(
              paste0("par_", cd_sandre_enantiomere),
              paste0("par_", cd_sandre_molecule_supra)
            )], 1, function(x) {
              ifelse(all(is.na(x)), NA , max(x, na.rm = T))

            })
          data2 <-
            data2 %>% mutate("par_{cd_sandre_molecule_supra}" := maxi_par)

          # on supprime les colonnes hors molécule totale
          data2 <-
            data2 %>% select(-paste0("par_", cd_sandre_enantiomere)) %>% select(-maxi_par)

        }
        return(data2)
      }



    # fonction pour sommer les paramètres individuels qui sont groupés dans un paramètre somme
    remplace_somme <- function(code_somme, vecteur_codes_a_sommer)
    {
      # si le paramètre est dans la liste des pesticides
      if (code_somme %in% liste_pesticides) {
        if (!(paste0("par_", code_somme) %in% names(data2))) {
          data2 <- data2 %>% add_column(!!"par_{code_somme}" := NA)
        }

        if (paste0("par_", code_somme) %in% names(data2)) {
          for (z in 1:length(vecteur_codes_a_sommer))
          {
            if (!(paste0("par_", vecteur_codes_a_sommer[z]) %in% names(data2))) {
              colonne_tmp<-paste0("par_",vecteur_codes_a_sommer[z])

              data2 <-
                data2 %>% add_column({{colonne_tmp}} :=  0)
            }
          }
        }


        cc <- paste("par_", vecteur_codes_a_sommer, sep = "")

        # on somme les sous-composantes
        data2$par_somme_tmp <-
          rowSums(data2[, cc], na.rm = TRUE)
        # on prend le max entre la somme des sous-composantes et la substance somme
        data2$par_somme_tmp <-
          apply(data2[, c("par_somme_tmp", paste0("par_", code_somme))], 1, function(x) {
            ifelse(all(is.na(x)), NA , max(x, na.rm = T))
          })
        # on remplace le paramètre substance somme s'il n'est pas renseigné
        data2 <-
          data2 %>% mutate("par_{code_somme}" := ifelse(is.na(eval(
            parse(text = paste0("data2$par_", code_somme))
          )), data2$par_somme_tmp, eval(parse(
            text = paste0("data2$par_", code_somme)
          ))))


        # on supprime les colonnes des paramètres individuels
        data2 <-
          data2 %>% select(-all_of(cc)) %>% select(-par_somme_tmp)
      }
      return(data2)
    }





    # cas du métolachlore
    #  Métolachlore total (1221) > S-Métolachlore (2974) > Métolachlore énantiomère S (8070) +  Métolachlore énantiomère R (8071)

    # Métolachlore total (1221) = Métolachlore énantiomère S (8070) +  Métolachlore énantiomère R (8071)
    data2 <-
      remplace_somme(code_somme = "1221",
                     vecteur_codes_a_sommer = c("8070", "8071"))
    #  S-Métolachlore (2974) est inclus dans Métolachlore total (1221)
    data2 <- f_remplace_inclus_dedans("2974", "1221")


    # cas du mecoprop
    #  Mécoprop (1214) > Mécoprop-P (2084)
    data2 <- f_remplace_inclus_dedans("2084", "1214")


    # cas du diméthénamide
    #  Diméthénamide (1678) > Diméthénamide-P (5617)
    data2 <- f_remplace_inclus_dedans("5617", "1678")


    # cas du Dichlorprop
    #  Dichlorprop (1169) > Dichlorprop-P (2544)
    data2 <- f_remplace_inclus_dedans("2544", "1169")

    # cas du Uniconizole
    #  Uniconizole (7482) > Uniconizole-P (	5845)
    data2 <- f_remplace_inclus_dedans("5845", "7482")

    # cas du Fluazifop
    #  Fluazifop (6545) > Fluazifop-P (5634)
    data2 <- f_remplace_inclus_dedans("5634", "6545")

    # cas du sulfosate, sel du glyphosate (on converti le résultat en glyphosate si ce dernier n'est pas déjà mesuré)
    # Sulfosate (2077) = sel du glyphosate (1506). Pour calculer la concentration en glyphosate à partir du sulfosate, il faut multiplier cette dernière par 0.690
    data2 <- f_remplace_inclus_dedans("2077", "1506", 0.690)

    #	Mepiquat chlorure (2089) = sel du mepiquat (1969). Pour calculer la concentration en mepiquat à partir du Mépiquat chlorure, il faut multiplier cette dernière par 0.7631
    data2 <- f_remplace_inclus_dedans("2089", "1969", 0.7631)


    # Somme des Hexachlorocyclohexanes (5537) = Hexachlorocyclohexane alpha (1200)
    # + bêta (1201) + delta (1202) + gamma (1203)
    data2 <-
      remplace_somme(
        code_somme = "5537",
        vecteur_codes_a_sommer = c("1200", "1201", "1202", "1203")
      )

    # Somme Heptachlore époxyde cis/trans (1198) = Heptachlore époxyde cis (1748)  + trans (1749)
    data2 <-
      remplace_somme(code_somme = "1198",
                     vecteur_codes_a_sommer = c("1748", "1749"))

    # Somme du DDE 44' et de la dieldrine
    data2 <-
      remplace_somme(code_somme = "6500",
                     vecteur_codes_a_sommer = c("1146", "1173"))

    # Somme des metabolites des dithiocarbamates (6235) = Ethylenethiouree (5648) + Ethyluree (5484) + Propylene thiouree (6214)
    data2 <-
      remplace_somme(
        code_somme = "6235",
        vecteur_codes_a_sommer = c("5648", "5484", "6214")
      )

    # Somme de Fluazifop-P-butyl (1404) et de Fluazifop-butyl (1825)
    data2 <-
      remplace_somme(code_somme = "8366",
                     vecteur_codes_a_sommer = c("1404", "1825"))

    # Somme de Ethylamine + Diméthylamine (	7887) = Ethylamine (6993) + Diméthylamine (2773)
    data2 <-
      remplace_somme(code_somme = "7887",
                     vecteur_codes_a_sommer = c("6993", "2773"))

    # Somme du Fenvalerate RR et Esfenvalerate SS (6613) = Fenvalerate RR 6606 + Esfenvalerate SS 6608
    data2 <-
      remplace_somme(code_somme = "6613",
                     vecteur_codes_a_sommer = c("6606", "6608"))

    # Somme des chloroanilines (m+p) 5502	= Chloroaniline-4 1591 + Chloroaniline-3 1592
    data2 <-
      remplace_somme(code_somme = "5502",
                     vecteur_codes_a_sommer = c("1591", "1592"))

    # Somme Acétochlore ESA + Alachlore ESA (7750) = Acétochlore ESA 6856 + Alachlore ESA 6800
    data2 <-
      remplace_somme(code_somme = "7750",
                     vecteur_codes_a_sommer = c("6856", "6800"))

    # Somme du DDD 44' et du DDT 24'6496 = DDD 44' 1144 + DDT 24' 1147
    data2 <-
      remplace_somme(code_somme = "6496",
                     vecteur_codes_a_sommer = c("1144", "1147"))

    # Somme DDT (3268) = DDT 24' (1147) + DDT 44' (1148)
    data2 <-
      remplace_somme(code_somme = "3268",
                     vecteur_codes_a_sommer = c("1147", "1148"))


    # Somme DDT et métabolites DDE DDD (6497) = DDDop' (1143) + DDDpp' (1144) + DDEop' (1145) + DDEpp' (1146) + DDTop' (1147) + DDTpp' (1148)
    data2 <-
      remplace_somme(
        code_somme = "6497",
        vecteur_codes_a_sommer = c("1143", "1144", "1145", "1146", "1147", "1148")
      )

    # Somme DDT et métabolites DDE DDD (6497) = DDDpp' (1144) + DDEpp' (1146) + Somme du DDD 24', DDE 24', DDT 24', DDT 44' (7170)
    data2 <-
      remplace_somme(
        code_somme = "6497",
        vecteur_codes_a_sommer = c("1144", "1146", "7170")
      )

    # Somme Metacresol, Orthocresol et Paracrésol 6341 = ortho-crésol 1640 + méta-crésol 1639 + para-crésol 1638
    data2 <-
      remplace_somme(
        code_somme = "6341",
        vecteur_codes_a_sommer = c("1640", "1639", "1638")
      )

    # Somme parathion ethyl+methyl 6947	= parathion éthyl 1232 + parathion methyl 1233
    data2 <-
      remplace_somme(code_somme = "6947",
                     vecteur_codes_a_sommer = c("1232", "1233"))

    # Somme du DDD 24', DDE 24', DDT 24', DDT 44' 7170 =  DDD 24' 1143 + DDE 24' 1145 + DDT 24' 1147 + DDT 44' 1148
    data2 <-
      remplace_somme(
        code_somme = "7170",
        vecteur_codes_a_sommer = c("1143", "1145", "1147", "1148")
      )

    # Somme du DDDpp', DDEpp', DDTop', DDTpp' 7146 = DDDpp' 1144 + DDEpp' 1146 + DDTop' 1147 + DDTpp' 1148
    data2 <-
      remplace_somme(
        code_somme = "7146",
        vecteur_codes_a_sommer = c("1144", "1146", "1147", "1148")
      )

    # Somme de l'Alachlor OXA et de l'Acetochlor OXA 8101	= Alachlor OXA 6855 + Acetochlor OXA 6862
    data2 <-
      remplace_somme(code_somme = "8101",
                     vecteur_codes_a_sommer = c("6855", "6862"))


    # Somme de Endosulfan (1743) = Endosulfan alpha (1178) + Endosulfan bêta (1179)
    data2 <-
      remplace_somme(code_somme = "1743",
                     vecteur_codes_a_sommer = c("1178", "1179"))

    # Somme de Endosulfan alpha, Endosulfan bêta et Endosulfan sulfate (8129) = Endosulfan alpha (1178) + Endosulfan bêta (1179) + Endosulfan sulfate (1742)
    data2 <-
      remplace_somme(
        code_somme = "8129",
        vecteur_codes_a_sommer = c("1178", "1179", "1742")
      )

    # Somme de Endosulfan alpha, Endosulfan bêta et Endosulfan sulfate (8129) = (Somme de Endosulfan (1743)) + Endosulfan sulfate (1742)
    data2 <-
      remplace_somme(code_somme = "8129",
                     vecteur_codes_a_sommer = c("1743", "1742"))

    # Somme de Chlordane (1132) = Chlrodane alpha (7010) + chlordane bêta (1757)
    data2 <-
      remplace_somme(code_somme = "1132",
                     vecteur_codes_a_sommer = c("7010", "1757"))


    # Somme de Déméton (1550) = Déméton-O (1150) + Déméton-S (1152)
    data2 <-
      remplace_somme(code_somme = "1550",
                     vecteur_codes_a_sommer = c("1150", "1152"))



    # Métalaxyl (1706) (inclus les formes métalaxyl-M = Méfénoxam (2987) )
    data2 <- f_remplace_inclus_dedans("2987", "1706")

    # HCH alpha+beta+delta+gamma (5537) = HCH delta (1202) + HCH alpha (1200)+ HCH béta (1201)+ HCH gamma (lindane) (1203)
    data2 <-
      remplace_somme(
        code_somme = "5537",
        vecteur_codes_a_sommer = c("1200", "1201", "1202", "1203")
      )


    # Permethrin (somme)(1523)= Permethrin cis (5682) +Permethrin trans (5683)
    data2 <-
      remplace_somme(code_somme = "1523",
                     vecteur_codes_a_sommer = c("5682", "5683"))

    # Fenvalerate (1701) =  Fenvalerate RR (6606) + Fenvalerate RS (6607)
    data2 <-
      remplace_somme(code_somme = "1701",
                     vecteur_codes_a_sommer = c("6606", "6607"))


    # Esfenvalerate (1809)= Esfenvalerate SS (6608) +  Esfenvalerate SR (6609)
    data2 <-
      remplace_somme(code_somme = "1809",
                     vecteur_codes_a_sommer = c("6608", "6609"))

    # (Somme du Fenvalerate RR et Esfenvalerate SS ( 6613 ))= Fenvalerate RR (6606) + Esfenvalerate SS (6608)
    data2 <-
      remplace_somme(code_somme = "6613",
                     vecteur_codes_a_sommer = c("6606", "6608"))

    # (Somme du Fenvalerate RS et Esfenvalerate SR (6614)) = Fenvalerate RS (6607) + Esfenvalerate SR (6609)
    data2 <-
      remplace_somme(code_somme = "6614",
                     vecteur_codes_a_sommer = c("6607", "6609"))

    # (Somme du Fenvalerate et du Esfenvalerate 	(8592)) = Fenvalerate (1701) + Esfenvalerate (1809)
    data2 <-
      remplace_somme(code_somme = "8592",
                     vecteur_codes_a_sommer = c("1701", "1809"))


    # (Somme du Fenvalerate et du Esfenvalerate 	(8592)) = Somme du Fenvalerate RR et Esfenvalerate SS ( 6613 ) + Somme du Fenvalerate RS et Esfenvalerate SR (6614)
    data2 <-
      remplace_somme(code_somme = "8592",
                     vecteur_codes_a_sommer = c("6613", "6614"))

    # (Somme du Fenvalerate et du Esfenvalerate 	(8592)) =  Fenvalerate RR (6606) + Esfenvalerate SS (6608) + Fenvalerate RS (6607) + Esfenvalerate SR (6609)
    data2 <-
      remplace_somme(
        code_somme = "8592",
        vecteur_codes_a_sommer = c("6606", "6608", "6607", "6609")
      )


    # Chlordane (cis + trans) (1132) = chlordane alpha (7010) +  chlordane bêta (1757)
    data2 <-
      remplace_somme(code_somme = "1132",
                     vecteur_codes_a_sommer = c("7010", "1757"))

    # Demeton O+S (1550) = déméton-O (code Sandre n°1150) + déméton-S (code Sandre n°1152)
    data2 <-
      remplace_somme(code_somme = "1550",
                     vecteur_codes_a_sommer = c("1150", "1152"))

    # Heptachlore époxyde (cis+trans) (1198) = Heptachlore époxyde exo cis (1748) + Heptachlore époxyde endo trans (1749)
    data2 <-
      remplace_somme(code_somme = "1198",
                     vecteur_codes_a_sommer = c("1748", "1749"))

    # Spinosad (A+D) (spinosyne) (5610) = Spinosad A (7438) + Spinosad D (7439)
    data2 <-
      remplace_somme(code_somme = "5610",
                     vecteur_codes_a_sommer = c("7438", "7439"))




    # calcul de la somme de pesticides
    data2 <-
      data2 %>% mutate(par_6276 = select(., starts_with('par_')) %>% rowSums(na.rm =
                                                                               T))

    # si option resultat_seul, on supprime toutes les colonnes intermédiaires
    if (resultat_seul) {
      data2 <-
        data2[, c("CdStationMesureEauxSurface",
                  "DatePrel",
                  "CdUniteMesure",
                  "par_6276")]

      data2$CdSupport <- "3"
      data2$CdFractionAnalysee <- "23"
      data2$CdRqAna <- ifelse(data2$par_6276 == 0, "10", "1")
      data2$LqAna <- 0

    }

    # on remet le tableau de resultat au format long pour faciliter les aggrégations avec les autres données et l'usage des ggplot
    data2 <-
      data2 %>% pivot_longer(
        names_to = 'CdParametre',
        cols = starts_with('par_'),
        names_prefix = "par_",
        values_to = "RsAna",
        values_drop_na = T
      )



    return(data2)
  }
