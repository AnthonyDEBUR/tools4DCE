

# base des seuils par paramètre
base_seuils <- read.csv2("tables_ref/base_seuils.csv", colClasses = c(rep("character",5),rep("numeric",2), rep("character",5)))

unique(base_seuils$TYPE)

save(base_seuils, file="data/base_seuils.RData")


# couleur des classes de qualité
couleurs_classes <- read.csv2("tables_ref/couleurs_classes.csv")
save(couleurs_classes, file="data/couleurs_classes.RData")


# ordre des facteurs de qualité
ordre_facteurs_qualite <- read.csv2("tables_ref/ordre_facteurs_qualite.csv")
names(ordre_facteurs_qualite)<-"CLASSE"
save(ordre_facteurs_qualite, file="data/ordre_facteurs_qualite.RData")
