# Importation du jeu de données
# Stockage dans le dataframe "capacity"
capacity <- read.table("capacity_data_r1.csv",
                      header=TRUE, sep=",", fileEncoding="utf-8")

summary(capacity)

# Pour afficher les titres des colonnes.
colnames(capacity)
# On peut ensuite afficher les modalités de la colonne "prof"
levels(capacity$prof)
# Et pour voir le nombre de réponses pour chaque modalités de "prof"
summary(capacity$prof)

# Selection des noms des colonnes à conserver pour une première ACM
cols_to_keep  <- c("sexe", "age_rec", "prof","internaute", "WIP.outil.pc", "WIP.outil.smart",
                   "WIP.outil.tabl", "WIP.outil.consol", "WIP.outil.tv")
# Création d'un nouveau dataframe "select" à partir de "capacity"
select = capacity[,cols_to_keep]

# Chargement de FactoMineR
library(FactoMineR)

# Premier ACM à partir de "select"
res <- MCA(select, quali.sup=c(1:4))
# Résumé des résultats sur les 3 premières composantes
summary(res, ncp=3, nbelements=Inf)

# Le graphe est très étrange, une grande partie des individus sont alignés le long d'une ligne parallèle
# au deuxième axe...
# La réponse rdr (refuse de répondre) influe trop sur le graphe.
# On peut voir le nombre de rdr, nsp (ne sais pas), adr (absence de réponse) et les champs vides avec summary :
summary(select, maxsum=20)  # maxsum permet d'afficher toutes les modalités, sinon summary se limite à 7 modalités

# Comme Laurent, je choisi arbitrairement de transformer ces réponses en NA.
select$WIP.outil.pc[select$WIP.outil.pc == ''] = NA
select$WIP.outil.pc[select$WIP.outil.pc == 'nsp'] = NA
select$WIP.outil.pc[select$WIP.outil.pc == 'rdr'] = NA
select$WIP.outil.pc[select$WIP.outil.pc == 'adr'] = NA

# Plus simple encore
select[select[] == ''] = NA
select[select[] == 'nsp'] = NA
select[select[] == 'rdr'] = NA
select[select[] == 'adr'] = NA

# On relance l'ACM
res <- MCA(select, quali.sup=c(1:4))
# Résumé des résultats sur les 3 premières composantes
summary(res, ncp=3, nbelements=Inf)

# On rajoute la Q15 pour voir si ça change la tête du graphe.
cols_to_keep  <- c("sexe", "age_rec", "prof","internaute", "WIP.outil.pc", "WIP.outil.smart",
                   "WIP.outil.tabl", "WIP.outil.consol", "WIP.outil.tv", "appren.info.reseau",
                   "appren.info.sup.ligne", "appren.info.sup.hrsligne", "appren.info.form", "appren.info.seul",
                   "appren.info.autre", "appren.info.sr")
# Création d'un nouveau dataframe "select" à partir de "capacity"
select = capacity[,cols_to_keep]

select[select[] == ''] = NA
select[select[] == 'nsp'] = NA
select[select[] == 'rdr'] = NA
select[select[] == 'adr'] = NA

res <- MCA(select, quali.sup=c(1:4))

