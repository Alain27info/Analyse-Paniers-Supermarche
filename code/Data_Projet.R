
# Téléchargement et installation de la libraire 'arules'
install.packages("arules")

# Activation de la libraire 'arules'
library(arules)
library(arulesViz)

#Importation des données
basket=read.csv("Data Projet.csv",stringsAsFactors=T)

#affichage des nombres de lignes (observations) et colonnes (variables)
dim(basket)

#Resumé de la structure du data frame basket 
str(basket)

#Résumé statistique de la matrice de données 
summary(basket)





########################################################################################################
# Extraction des itemset fréquents représentant les articles les plus fréquemment achtetés simultanément
#########################################################################################################
# Création du data frame 'basket2' par sélection des colonnes numéros 8 à 25 de basket
basket2 <- basket[,c(8:25)]

dim(basket2) # affiche 2000  18

str(basket2)

summary(basket2)

names(basket2)

#basketF représente notre matrice de données ou les variables articles sont sous la forme facteur 
basketF <- as.data.frame(
  lapply(basket2, function(x) factor(x, levels = c(0,1)))
)
#On transforme les "0" en NA 
basketF[basketF == "0"] <- NA

# Mise à jour des valeurs possibles ('levels') pour chaque variable de 'basketF'
basketF <- data.frame(lapply(basketF, droplevels)) 


#------------------------------------
# AFFICHAGE DES FREQUENCES DES ITEMS 
#------------------------------------

# Representation au format transactionnel dans basketT necessaire pour appliquer itemFrequencyPlot()

basketT <- as(basketF, "transactions")


summary(basketT)

dimnames(basketT)

#affichae le nombre d'items dans basketT
nitems(basketT)

#affichage de la fréquence pour chaque variable article de basket
itemFrequencyPlot(basketT, col=rainbow(nitems(basketT)))



#extraction des itemsets frequents pour minsupport=10%
fi <-apriori(basketT, parameter=list(supp=0.1, target="frequent itemsets"))
 #ici on applique apriori sur objet de type transactions car valeurs de type numerique de base
summary(fi)

inspect(fi)

# Tri des itemsets fréquents par ordre décroissant des Supports
fi <- sort(fi, by="support")
inspect(fi)

# Histogramme d'effectifs des itemsets fréquents par taille d'itemset
barplot(table(size(fi)), xlab="Taille itemset", ylab="Nombre")


# Visualisation graphique sous forme de graphe
plot(fi, method = "graph") 

# ce que le prof demande
# Visualisation graphique interactive
plot(fi, method = "graph", engine = "interactive") 



############################################################
# EXTRACTION DE REGLES D'ASSOCIATION ENTRE ARTICLES ACHETES
############################################################


# Extraction des règles d'association pour minsupport=10% et minconfiance=50%
rules1 <- apriori(basketT, parameter = list(supp = 0.1, conf = 0.5, target ="rules"))

# Affichage textuel des règles générées
inspect(rules1)

# Pour afficher les valeurs numériques avec trois chiffres significatifs seulement
options(digits=3)
inspect(rules1)

# Tri par Confiance décroissante puis affichage
rules1 <- sort(rules1, by="confidence")
inspect(rules1)


# Visualisation graphique sous forme de matrice
plot(rules1, method = "matrix", measure = "confidence")

# Visualisation type boulier(scatter plot)

plot(rules1, method = "scatterplot",
     measure = c("support", "confidence"),
     shading = "lift")

# Visualisation sous forme de coordonnées parallèles 
plot(rules1, method = "paracoord")


#visualisation sous forme de graphe interactive
plot(rules1, method = "graph", interactive = TRUE)

#########################################################
# EXTRACTION DE REGLES D'ASSOCIATION DES ACHATS PAR GENRE
#########################################################
#suppression des genres non attribués en NA 
basket$Genre[basket$Genre =="-"] <- NA

#mise a jour des levels de basket genre
basket$Genre <- droplevels(basket$Genre)

summary(basket$Genre)

#Mise à jour de basketF en ajoutant les variables genre
basketF$Genre <- basket$Genre

summary(basketF)

basketT <- as(basketF, "transactions")


#### Extraction pour genre Homme ##############
# Extraction pour minsupport=10% et minconfiance=40%  contenant au moins 2 items et 'Genre=Homme' en antécédent
rules_homme_lhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.4,target ="rules", minlen=2),
                          appearance=list(lhs="Genre=Homme"))
inspect(sort(rules_homme_lhs, by="support"))

# Extraction pour minsupport=10% et minconfiance=50%  contenant au moins 2 items et 'Genre=Homme' en conséquence
rules_homme_rhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.5,target ="rules", minlen=2),
                  appearance=list(rhs="Genre=Homme"))
inspect(sort(rules_homme_rhs, by="support"))





#### Extraction pour genre Femme ##############
# Extraction pour minsupport=10% et minconfiance=30%  contenant au moins 2 items et 'Genre=Femme' en antécédent
rules_femme_lhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.3,target ="rules", minlen=2),
                          appearance=list(lhs="Genre=Femme"))
inspect(sort(rules_femme_lhs, by="support"))

# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'Genre=Femme' en conséquence
rules_femme_rhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.3,target ="rules", minlen=2),
                          appearance=list(rhs="Genre=Femme"))
inspect(sort(rules_femme_rhs, by="support"))



#########################################################
# EXTRACTION DE REGLES D'ASSOCIATION DES ACHATS PAR AGE
#########################################################
# discretisation de l'age
#on supprimes les ages irréalistes
basket$Age[basket$Age > 100] <- NA 

quantile(basket$Age, probs = c(0, 1/3, 2/3, 1),na.rm=TRUE)

#  on a categorise les ages en trois
basket$Age_cat <- cut(
  basket$Age,
  breaks = quantile(basket$Age, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Jeune", "Adulte","Vieux"), 
  include.lowest = TRUE)

summary(basket$Age_cat) 


# Suppression de la variable 'Genre' du data frame 'basketF'
basketF <- basketF[, colnames(basketF) != "Genre"]

#Mise à jour de basketF en ajoutant les variables age
basketF$Age_cat <- basket$Age_cat

summary(basketF)

basketT <- as(basketF, "transactions")


#### Extraction pour jeune ##############
# Extraction pour minsupport=15% et minconfiance=10%  contenant au moins 2 items et 'jeune' en antécédent
rules_jeune_lhs<- apriori(basketT, parameter = list(supp=0.15,conf=0.1,target ="rules", minlen=2),
                          appearance=list(lhs="Age_cat=Jeune"))
inspect(sort(rules_jeune_lhs, by="support"))

# Extraction pour minsupport=15% et minconfiance=10%  contenant au moins 2 items et 'jeune' en conséquence
rules_Jeune_rhs<- apriori(basketT, parameter = list(supp=0.15,conf=0.1,target ="rules", minlen=2),
                          appearance=list(rhs="Age_cat=Jeune"))
inspect(sort(rules_Jeune_rhs, by="support"))

#### Extraction pour adulte ##############
# Extraction pour minsupport=15% et minconfiance=10%  contenant au moins 2 items et 'Adulte' en antécédent
rules_adulte_lhs<- apriori(basketT, parameter = list(supp=0.15,conf=0.1,target ="rules", minlen=2),
                          appearance=list(lhs="Age_cat=Adulte"))
inspect(sort(rules_adulte_lhs, by="support"))

# Extraction pour minsupport=15% et minconfiance=10%  contenant au moins 2 items et 'Adulte' en conséquence
rules_Adulte_rhs<- apriori(basketT, parameter = list(supp=0.15,conf=0.1,target ="rules", minlen=2),
                          appearance=list(rhs="Age_cat=Adulte"))
inspect(sort(rules_Adulte_rhs, by="support"))


#### Extraction pour Vieux ##############
# Extraction pour minsupport=15% et minconfiance=10%  contenant au moins 2 items et 'Vieux' en antécédent
rules_Vieux_lhs<- apriori(basketT, parameter = list(supp=0.15,conf=0.1,target ="rules", minlen=2),
                          appearance=list(lhs="Age_cat=Vieux"))
inspect(sort(rules_Vieux_lhs, by="support"))

# Extraction pour minsupport=15% et minconfiance=10%  contenant au moins 2 items et 'vieux' en conséquence
rules_Vieux_rhs<- apriori(basketT, parameter = list(supp=0.15,conf=0.1,target ="rules", minlen=2),
                          appearance=list(rhs="Age_cat=Vieux"))
inspect(sort(rules_Vieux_rhs, by="support"))



#########################################################
# EXTRACTION DE REGLES D'ASSOCIATION DES ACHATS PAR REVENU
#########################################################
#Definition des categories de revenus 
quantile(basket$Revenu, probs = c(0, 1/3, 2/3, 1))

basket$Revenu_cat <- cut(
  basket$Revenu,
  breaks = quantile(basket$Revenu, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("faible", "median", "élevé"),
  include.lowest = TRUE
)

summary(basket$Revenu_cat)


# Suppression de la variable 'Age' du data frame 'basketF'
basketF <- basketF[, colnames(basketF) != "Age_cat"]

#Mise à jour de basketF en ajoutant les variables Revenu
basketF$Revenu_cat <- basket$Revenu_cat

summary(basketF)

basketT <- as(basketF, "transactions")


#### Extraction pour revenu_faible ##############
# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'revenu_cat=faible' en antécédent
rules_revenu_faible_lhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.1,target ="rules", minlen=2),
                          appearance=list(lhs="Revenu_cat=faible"))
inspect(sort(rules_revenu_faible_lhs, by="support"))

# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'revenu_cat=faible' en conséquence
rules_revenu_faible_rhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.1,target ="rules", minlen=2),
                          appearance=list(rhs="Revenu_cat=faible"))
inspect(sort(rules_revenu_faible_rhs, by="support"))



#### Extraction pour revenu_median ##############
# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'revenu_cat=median' en antécédent
rules_revenu_median_lhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.1,target ="rules", minlen=2),
                           appearance=list(lhs="Revenu_cat=median"))
inspect(sort(rules_revenu_median_lhs, by="support"))
#ici le support maximal est de 0.1330 on diminue donc le support afin de voir les règles d'association présents

# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'revenu_cat=median' en conséquence
rules_revenu_median_rhs<- apriori(basketT, parameter = list(supp=0.10,conf=0.1,target ="rules", minlen=2),
                                  appearance=list(rhs="Revenu_cat=median"))
inspect(sort(rules_revenu_median_rhs, by="support"))



#### Extraction pour revenu_élévé ##############
# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'revenu_cat=élevé' en antécédent
rules_revenu_élevé_lhs<- apriori(basketT, parameter = list(supp=0.1,conf=0.1,target ="rules", minlen=2),
                                  appearance=list(lhs="Revenu_cat=élevé"))
inspect(sort(rules_revenu_élevé_lhs, by="support"))

# Extraction pour minsupport=10% et minconfiance=10%  contenant au moins 2 items et 'revenu_cat=élevé' en conséquence
rules_revenu_élevé_rhs<- apriori(basketT, parameter = list(supp=0.10,conf=0.1,target ="rules", minlen=2),
                                  appearance=list(rhs="Revenu_cat=élevé"))
inspect(sort(rules_revenu_élevé_rhs, by="support"))


# Suppression de la variable 'Revenu_cat' du data frame 'basketF'
basketF <- basketF[, colnames(basketF) != "Revenu_cat"]


################################################################################################################
####Extraction des items set fréquents indiquant quels sont les achats le plus fréquemment achetés dans les 
#10% de transactions dont le montant est le plus élevé
##############################################################################################################

seuil <- quantile(basket$Montant,0.9, na.rm=TRUE)

# sélection des articles correspondant à ces transactions
basketF_10 <- basketF[basket$Montant >= seuil, ]

# transformation en transactions (articles uniquement)
basketF_10_T <- as(basketF_10, "transactions")

str(basketF_10_T)

itemLabels(basketF_10_T)  #permet de verifier les items qui sont dans transactions #


# extraction des items sets frequents 
#extraction des itemsets frequents pour minsupport=10%

fi_10 <-apriori(basketF_10_T, parameter=list(supp=0.1, target="frequent itemsets", minlen=4,maxlen=4))
#ici on applique apriori sur objet de type transactions car valeurs de type numerique de base
summary(fi_10)

inspect(sort(fi_10,by="support"))

#Dans les 10 % de transactions ayant le montant le plus élevé, les articles Poissonn, Pommes ,Salade et Yaourt sont fréquemment achetés ensemble 


###############################################################
#Evaluation des clusters
#################################################################
# Téléchargement et installation de la libraire 'clustMixType'
install.packages("clustMixType")

# Activation de la libraire 'clustMixType'
library(clustMixType)
# Affichage des valeurs numériques avec 2 décimales 
options(digits=3) 

# Définition de la suite de tirages aléatoires pour la reproductibilité des résultats
set.seed(100)

#--------------------------
# Extraction de 3 clusters
#--------------------------

# Extraction de 3 clusters par la fonction kmeans()
km_3 <- kmeans(basket2, centers = 3)

# Affichage de l'objet 'km_3' généré
km_3

# Effectifs (nombre d'instances) des clusters extraits
km_3$size

# Caractérisation (valeurs moyennes/modes des variables) des clusters extraits
View(format(km_3$centers, digits=3))

# Extraction de 4 clusters par la fonction kmeans()
km_4 <- kmeans(basket2, centers = 4)

# Affichage de l'objet 'km_4' généré
km_4

# Effectifs (nombre d'instances) des clusters extraits
km_4$size

# Caractérisation (valeurs moyennes/modes des variables) des clusters extraits
View(format(km_4$centers, digits=3))

# Extraction de 4 clusters par la fonction kmeans()
km_6<- kmeans(basket2, centers = 6)

# Caractérisation (valeurs moyennes/modes des variables) des clusters extraits
View(format(km_6$centers, digits=3))


####################################
# Évaluation du nombre de clusters #
####################################

#----------------------------------------
# Courbe Elbow avec weigthed squared sum
#----------------------------------------
install.packages("factoextra")
library(factoextra)

fviz_nbclust(basket2, kmeans, method = "wss", print.summary = T, linecolor = "black")  + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18), plot.title = element_text(size = 16)) + 
  geom_line(aes(group = 1), color = "steelblue", linewidth = 1.5) + 
  geom_point(group = 1, size = 5, color = "steelblue")

#---------------------------------------
# Courbe des coefficients de Silhouette
#---------------------------------------

fviz_nbclust(basket2, kmeans, method = "silhouette", print.summary = T, linecolor = "black")  + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18), plot.title = element_text(size = 16)) + 
  geom_line(aes(group = 1), color = "steelblue", linewidth = 1.5) + 
  geom_point(group = 1, size = 5, color = "steelblue")

#-------------------------
# Courbe de Gap statistic
#-------------------------

fviz_nbclust(basket2, kmeans, method = "gap_stat", print.summary = T, linecolor = "black")  + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18), plot.title = element_text(size = 18)) + 
  geom_line(aes(group = 1), color = "steelblue", linewidth = 1.5) + 
  geom_point(group = 1, size = 5, color = "steelblue")



#Visualisation des clusters
# On utilise l'objet 'km_6' car nous avons choisi k=6

basket2$cluster <- km_6$cluster

# On récupère les centres des clusters (les moyennes d'achat)
centres <- km_6$centers

# On crée une Heatmap simple
# Rowv=NA et Colv=NA empêchent R de tout mélanger (on garde l'ordre des clusters)
heatmap(as.matrix(centres),
        scale = "column",       # On compare les colonnes entre elles
        col = cm.colors(256),   # Palette de couleurs (Cyan à Magenta)
        Rowv = NA, Colv = NA,
        main = "Heatmap : Intensité des achats par Cluster",
        ylab = "Cluster ID",
        xlab = "Produits")




#Caractérisation des clusters extraits (Profilage Socio-Démographique)

str(basket)


# Suppression de la variable 'Age' du data frame 'basket'
basket <- basket[, colnames(basket) != "Age"]


# Suppression de la variable 'Revenus' du data frame 'basket'
basket <- basket[, colnames(basket) != "Revenus"]


str(basket)

library(dplyr)

df_profil <- basket
df_profil$Cluster <- km_6$cluster



get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 4. Calcul des Moyennes et Modes par Cluster
# On groupe par cluster et on calcule les stats demandées
synthese_clusters <- df_profil %>%
  group_by(Cluster) %>%
  summarise(
    # --- Variables Numériques (Moyennes) ---
  
    Moyenne_Enfants = mean(Nb_enfants, na.rm = TRUE),
    Moyenne_Montant = mean(Montant, na.rm = TRUE),
    # --- LES PROFILS  ---
    
    Classe_Age_Dominante    = get_mode(Age_cat),      
    Classe_Revenu_Dominante = get_mode(Revenu_cat),
    
    # --- Variables Discrètes (Modes) ---
    Mode_Genre      = get_mode(Genre),
    Mode_Statut     = get_mode(Statut_marital),
    
    
    # --- Taille du groupe ---
    Effectif        = n()
  )

# Affichage du résultat
View(synthese_clusters)