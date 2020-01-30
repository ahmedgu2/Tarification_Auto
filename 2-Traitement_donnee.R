##Detection des anomalies
summary(baseCOUT$cout)
#Eliminer les donnEes qui prEsentent un cout nEgative
baseCOUT=baseCOUT[!(baseCOUT$cout <0 ), ]
#Eliminer les donnEes qui prEsentent un age de vEhicule Egale a 69 ans
baseCOUT=baseCOUT[!(baseCOUT$agevehicule >=69 ), ]
#supprimer la deuxieme variable 
baseCOUT=baseCOUT[,-2]

View(baseCOUT)
#Tester l'existence des redondances
n_occur <- data.frame(table(baseCOUT$nocontrat))
n_occur[n_occur$Freq > 1,]
#baseCOUT[!duplicated(baseCOUT), ] 
###Traiter les redondances
#Somme de cout par contrat
baseCOU<-aggregate(baseCOUT$cout, by=list(Category=baseCOUT$nocontrat), FUN=sum)
#dEnomination des noms des variables
baseCOU <- data.frame(nocontrat=baseCOU$Category,cout=baseCOU$x)
View(baseCOU)




##Ajouter les colonnes a la base des contrats uniques
#la commande match renvoie un vecteur des positions des (premières) correspondances de son premier argument dans son second.
baseCOU$exposition <- baseCOUT$exposition[match(baseCOU$nocontrat,baseCOUT$nocontrat)] 
baseCOU$zone <- baseCOUT$zone[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$puissance <- baseCOUT$puissance[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$agevehicule<- baseCOUT$agevehicule[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$ageconducteur <- baseCOUT$ageconducteur[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$bonus <- baseCOUT$bonus[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$marque <- baseCOUT$marque[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$carburant<- baseCOUT$carburant[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$densite <- baseCOUT$densite[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
baseCOU$region <- baseCOUT$region[match(baseCOU$nocontrat,baseCOUT$nocontrat)]
View(baseCOU)

##Deplacer les valeurs de la colonne marque dans la baseCOU

# baseCOU$marque[baseCOU$marque == 1] <- "Renault/Nissan"
# baseCOU$marque[baseCOU$marque == 2] <- "Peugeot Citroën"
# baseCOU$marque[baseCOU$marque == 3] <- "Volkswagen Audi Skoda Seat"
# baseCOU$marque[baseCOU$marque == 4] <- "Opel GM"
# baseCOU$marque[baseCOU$marque == 5] <- "Ford "
# baseCOU$marque[baseCOU$marque == 6] <- " Fiat"
# baseCOU$marque[baseCOU$marque == 10] <- " Mercedes Chrysler"
# baseCOU$marque[baseCOU$marque == 11] <- "BMW Mini"
# baseCOU$marque[baseCOU$marque == 12] <- "Autres japonaises et Core"
# baseCOU$marque[baseCOU$marque == 13] <- "Autres europEennes"
# baseCOU$marque[baseCOU$marque == 14] <- "Autres"




##DEcoder les modalitEs de la variable marque en classes (voiture_pop, marque_haute_gamme,marque_non_connue)
baseCOU$marque[baseCOU$marque == 1] <- "Voiture_pop"
baseCOU$marque[baseCOU$marque == 2] <- "Voiture_pop"
baseCOU$marque[baseCOU$marque == 3] <- "marque_haute_gamme"
baseCOU$marque[baseCOU$marque == 4] <- "Voiture_pop"
baseCOU$marque[baseCOU$marque == 5] <- "Voiture_pop"
baseCOU$marque[baseCOU$marque == 6] <- "Voiture_pop"
baseCOU$marque[baseCOU$marque == 10] <- "marque_haute_gamme"
baseCOU$marque[baseCOU$marque == 11] <- "marque_haute_gamme"
baseCOU$marque[baseCOU$marque == 12] <- "marque_non_connue"
baseCOU$marque[baseCOU$marque == 13] <- "marque_non_connue"
baseCOU$marque[baseCOU$marque == 14] <-"marque_non_connue"




##Deplacer les valeurs de la colonne densitE dans la baseCOU
#DEnomination des modalitEs de la variable region 
baseCOU$region[baseCOU$region == -1] <- 'R_-1'
baseCOU$region[baseCOU$region == 0] <- 'R_0'
baseCOU$region[baseCOU$region == 1] <- 'R_1'
baseCOU$region[baseCOU$region == 2] <- 'R_2'
baseCOU$region[baseCOU$region == 3] <- 'R_3'
baseCOU$region[baseCOU$region == 4] <- 'R_4'
baseCOU$region[baseCOU$region == 5] <- 'R_5'
baseCOU$region[baseCOU$region == 6] <- 'R_6'
baseCOU$region[baseCOU$region == 7] <- 'R_7'
baseCOU$region[baseCOU$region == 8] <- 'R_8'
baseCOU$region[baseCOU$region == 9] <- 'R_9'
baseCOU$region[baseCOU$region == 10] <- 'R_10'
baseCOU$region[baseCOU$region == 11] <- 'R_11'
baseCOU$region[baseCOU$region == 12] <- 'R_12'
baseCOU$region[baseCOU$region == 13] <- 'R_13'


View(baseCOU)

n_occur <- data.frame(table(baseFREQ$nocontrat))
n_occur[n_occur$Freq > 1,]



baseFREQ=baseFREQ[!(baseFREQ$agevehicule >68 ), ]
##Deplacer les valeurs de la colonne marque dans la baseFREQ  en classes (voiture_pop, marque_haute_gamme,marque_non_connue)
baseFREQ$marque[baseFREQ$marque == 1] <- "Voiture_pop"
baseFREQ$marque[baseFREQ$marque == 2] <- "Voiture_pop"
baseFREQ$marque[baseFREQ$marque == 3] <- "marque_haute_gamme"
baseFREQ$marque[baseFREQ$marque == 4] <- "Voiture_pop"
baseFREQ$marque[baseFREQ$marque == 5] <- "Voiture_pop"
baseFREQ$marque[baseFREQ$marque == 6] <- "Voiture_pop"
baseFREQ$marque[baseFREQ$marque == 10] <- " marque_haute_gamme"
baseFREQ$marque[baseFREQ$marque == 11] <- "marque_haute_gamme"
baseFREQ$marque[baseFREQ$marque == 12] <- "marque_non_connue"
baseFREQ$marque[baseFREQ$marque == 13] <- "marque_non_connue"
baseFREQ$marque[baseFREQ$marque == 14] <-"marque_non_connue"




##Deplacer les valeurs de la colonne densitE dans la basefreq
#DEnomination des modalitEs de la variable region 
baseFREQ$region[baseFREQ$region == -1] <- 'R_-1'
baseFREQ$region[baseFREQ$region == 0] <- 'R_0'
baseFREQ$region[baseFREQ$region == 1] <- 'R_1'
baseFREQ$region[baseFREQ$region == 2] <- 'R_2'
baseFREQ$region[baseFREQ$region == 3] <- 'R_3'
baseFREQ$region[baseFREQ$region == 4] <- 'R_4'
baseFREQ$region[baseFREQ$region == 5] <- 'R_5'
baseFREQ$region[baseFREQ$region == 6] <- 'R_6'
baseFREQ$region[baseFREQ$region == 7] <- 'R_7'
baseFREQ$region[baseFREQ$region == 8] <- 'R_8'
baseFREQ$region[baseFREQ$region == 9] <- 'R_9'
baseFREQ$region[baseFREQ$region == 10] <- 'R_10'
baseFREQ$region[baseFREQ$region == 11] <- 'R_11'
baseFREQ$region[baseFREQ$region == 12] <- 'R_12'
baseFREQ$region[baseFREQ$region == 13] <- 'R_13'

View(baseCOU)
View(baseFREQ)






