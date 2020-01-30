##Importation de données
#library(here)
#setwd(here())
contrat <- read.csv(file.choose(), sep=";")
#contrat <- read.csv("contratUdM.txt", sep=";")
View(contrat)
summary(contrat)
str(contrat)
sinistre<- read.csv(file.choose(), sep=";")
#sinistre<- read.csv("sinistreUdM.txt", sep=";")
View(sinistre)
summary(sinistre)
str(sinistre)
#sinistre=sinistre[sinistre$cout>0,]#laisser que les couts positives

##Traiter une seule garantie
sinistre=sinistre[sinistre$garantie=="1RC",]

##Frequence des contrats dans la table sinistre
Freq_contrat<-table(sinistre$nocontrat)

View(Freq_contrat)
#Convertir Names(T)=Nocotract en type numerique
header_Freq_contrat<-as.numeric(names(Freq_contrat))
#Convertir en type numérique les freq 
Valeurs_Freq_contrat<-as.numeric(Freq_contrat)

#Combiner les deux vecteurs en une seule data frame : nombre1 est une data frame qui contient le nombre de sinistres pour chaque contrat
Freq_contrat <- data.frame(nocontrat=header_Freq_contrat,nbre=Valeurs_Freq_contrat)

#Créer une variable boléenne qui teste l'existence des Numeros de contrat de la table contrat dans la table T1(T1:les numéros de contrat distincts de la table sinistres) )
I <- contrat$nocontrat%in%header_Freq_contrat 
# liste des contrats sans aucun sinistre
View(I)

#Extraire les contrats avec 0 sinistres et former la table aucun sinistre
header_Freq_contrat<-contrat$nocontrat[I==FALSE]

aucun_sinistre <- data.frame(nocontrat=header_Freq_contrat,nbre=0)

#Combiner les contats non sinistrés et les contrats sinistrés 
nombre<-rbind(Freq_contrat,aucun_sinistre)

#Joindre la table contrat avec la table nombre (Rq: dans les cas simples : la fonction merge trouve l'intersection entre les deux tables )
baseFREQ <- merge(contrat,nombre)
View(baseFREQ)

#Former la table baseCOUT : trouver le cout de chaque contrat sinistré 
baseCOUT <- merge(sinistre,contrat)
View(baseCOUT)








