######################################"Analyse graphique pour identifier la structure du portefeuille#######################








######################Histogramme des variables liees au bien assure###############


#Histogramme presentant la frequence de sinistres par marque de voiture
par(mfrow=c(3,3))
hist(baseCOUT$marque,
     main="Frequence par marque de voiture",
     xlab="Marque",
     freq = FALSE,
     border="black",
     col="red3",
     xlim=c(1,14),
     las=1,
     breaks=7)



base_T<-baseFREQ
##Barlot presentant le nombre de sinistres par marque de voiture
A<-aggregate(base_T$nbre, by=list(Category=base_T$marque), FUN=sum)
barplot(A$x, names.arg=A$Category, main ="Nbre par marque de voiture",ylab="Nbre Sinistres", xlab="Marque",col="red3")

base_T<-baseCOU
##Barlot presentant le cout de sinistres par marque de voiture
A<-aggregate(base_T$cout, by=list(Category=base_T$marque), FUN=sum)
barplot(A$x, names.arg=A$Category, main="Cout par marque de voiture" ,ylab="cout", xlab="Marque",col="red3")

#histogramme presentant la frequence de sinistres selon l'age de vehicule
hist(base_T$agevehicule, 
     main="Frequence selon l'age de vehicule", 
     xlab="Age Vehicule", 
     border="black", 
     col="red3",axes = F)
axis(2)
axis(1, at=seq(0,20, by=1), labels=seq(0,20, by=1))

   

##Barlot presentant le nombre de sinistres par age de voiture
base_T<-baseFREQ
A<-aggregate(base_T$nbre, by=list(Category=base_T$agevehicule), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Nbre par age de voiture", ylab="Nbre sinistres", xlab="Age Vehicule",col="red3")

##Barlot presentant le cout de sinistres par age de voiture
base_T<-baseCOU
A<-aggregate(base_T$cout, by=list(Category=base_T$agevehicule), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Cout par age de voiture", ylab="cout", xlab="Age Vehicule",col="red3")


##Histogramme presentant la repartition du nombre de sinistres par puissance de vehicule
base_T<-baseCOU
hist(base_T$puissance, 
     main="Nombre de sinistres selon puissance voiture", 
     xlab="puissance Vehicule", 
     border="black", 
     col="red3",
     
     las=1, 
     breaks=7)

##Barplot presentant le nombre de sinistres par puissance de vehicule
base_T<-baseFREQ
A<-aggregate(base_T$nbre, by=list(Category=base_T$puissance), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Nbre par puissance de voiture",ylab="Nbre sinistres", xlab="Puissance",col="red3")



##Barplot presentant le cout de sinistres par puissance de vehicule
base_T<-baseCOU
A<-aggregate(base_T$cout, by=list(Category=base_T$puissance), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Cout par puissance de voiture", ylab="cout", xlab="Puissance",col="red3")
































######################Histogramme des variables liees au assures###############

#Histogramme presentant la frequence de sinistres selon l'age du conducteur
base_T<-baseCOU
par(mfrow=c(3,3))

hist(base_T$ageconducteur, 
     main="Frequence selon l'age du conducteur", 
     xlab="age", 
     border="black", 
     col="red3"
)

#Barplot presentant le nombre de sinistres selon l'age du conducteur
base_T<-baseFREQ
A<-aggregate(base_T$nbre, by=list(Category=base_T$ageconducteur), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Nbre par age",ylab="Nbre sinistres", xlab="Age",col="red3")

#Barplot presentant le cout de sinistres selon l'age du conducteur
base_T<-baseCOU
A<-aggregate(base_T$cout, by=list(Category=base_T$ageconducteur), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Cout par Age", ylab="cout", xlab="Age",col="red3")


#Histogramme presentant la repartition de sinistres selon la region
base_T<-baseCOUT
hist(base_T$region, 
     main="Repartition selon la region", 
     xlab="region", 
     border="black", 
     col="red3",
     xlim=c(-1,13),
     las=1, 
     breaks=7)

#Barplot presentant le nombre de sinistres selon la region
base_T<-baseFREQ
A<-aggregate(base_T$nbre, by=list(Category=base_T$region), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Nbre par Region",ylab="Nbre sinistres", xlab="Region",col="red3")

#Barplot presentant le cout de sinistres par region
base_T<-baseCOU
A<-aggregate(base_T$cout, by=list(Category=base_T$region), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Cout par region", ylab="cout", xlab="Region",col="red3")


#Barplot presentant la frequence de sinistres par zone
base_T<-baseCOU
A<-table(base_T$zone)
barplot(A,main ="Frequence par zone",ylab="Frequence", xlab="zone",col="red3")


#Barplot presentant la frequence de sinistres par zone
base_T<-baseFREQ
A<-aggregate(base_T$nbre, by=list(Category=base_T$zone), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Nbre par zone",ylab="Nbre sinistres", xlab="zone",col="red3")


#Barplot presentant le cout de sinistres par zone
base_T<-baseCOU
A<-aggregate(base_T$cout, by=list(Category=base_T$zone), FUN=sum)
barplot(A$x, names.arg=A$Category,main ="Cout par zone", ylab="cout", xlab="zone",col="red3")

































##############Observer la frequence de sinistre################

###########Variable Zone########
par(mfrow=c(3,3))

##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition
####Observer le nbre de Sinistres par zone 
##Frequence de zones
Freq_nbre_zone<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$zone), length)
Freq_nbre_zone <- data.frame(zone=Freq_nbre_zone$Category,Nbre=Freq_nbre_zone$x)

###Somme des frequences par zone
frequence_par_zone<-aggregate(as.numeric(baseFREQ$frequence), by=list(Category=baseFREQ$zone), FUN=sum)
colnames(frequence_par_zone)<-c('zone','frequence')

####fusionner les deux data frames
freq_nbre_par_zone <- merge(Freq_nbre_zone,frequence_par_zone)  
freq_nbre_par_zone


####Calculer le nbre moyen par zone
freq_nbre_par_zone$NM<-frequence_par_zone$frequence/Freq_nbre_zone$Nbre
freq_nbre_par_zone$NM


####plot le nbre moyen par zone
plot(freq_nbre_par_zone$zone,freq_nbre_par_zone$NM,type="l",main ="Nbre moyen par zone",ylab="Nbre moyen", xlab="Zone",col='red3',axes=F)
xlabel <-seq(0,100,by=1)
axis(1,at = xlabel, las = 1)
axis(2)
box()






###########Variable Puissance##########

##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de puissance

Freq_nbre_puissance<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$puissance), length)
Freq_nbre_puissance <- data.frame(puissance=Freq_nbre_puissance$Category,Nbre=Freq_nbre_puissance$x)

###Somme des frequences par puissance
frequence_par_puissance<-aggregate(as.numeric(baseFREQ$frequence), by=list(Category=baseFREQ$puissance), FUN=sum)
colnames(frequence_par_puissance)<-c('puissance','frequence')

####fusionner les deux data frames(Age+frequence de l'age+cout)
freq_nbre_par_puissance <- merge(Freq_nbre_puissance,frequence_par_puissance)#by.y="Category",by.x="Age")   


####Calculer le nbre moyen par puissance
freq_nbre_par_puissance$NM<-frequence_par_puissance$frequence/Freq_nbre_puissance$Nbre

####plot le nbre moyen par puissance
plot(freq_nbre_par_puissance$puissance,freq_nbre_par_puissance$NM,type="l",main ="Frequence moyenne par puissance",ylab="Frequence moyenne", xlab="Puissance",col='red3',axes=F)
xlabel <-seq(0,100,by=1)
axis(1,at = xlabel, las = 1)
axis(2)
box()





###########Variable agevehicule###########

##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de agevehicule

Freq_nbre_ageV<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$agevehicule), length)
Freq_nbre_ageV <- data.frame(agev=Freq_nbre_ageV$Category,Nbre=Freq_nbre_ageV$x)

###Somme des frequences par agevehicule
frequence_par_ageV<-aggregate(as.numeric(baseFREQ$frequence), by=list(Category=baseFREQ$agevehicule), FUN=sum)
colnames(frequence_par_ageV)<-c('agev','frequence')

####fusionner les deux data frames
freq_nbre_par_ageV <- merge(Freq_nbre_ageV,frequence_par_ageV)  


####Calculer le nbre moyen par age de vehicule
freq_nbre_par_ageV$NM<-frequence_par_ageV$frequence/Freq_nbre_ageV$Nbre
colnames(freq_nbre_par_ageV)

####plot le nbre moyen par age de vehicule
plot(freq_nbre_par_ageV$agev,freq_nbre_par_ageV$NM,type="l",main ="Nbre moyen par age Vehicule",ylab="Nbre moyen", xlab="Age Vehicule",col='red3',axes=F)
xlabel <-seq(0,100,by=1)
axis(1,at = xlabel, las = 1)
axis(2)
box()




###########Variable age conducteur##########

##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de age de conducteur
Freq_nbre_ageC<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$ageconducteur), length)
Freq_nbre_ageC <- data.frame(agec=Freq_nbre_ageC$Category,Nbre=Freq_nbre_ageC$x)

###Somme des frequences par age du conducteur
frequence_par_ageC<-aggregate(as.numeric(baseFREQ$frequence), by=list(Category=baseFREQ$ageconducteur), FUN=sum)
colnames(frequence_par_ageC)<-c('agec','frequence')

####fusionner les deux data frames
freq_nbre_par_ageC <- merge(Freq_nbre_ageC,frequence_par_ageC)#by.y="Category",by.x="Age")   


####Calculer le nbre moyen par age du conducteur
freq_nbre_par_ageC$NM<-frequence_par_ageC$frequence/Freq_nbre_ageC$Nbre
colnames(freq_nbre_par_ageC)

####plot le nbre moyen par age du conducteur
plot(freq_nbre_par_ageC$agec,freq_nbre_par_ageC$NM,type="l",main ="Frequence moyenne des sinistres par age du conducteur",ylab="Frequence moyenne", xlab="Age conducteur",col='red3',axes=F)
xlabel <-seq(0,100,by=1)
axis(1,at = xlabel, las = 1)
axis(2)
box()




###########Variable marque###########
##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de marque
Freq_nbre_marque<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$marque), length)
Freq_nbre_marque <- data.frame(marque=Freq_nbre_marque$Category,Nbre=Freq_nbre_marque$x)

###Somme des frequences par marque
frequence_par_marque<-aggregate(as.numeric(baseFREQ$frequence), by=list(Category=baseFREQ$marque), FUN=sum)
colnames(frequence_par_marque)<-c('marque','frequence')

####fusionner les deux data frames
freq_nbre_par_marque <- merge(Freq_nbre_marque,frequence_par_marque)


####Calculer le nbre moyen par marque
freq_nbre_par_marque$NM<-frequence_par_marque$frequence/Freq_nbre_marque$Nbre
colnames(freq_nbre_par_marque)

####plot le nbre moyen par marque
barplot(freq_nbre_par_marque$NM, names.arg=freq_nbre_par_marque$marque,main ="Frequence moyenne par marque",ylab="Frequence moyenne", xlab="Marque",col='red3')




###########Variable carburant###########
##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de carburant
Freq_nbre_carburant<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$carburant), length)
Freq_nbre_carburant <- data.frame(carburant=Freq_nbre_carburant$Category,Nbre=Freq_nbre_carburant$x)

###Somme des frequences par carburant
frequence_par_carburant<-aggregate(as.numeric(baseFREQ$carburant), by=list(Category=baseFREQ$carburant), FUN=sum)
colnames(frequence_par_carburant)<-c('carburant','frequence')

####fusionner les deux data frames
freq_nbre_par_carburant<- merge(Freq_nbre_carburant,frequence_par_carburant)

####Calculer le nbre moyen par carburant
freq_nbre_par_carburant$NM<-frequence_par_carburant$frequence/Freq_nbre_carburant$Nbre
colnames(freq_nbre_par_carburant)

####plot le nbre moyen par carburant
barplot(freq_nbre_par_carburant$NM, names.arg=freq_nbre_par_carburant$marque,main ="Nbre moyen par carburant",ylab="Nbre moyen", xlab="Carburant",col='red3')





###########Variable densite###########
#### densite est la densite de population dans la commune ou habite le conducteur principal,zone : zone A B C D E ou F,
###selon la densite en nombre d'habitants par km2 de la commune de residence ,region : code Ã  2 chires donnant les 22 regions
##françaises (code INSEE)

##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de densite
Freq_nbre_densite<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$densite), length)
Freq_nbre_densite <- data.frame(densite=Freq_nbre_densite$Category,Nbre=Freq_nbre_densite$x)

###Somme des frequences par densite
frequence_par_densite<-aggregate(as.numeric(baseFREQ$densite), by=list(Category=baseFREQ$densite), FUN=sum)
colnames(frequence_par_densite)<-c('densite','frequence')

####fusionner les deux data frames
freq_nbre_par_densite<- merge(Freq_nbre_densite,frequence_par_densite)


####Calculer le nbre moyen par densite
freq_nbre_par_densite$NM<-frequence_par_densite$frequence/Freq_nbre_densite$Nbre
colnames(freq_nbre_par_densite)

####plot le nbre moyen par densite
barplot(freq_nbre_par_densite$NM, names.arg=freq_nbre_par_densite$densite,main ="Nbre moyen par densite",ylab="Nbre moyen", xlab="densite",col='red3')
xlabel <-seq(11,94,by=1)
axis(1,at = xlabel, las = 1)
axis(2)
box()




###########Variable region###########
##Creation de la variable Frequence
baseFREQ$frequence<-baseFREQ$nbre/baseFREQ$exposition

##Frequence de de regions
Freq_nbre_region<-aggregate(baseFREQ$nocontrat, by=list(Category=baseFREQ$region), length)
Freq_nbre_region<- data.frame(region=Freq_nbre_region$Category,Nbre=Freq_nbre_region$x)

###Somme des frequences par region

frequence_par_region<-aggregate(as.numeric(as.factor(baseFREQ$region)), by=list(Category=baseFREQ$region), FUN=sum)
colnames(frequence_par_region)<-c('region','frequence')

####fusionner les deux data frames
freq_nbre_par_region<- merge(Freq_nbre_region,frequence_par_region)

####Calculer le nbre moyen par region
freq_nbre_par_region$NM<-frequence_par_region$frequence/Freq_nbre_region$Nbre
colnames(freq_nbre_par_region)

####plot le nbre moyen par region 
barplot(freq_nbre_par_region$NM, names.arg=freq_nbre_par_region$region,main ="Nbre moyen par region",ylab="Nbre moyen", xlab="region",col='red3')




































#####################################################################################################################################
# 







































###############################Choix des variables Tarifaires : 11ere selection ##############################
#Test d'independance entre les variables
base_T<-baseFREQ
base_T$cat=base_T$nbre>0 ###creation de la variable CAT

View(base_T)

chisq.test(table(base_T$cat,base_T$agevehicule))
chisq.test(table(base_T$cat,base_T$agevehicule))$p.value   # 1.36408e-10 
#On rejette l'hypothese nulle d'independance : la survenance depend donc de l'age

chisq.test(table(base_T$cat,base_T$marque))
chisq.test(table(base_T$cat,base_T$marque))$p.value   # 8.873289e-24

chisq.test(table(base_T$cat,base_T$ageconducteur))
chisq.test(table(base_T$cat,base_T$ageconducteur))$p.value   #0.1448406
#On accepte l'hypothese nulle d'independance : la survenance ne depend pas de l'age du conducteur

chisq.test(table(base_T$cat,base_T$puissance))
chisq.test(table(base_T$cat,base_T$puissance))$p.value   # 7.402073e-06

chisq.test(table(base_T$cat,base_T$carburant))
chisq.test(table(base_T$cat,base_T$carburant))$p.value   #0.05723584
# pas de relation

chisq.test(table(base_T$cat,base_T$densite))
chisq.test(table(base_T$cat,base_T$densite))$p.value   #4.569934e-20

chisq.test(table(base_T$cat,base_T$region))
chisq.test(table(base_T$cat,base_T$region))$p.value   #1.951453e-38

chisq.test(table(base_T$cat,base_T$zone))
chisq.test(table(base_T$cat,base_T$zone))$p.value   #3.966835e-05



