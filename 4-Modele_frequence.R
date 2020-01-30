

###############################################Determination de loi Nbre######################################
#Ajustement de la loi
library(vcd)
N <-baseFREQ$nbre
#hist(table(N), breaks=50)

###Ajustement avec poisson
gof <- goodfit(N,type= "poisson",method= "ML") 
plot(gof,main="ajustement avec la loi de poisson") #Ajustement des frequences de sinistres declarés par une loi de poisson

###Ajustement avec binomiale
gof1 <- goodfit(N,type= "binomial",method= "ML")
plot(gof1,main="ajustement avec la loi binomiale")#Ajustement des frequences de sinistres declarés par une loi binomiale

###Ajustement avec binomiale negative
gof2 <- goodfit(N,type= "nbinomial",method= "ML")
plot(gof2,main="ajustement avec la loi binomiale negative")#Ajustement des frequences de sinistres declarés par une loi binomiale négative

###Ajustement avec Zero-inflated-poisson
library(fitdistrplus) 
library(gamlss)  
library(emdbook) 
library(pscl)
fit_zip = fitdist(N, 'ZIP',start = list(mu = mean(N), sigma = sd(N)))
plot(fit_zip)

###Ajustement avec Zero-inflated-NB
fit_znb <- zeroinfl(N ~ 1|1,dist="negbin") 

 exp(coef(fit_znb)[1])  ## mu 
 fit_znb$theta          ## theta 
 plogis(coef(fit_znb)[2]) ## zprob 
 fitdistr(N,dzinbinom,start=list(mu= exp(coef(fit_znb)[1]) ,size=length(N),zprob= plogis(coef(fit_znb)[2])))

##Regression GLM
library(boot)
library(pscl)
seuils <- c(11,31,74,94) #seuils choisis  de la variable densite
seuils2<-c(18,26,40,50,80,100) #seuils choisis de la variable age

##diviser les variables en sous populations
baseFREQ$cutdensite <- cut(baseFREQ$densite,breaks=seuils,include.lowest=TRUE,right=F)
baseFREQ$cutAge <- cut(baseFREQ$ageconducteur,breaks=seuils2,include.lowest=TRUE,right=F)

#Fit zero-inflated regression models for count data via maximum likelihood.
regZI <- zeroinfl(nbre~cutAge+cutdensite |marque ,offset=exposition,data = baseFREQ,dist = "poisson",link="logit")
regZI<-summary(regZI)
regZI

##Modele zero
zero<-as.data.frame(regZI$coefficients[2])
##Modele Count
count<-as.data.frame(regZI$coefficients[1])

