
##Construction de la base de modelisation des couts Moyen :
basetarif<-merge(baseFREQ,baseCOU)
View(basetarif)

##Garder que les couts posiitifs
basetarif=basetarif[basetarif$cout >0 , ]
##Calculer la variable cout moyen par nbre de sinistre
basetarif$CM<-ifelse(basetarif$nbre ==0,basetarif$cout,basetarif$CM<-basetarif$cout/basetarif$nbre) 

#Chercher la loi des couts
cout <- basetarif$CM
plot(hist(cout, breaks=500))

library(fitdistrplus)
##### Ajustement avec la loi lognormal #####
fit_params_lognormal <- fitdistr(cout,"lognormal")
quants <-seq(0,1,length=81)[2:80]
# Find quantiles for the fitted distribution
fit_quants <- qlnorm(quants,fit_params_lognormal$estimate['meanlog'], fit_params_lognormal$estimate['sdlog'])
# Find quantiles of the original data
data_quants <- quantile(cout,quants)

# Fit and data quantiles side by side
data.frame(fit_quants,data_quants)
plot(fit_quants, data_quants, xlab="Theoretical Quantiles", ylab="Sample Quantiles")
title(main = "Q-Q plot of lognormal fit against data")
abline(0,1)








#######Ajustement avec la loi Gamma #######
fit_params_gamma <- fitdistr(cout/500000,"gamma")
quants <-seq(0,1,length=81)[2:80]

# Find quantiles for the fitted distribution
fit_quants <- qgamma(quants,fit_params_gamma$estimate['rate'], fit_params_gamma$estimate['shape'])

# Find quantiles of the original data
data_quants <- quantile(cout/500000,quants)

#Fit and data quantiles side by side
data.frame(fit_quants,data_quants)
plot(fit_quants, data_quants, xlab="Theoretical Quantiles", ylab="Sample Quantiles")
title(main = "Q-Q plot of gamma fit against data")
abline(0,1)






####### Fit to exponentiel #######
fit_params_exp<- fitdistr(cout,"exponential")
quants <-seq(0,1,length=81)[2:80]
##Definition quantile :les quantiles sont les valeurs qui divisent un jeu de donnees en intervalles contenant le même nombre de donnees.
# Find quantiles for the fitted distribution
fit_quants <- qexp(quants,fit_params_exp$estimate['rate'])

# Find quantiles of the original data
data_quants <- quantile(cout,quants)

# Fit and data quantiles side by side
data.frame(fit_quants,data_quants)
plot(fit_quants, data_quants, xlab="Theoretical Quantiles", ylab="Sample Quantiles")
title(main = "Q-Q plot of exponential fit against data")
abline(0,1)
# 





# #### Tester les resultats par les test statistiques de conformite de Kolmogrov-Smirnov, Cramer Von Mises et Anderson-Darling
# ##### Test de kolmogorov-Smirnov 

ks.test(cout,"pgamma", fit_params_gamma$estimate['rate'], fit_params_gamma$estimate['shape'] )
##Interpretation

# si lhypothese nulle est vraie (ici H0 : {Fn = Fth}), la probabilite de voir la statistique
#de test (D = 0.8608) autant eloignee de la valeur 0 est inferieure a <2.2e-16, soit une faible probabilite.
#Autrement dit, la valeur de D est anormalement grande et donc on ne peut pas accepter l’hypothese nulle.
#Ceci confirme donc que la loi de probabilite de "cout" n est pas celle d une loi normale.

ks.test(cout,"plnorm",fit_params_lognormal$estimate['sdlog'],fit_params_lognormal$estimate['meanlog']) 

ks.test(cout,"pexp",fit_params_exp$estimate['rate'] )


###Test de Cramer Von-mises (Goodness of fit test)
library(goftest)
cvm.test(cout, "plnorm", fit_params_lognormal$estimate['sdlog'],fit_params_lognormal$estimate['meanlog'])
cvm.test(cout, "pgamma" ,fit_params_gamma$estimate['rate'], fit_params_gamma$estimate['shape'])
cvm.test(cout,"pexp",fit_params_exp$estimate['rate']  )


#### GLM COUT

regression_cout<- glm(CM~marque+region+carburant,data=basetarif,family=Gamma(link="log"))
summary(regression_cout) #On trouve la valeur AIC: 29479
