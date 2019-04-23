####Atelier Méta-analyse 101####
#Atelier donné au 13e colloque du CEF, 3 mai 2019, Chicoutimi
#Par Emilie Champagne
#Note: Je ne suis pas une experte en statistique. Le code présenté ici permet d'explorer les fonctions de Metafor
#Consultez des livres, des articles et des statisticiens lorsque vous réalisez une méta-analyse!

####Package####

library(metafor)

##Metafor est supporté par un site comprenant des tonnes d'exemples (http://www.metafor-project.org/)

##Articles recommandés liés à ce package 
#Viechtbauer, W. 2010. Conducting meta-analyses in R with the metafor package. Journal of statistical software 36:1-48.
#Viechtbauer, W., J. A. López-López, J. Sánchez-Meca, and F. Marín-Martínez. 2015. A comparison of procedures to test for moderators in mixed-effects meta-regression models. Psychological Methods 20:360.

####Données####

##Extrait des données de 
#Champagne, E., J.-P. Tremblay, and S. D. Côté. 2016. Spatial extent of neighboring plants influences the strength of associational effects on mammal herbivory. Ecosphere 7:e01371.

Data <-read.table(file="Data.txt", header= TRUE, 
                  colClasses = c("factor", "factor", rep("numeric", 7)))

##Description du jeu de données
#Year: Année de publication de l'article
#Type_exp: Type d'étude (Observation ou Expérimental)
#Plot_size: Taille des parcelles étudiées
#Mean_C, N_C, SD_C: Moyenne, n et SD du contrôle (Dommage par des herbivore de la plante focale sans plante compagne)
#Mean_E, N_E, SD_E: Moyenne, n et SD du traitement (Dommage par des herbivore de la plante focale avec plante compagne)

##Il existe plusieurs autres jeux de données pour faire des tests
#Exemples de jeux de données: dat.bcg, dat.hine1989, dat.raudenbush1985...


####Taille d'effet####

#Calcul de la taille d'effet avec la fonction escalc
Data <- escalc(measure="SMD", m1i= Mean_C, m2i= Mean_E, sd1i= SD_C, sd2i= SD_E,
       n1i= N_C, n2i= N_E, data= Data)
#SMD est la formule de Hedges. Autres options possibles, voir avec ?escalc

summary(Data$yi)

####Analyses: modèle avec effet fixe####

rma(yi = yi, vi = vi, data = Data, method="FE")

####Analyses: modèle avec effet aléatoire####

rma(yi = yi, vi = vi, data = Data, test= "knha", method="REML") 

#Possibilité d'utiliser plusieurs méthodes d'estimation différentes. Voir ?rma
#Le choix de la méthode d'estimation peut influencer les résultats (Vietchbauer et al 2015)
#Voir ?rma pour articles de référence; Borenstein et al 2009 parle de DL ou REML. Je vous conseille de vous renseigner!
#Ajustement au test statistique choisi selon Vietchbauer et al (2015)

####Analyses: modèles avec modérateurs####

rma(yi = yi, vi = vi, mods = Plot_size, data = Data, method = "REML", test = "knha")
rma(yi ~ Plot_size, vi, data = Data, method = "REML", test = "knha")

rma(yi = yi, vi = vi, mods = ~Type_exp, data = Data, method = "REML", test = "knha")

mod <- rma(yi ~ Type_exp + Plot_size + Type_exp*Plot_size, vi, data = Data, method = "REML", test = "knha")

summary(mod) #Donne notamment des valeurs d'AIC, BIC et AICc
anova(mod) #Test statistique sur les modérateurs
