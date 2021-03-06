####Atelier Méta-analyse 101####
#Atelier donné virtuellement, 26 avril 2021, de chez moi, en collaboration avec le CEF et l'ISFORT
#Par Emilie Champagne
#Note: Je ne suis pas une experte en statistique. Le code présenté ici permet d'explorer les fonctions de Metafor
#Consultez des livres, des articles et des statisticiens lorsque vous réalisez une méta-analyse!

####Package metafor####

library(metafor)

##Metafor est supporté par un site comprenant des tonnes d'exemples (http://www.metafor-project.org/)
#De par la qualité du support technique et statistique, c'est mon choix habituel pour les analyses

##Articles recommandés liés à ce package 
#Viechtbauer, W. 2010. Conducting meta-analyses in R with the metafor package. Journal of statistical software 36:1-48.
#Viechtbauer, W., J. A. López-López, J. Sánchez-Meca, and F. Marín-Martínez. 2015. A comparison of procedures to test for moderators in mixed-effects meta-regression models. Psychological Methods 20:360.

####Données####

##Extrait des données de 
#Champagne, E., J.-P. Tremblay, and S. D. Côté. 2016. Spatial extent of neighboring plants influences the strength of associational effects on mammal herbivory. Ecosphere 7:e01371.

Data <- read.table( file = "Data.txt", header = TRUE, 
                    colClasses = c("factor", "factor", rep("numeric", 7)))

##Description du jeu de données
#Year: Année de publication de l'article
#Type_exp: Type d'étude (Observation ou Expérimental)
#Plot_size: Taille des parcelles étudiées
#Mean_C, N_C, SD_C: Moyenne, n et SD du contrôle (Dommage par des herbivore de la plante focale sans plante compagne)
#Mean_E, N_E, SD_E: Moyenne, n et SD du traitement (Dommage par des herbivore de la plante focale avec plante compagne)

##Jeux de données disponibles pour tests (inclut dans metafor): dat.bcg, dat.hine1989, dat.raudenbush1985...

####Calcul de taille d'effet####

#Calcul de la taille d'effet avec la fonction escalc
Data <- escalc( measure = "SMD", m1i = Mean_C, m2i = Mean_E, sd1i = SD_C, sd2i = SD_E,
       n1i = N_C, n2i = N_E, data = Data)
#SMD est la formule de Hedges avec variance hétérogènes. Autres options possibles, voir avec ?escalc

summary( Data$yi )
summary( Data$vi )

####Analyses: modèle avec effet fixe####

rma( yi = yi, vi = vi, data = Data, method = "FE" )


####Analyses: modèle avec effet aléatoire####

rma( yi = yi, vi = vi, data = Data, test= "knha", method = "REML") 


#Possibilité d'utiliser plusieurs méthodes d'estimation différentes. Voir ?rma
#Le choix de la méthode d'estimation peut influencer les résultats (Vietchbauer et al 2015)
#Voir ?rma pour articles de référence; Borenstein et al 2009 parle de DL ou REML. Je vous conseille de vous renseigner!

#Ajustement au test statistique choisi selon Vietchbauer et al (2015). Cet ajustement rend le test plus conservateur
#La combinaison de "knha" et de "REML" peut mener à des problèmes de convergence (Vietchbauer 2010)

####Analyses: modèles avec modérateurs####

##Modérateur continu
rma( yi = yi, vi = vi, mods = Plot_size, data = Data, method = "REML", test = "knha" )
rma( yi ~ Plot_size, vi, data = Data, method = "REML", test = "knha" ) #Présenté sous format de formule

##Modérateur catégorique
rma( yi = yi, vi = vi, mods = ~Type_exp, data = Data, method = "REML", test = "knha")
anova( rma( yi = yi, vi = vi, mods = ~Type_exp, data = Data, method = "REML", test = "knha") )

##Combiner les modérateurs
mod <- rma( yi ~ Type_exp + Plot_size + Type_exp*Plot_size, vi, data = Data, method = "REML", test = "knha" )

anova( mod ) #Test statistique sur les modérateurs
summary( mod ) #Donne notamment des valeurs d'AIC, BIC et AICc
confint( mod ) #Intervalles de confiance pour les estimateurs de l'hétérogénéité

##Attention!
#Vous avez ici plusieurs exemples de modèles, mais la taille d'échantillon est très petit.
#Pouvons-nous réalistement tester autant de modérateurs? Probablement pas.
#Pouvons-nous faire plusieurs modèles sur un seul jeu de données? Pas vraiment.
#Les concepts statistiques de 'base' s'appliquent également à la méta-analyse.

####Outils de diagnostic####

##Outliers

inf <- influence( mod )
plot( inf, plotdfb = TRUE )

#Consultez Viechtbauer, W., Cheung, M.W.L., 2010. Outlier and influence diagnostics for meta‐analysis.
#Research Synthesis Methods 1, 112-125. 

####Biais de publication####

##Funnel plot

par( mfrow = c( 1,1 ) )
funnel( mod, yaxis="vi" )
  #Plusieurs possibilités pour l'axe des y. Voir ?funnel

#Funnel plot avec trim-and-fill

mod <- rma( yi = yi, vi = vi, data = Data, test= "knha", method = "REML") 
trimfill( mod ) #Cette function estime la présence de données 'manquantes'. Voir ?trimfill
taf <- trimfill( rma(yi = yi, vi = vi, data = Data, test= "knha", method="REML") ) 
funnel( taf )
  #Aucune étude manquante dans ce jeu de données
  #Pourrait être subdivisé en groupes de modérateurs

####Package metagear####
library(EBImage)
library(metagear)
library(tidyverse) #simplement pour utiliser les pipes (%>%)

##Jeu de données : liste de 11 articles. 
#Il est possible d'exporter un fichier similaire d'un logiciel de référencement bibliographique et
#de l'ajuster pour correspondre à cet exemple.

data(example_references_metagear) 
ex <- example_references_metagear


##Division du travail entre plusieurs observateurs)

team <- c("Emilie", "JP") #Équipe qui va évaluer les références

ref_unscreened <- effort_distribute(ex, reviewers = team, effort = c(80, 20), #Ici on divise l'effort "inégalement"
                                    initialize = TRUE, save_split = TRUE) 
#La fonction effort_distribute permet de diviser les références à évaluer entre plusieurs observateurs
#aléatoirement. 

list.files(pattern = "effort")#Deux fichiers ont été créé

##Phase de "screening" des références : évaluation par leur titre et résumé

abstract_screener("effort_Emilie.csv", aReviewer = "Emilie") 
#Vous devriez voir apparaitre une fenêtre qui permet d'évaluer chaque référence.

##Fonctions additionnelles Metagear

#Une fois l'évaluation terminée, les fichiers .csv peuvent être combinés avec `effort_merge()`, 
#et comparés avec `effort_summary()`

#Metagear peut tenter de downloader les PDF avec le DOI, avec les fonctions `PDF_download()` et `PDFs_collect()`
