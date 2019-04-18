library(metafor)

####Données####
#Extrait des données de 
#Champagne, E., J.-P. Tremblay, and S. D. Côté. 2016. Spatial extent 
#of neighboring plants influences the strength of associational effects 
#on mammal herbivory. Ecosphere 7:e01371.

Data <-read.table(file="Data.txt", header= TRUE, 
                  colClasses = c("factor", "factor", rep("numeric", 7)))
Data
#Year: Année de publication de l'article
#Type_exp: Type d'étude (Observation ou Expérimental)
#Plot_size: Taille des parcelles étudiées
#Mean_C, N_C, SD_C: Moyenne, n et SD du contrôle (Dommage par des herbivore de la plante focale sans plante compagne)
#Mean_E, N_E, SD_E: Moyenne, n et SD du traitement (Dommage par des herbivore de la plante focale avec plante compagne)

escalc(measure="SMD", m1i= Mean_C, m2i= Mean_E, sd1i= SD_C, sd2i= SD_E,
       n1i= N_C, n2i= N_E, data= Data)
