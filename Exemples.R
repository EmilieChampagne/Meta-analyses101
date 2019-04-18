library(metafor)

####Données####
#Extrait des données de 
#Champagne, E., J.-P. Tremblay, and S. D. Côté. 2016. Spatial extent 
#of neighboring plants influences the strength of associational effects 
#on mammal herbivory. Ecosphere 7:e01371.

Data <-read.table(file="Exemple.txt", header= TRUE, na.string= ".", stringsAsFactors = FALSE)
Data
#Year: Année de publication de l'article
#Type_exp: Type d'étude (Observation ou Expérimental)
#Plot_size: Taille des parcelles étudiées
#Mean_C, N_C, SD_C: Moyenne, n et SD du contrôle (Dommage par des herbivore de la plante focale sans plante compagne)
#Mean_E, N_E, SD_E: Moyenne, n et SD du traitement (Dommage par des herbivore de la plante focale avec plante compagne)

