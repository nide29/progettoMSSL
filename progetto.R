##############################################################
# Progetto d'esame Modelli Statistici & Statistical Learning #
##############################################################

# Caricamento delle librerie utili per l'analisi
library(tidyverse)
library(corrplot)

# Lettura del dataset (AirBnb Data)
file_path <- "/Users/alessandro/Library/CloudStorage/OneDrive-UniversitàdellaCalabria/Alex/Università/Magistrale/1 ANNO/Modelli Statistici e Statistical Learning/progetto/AB_NYC_2019.csv"
airbnb <- read.csv(file_path)

dim(airbnb) #mostra la dimensione del dataset
head(airbnb) #mostra le prime 6 righe del dataset
str(airbnb) #mostra la struttura interna del dataset
summary(airbnb) #sommario del dataset


# introduciamo delle variabili dummy per ogni macro-quartiere

NB_brooklyn <- ifelse(airbnb$neighbourhood_group=="Brooklyn",1,0)
NB_manhattan <- ifelse(airbnb$neighbourhood_group=="Manhattan",1,0)
NB_queens <- ifelse(airbnb$neighbourhood_group=="Queens",1,0)
NB_statenisland <- ifelse(airbnb$neighbourhood_group=="Staten Island",1,0)
NB_bronx <- ifelse(airbnb$neighbourhood_group=="Bronx",1,0)


# introduciamo delle variabili dummy per ogni tipologia di AirBnb

RM_private <- ifelse(airbnb$room_type=="Private room",1,0)
RM_entire <- ifelse(airbnb$room_type=="Entire home/apt",1,0)
RM_shared <- ifelse(airbnb$room_type=="Shared room",1,0)

airbnb <- cbind(airbnb,
                NB_brooklyn,
                NB_manhattan,
                NB_queens,
                NB_statenisland,
                NB_bronx,
                RM_private,
                RM_entire,
                RM_shared
)

dim(airbnb)
head(airbnb)

airbnb[is.na(airbnb)] <- 0
head(airbnb)

airbnb_numerico <- subset(airbnb, select = -c(name, host_name, neighbourhood_group, neighbourhood, room_type, last_review))

# matrice di correlazione
matrice_correlazione <- cor(airbnb_numerico)
corrplot(matrice_correlazione, method = "number")

# istogramma della variabile dipendente price
hist(airbnb_numerico$price, xlim=c(0,1250), breaks = 200)
boxplot(airbnb_numerico$price)
plot(density(airbnb_numerico$price))
summary(airbnb_numerico$price)


seed(123)





###########################################################################################################
## da qui in poi ha detto tutto gemini, e noi non ci fidiamo mica tanto

# Distribuzione delle variabili numeriche
lapply(airbnb[, sapply(airbnb, is.numeric)], hist)

# Controllo per outliers
boxplot(airbnb[, sapply(airbnb, is.numeric)])

# Calcolo della matrice di correlazione
correlations <- cor(airbnb[, sapply(airbnb, is.numeric)])

# Visualizzazione della matrice di correlazione
corrplot(correlations, method = "number") #method = "color" se vuoi i quadratini oppure "circle" è carino

# Identificazione di correlazioni elevate (soglia > 0.7)
corr_matrix <- abs(correlations) > 0.7
corr_matrix[upper.tri(corr_matrix)] <- FALSE
colnames(airbnb)[colSums(corr_matrix) > 1]

# Visualizzazione delle correlazioni per coppia di variabili
pairs(airbnb[, sapply(airbnb, is.numeric)])