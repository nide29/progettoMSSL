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

# sostituiamo le celle NA in celle contenente il valore zero
airbnb[is.na(airbnb)] <- 0
head(airbnb)

# rimuoviamo le celle contenenti prezzo pari a zero
airbnb <- airbnb[airbnb$price != 0, ]


# creiamo un nuovo dataframe in cui sono presenti solamente gli attributi significativi per la nostra analisi
airbnb_numerico <- subset(airbnb, select = -c(id, host_id, latitude, longitude, name, host_name, neighbourhood_group, neighbourhood, room_type, last_review))

# combattiamo la trappola delle dummy
airbnb_dummytrap <- subset(airbnb_numerico, select = -c(NB_manhattan, RM_entire))
matrice_correlazione <- cor(airbnb_dummytrap)
corrplot(matrice_correlazione, method = "circle")

#modello lineare
model <- lm(formula = airbnb_dummytrap$price ~ ., data = airbnb_dummytrap)
summary(model)




# matrice di correlazione
matrice_correlazione <- cor(airbnb_numerico)
corrplot(matrice_correlazione, method = "circle")

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