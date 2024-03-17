##############################################################
# Progetto d'esame Modelli Statistici & Statistical Learning #
##############################################################

# Caricamento delle librerie utili per l'analisi
library(tidyverse)
library(corrplot)
library(car)
library(lmtest)

# Lettura del dataset (AirBnb Data)
file_path <- "/Users/vincenzopresta/Desktop/mssl/progettoMSSL/AB_NYC_2019.csv"
file_path_A <- "/Users/alessandro/Library/CloudStorage/OneDrive-UniversitàdellaCalabria/Alex/Università/Magistrale/1 ANNO/Modelli Statistici e Statistical Learning/progetto/AB_NYC_2019.csv"
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

# trappola delle dummy
airbnb_dummy <- subset(airbnb_numerico, select = -c(NB_manhattan, RM_entire))
matrice_correlazione <- cor(airbnb_dummy)
corrplot(matrice_correlazione, method = "circle")

# istogramma della variabile dipendente price
hist(airbnb_numerico$price, xlim=c(0,1250), breaks = 200)
boxplot(airbnb_numerico$price)
plot(density(airbnb_numerico$price))
summary(airbnb_numerico$price)

#modello lineare che tiene in considerazione tutti i regressori
model <- lm(formula = airbnb_dummy$price ~ ., data = airbnb_dummy)
summary(model)

# matrice di correlazione
matrice_correlazione <- cor(airbnb_numerico)
corrplot(matrice_correlazione, method = "circle")

# determinante della matrice XtX
X <- as.matrix(cbind(rep(1, nrow(airbnb_dummy)), airbnb_dummy$minimum_nights, airbnb_dummy$number_of_reviews, airbnb_dummy$reviews_per_month, airbnb_dummy$calculated_host_listings_count, airbnb_dummy$availability_365, airbnb_dummy$NB_brooklyn, airbnb_dummy$NB_queens, airbnb_dummy$NB_statenisland, airbnb_dummy$NB_bronx, airbnb_dummy$RM_private, airbnb_dummy$RM_shared))
determinante <- det(t(X) %*% X)
print(determinante)

# conditional number
autoval<-eigen(t(X)%*%X)
condition.number<-sqrt(max(autoval$values)/min(autoval$values))
print(autoval$values)
print(autoval$vectors)
print(condition.number)
print(max(autoval$values))
print(min(autoval$values))

# calcolo del VIF
vif(model)

# calcolo della Tolleranza
toleranceValue <- 1/vif(model)
print(toleranceValue)


set.seed(123)
#Verifica della presenza dell'eteroschedasticità dal punto di vista grafico
residui <- residuals(model)
ordinate_stimate <- fitted(model)


# Crea il grafico dei residui rispetto alle ordinate stimate
plot(ordinate_stimate, residui,
     xlab = "Ordinate stimate",
     ylab = "Residui",
     main = "Grafico dei residui rispetto alle ordinate stimate")

# Test di Breusch-Pagan
res1 <- residuals(model); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = airbnb_dummy)
summary(modBPtest)

#Test di White
fit1<-fitted(model); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

# Alternativa: Trasformazione logaritmica alla variabile dipendente
model_log_lin <- lm(formula = log(airbnb_dummy$price) ~ ., data = airbnb_dummy)
#Nota: Con la trasf logaritmica alla variabile dipendente il modello migliora in termini di R squared e adj- r squared

#Verifica della presenza dell'eteroschedasticità dal punto di vista grafico
residui_log <- residuals(model_log_lin)
log_fitted <- fitted(model_log_lin)

# Crea il grafico 
plot(log_fitted, residui_log,
     xlab = "Fitted",
     ylab = "Residui",
     main = "Residui vs Log fitted")

# Test di Breusch-Pagan
res1 <- residuals(model_log_lin); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = airbnb_dummy)
summary(modBPtest)

#Test di White
fit1<-fitted(model_log_lin); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#Procedere al modello pesato per le ordinate stimate(i regressori vengono divisi per le ordinate stimate)
airbnb_w <- airbnb_dummy

# Dividere tutti i regressori per le ordinate stimate
airbnb_w[-1] <- airbnb_w[-1] / fitted(model)

wModel <- lm(formula = price ~ ., data = airbnb_w)
summary(wModel)

# Test di Breusch-Pagan per il modello pesato 
res1 <- residuals(wModel); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = airbnb_w)
summary(modBPtest)

#Test di White per il modello pesato 
fit1<-fitted(wModel); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#A questo punto bisogna pesare il modello per ogni regressore e cercare di eliminare l'eteroschedasticità

#Modello pesato per il regressore
wAirbnb_regressor <- airbnb_dummy
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_queens)
summary(modello_pesato_mn)


# Test di Breusch-Pagan per il modello pesato 
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_queens)
summary(modBPtest)

#Test di White per il modello pesato 
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)




#TODO: 
#se ancora presente si passa a un modello pesato per i regressori