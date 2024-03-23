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

#PROCESSO DI RIMOZIONE OUTLIERS

#-PRICE - Variabile dipendente
mean_value <- mean(airbnb_dummy$price)
std_dev <- sd(airbnb_dummy$price)
upper_limit <- mean_value + 2 * std_dev
outliers <- airbnb_dummy$price[airbnb_dummy$price > upper_limit]
airbnb_final <- airbnb_dummy[airbnb_dummy$price <= upper_limit, ]

print(mean_value)
print(upper_limit)
#---------------------------------------------------------------#
#modello lineare che tiene in considerazione tutti i regressori
model <- lm(formula = airbnb_final$price ~ ., data = airbnb_final)
summary(model)

#istogramma della variabile dipendente
hist(airbnb_final$price, main = "Istogramma della variabile dipendente [price]")

summary(airbnb_final$price)

# matrice di correlazione
correlazione <- cor(airbnb_final)
corrplot(correlazione, method = "number")

# determinante della matrice XtX
X <- as.matrix(cbind(rep(1, nrow(airbnb_final)), airbnb_final))
determinante <- det(t(X) %*% X)
print(determinante)

# conditional number
autoval<-eigen(t(X)%*%X)
condition.number<-sqrt(max(autoval$values)/min(autoval$values))
print(condition.number)
print(max(autoval$values))
print(min(autoval$values))

# calcolo del VIF
vif(model)

# calcolo della Tolleranza
toleranceValue <- 1/vif(model)
print(toleranceValue)


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
modBPtest <- lm(formula = res12~ ., data = airbnb_final)
summary(modBPtest)

#Test di White
fit1<-fitted(model); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

# Trasformazione logaritmica alla variabile dipendente
model_log_lin <- lm(formula = log(airbnb_final$price) ~ ., data = airbnb_final)
summary(model_log_lin)

#Nota: Con la trasf logaritmica alla variabile dipendente il modello migliora in termini di R squared e adj- r squared

#Verifica della presenza dell'eteroschedasticità dal punto di vista grafico 2
residui_log <- residuals(model_log_lin)
log_fitted <- fitted(model_log_lin)

# Crea il grafico 
plot(log_fitted, residui_log,
     xlab = "Fitted",
     ylab = "Residui",
     main = "Residui vs Log fitted")

# Test di Breusch-Pagan
res1 <- residuals(model_log_lin); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = airbnb_final)
summary(modBPtest)

#Test di White
fit1<-fitted(model_log_lin); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#Procedere al modello pesato per le ordinate stimate(i regressori vengono divisi per le ordinate stimate)
airbnb_w <- airbnb_final

# Dividere tutti i regressori per le ordinate stimate
airbnb_w[-1] <- airbnb_w[-1] / fitted(model)

wModel <- lm(formula = price ~ ., data = airbnb_w)
summary(wModel)

fittedM <- fitted(wModel)
residui <- residuals(wModel)

plot(fittedM, residui)

# Test di Breusch-Pagan per il modello pesato 
res1 <- residuals(wModel); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = airbnb_w)
summary(modBPtest)

#Test di White per il modello pesato 
fit1<-fitted(wModel); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#A questo punto bisogna pesare il modello per ogni regressore e cercare di eliminare l'eteroschedasticità
wAirbnb_regressor <- airbnb_final

#Modello pesato per il regressore minimum_nights
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$minimum_nights)
summary(wModel_regressor)

fittedM <- fitted(wModel_regressor)
residui <- residuals(wModel_regressor)

plot(fittedM, residui)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore minimum nights
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$minimum_nights)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore minimum nights
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore number_of_reviews
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$number_of_reviews)
summary(wModel_regressor)

fittedM <- fitted(wModel_regressor)
residui <- residuals(wModel_regressor)

plot(fittedM, residui)

# Test di Breusch-Pagan per il modello pesato rispetto al regressore minimum nights
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$number_of_reviews)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore minimum nights
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore reviews_per_month
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$reviews_per_month)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore reviews_per_month
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$reviews_per_month)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore reviews_per_month
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore calculated_host_listings_count
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$calculated_host_listings_count)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore calculated_host_listings_count
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$calculated_host_listings_count)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore calculated_host_listings_count
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)


#---

#Modello pesato per il regressore availability_365
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$availability_365)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore availability_365
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$calculated_host_listings_count)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore availability_365
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore NB_brooklyn
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_brooklyn)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore NB_brooklyn
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_brooklyn)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore NB_brooklyn
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore NB_queens
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_queens)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore NB_queens
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_queens)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore NB_queens
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)


#---

#Modello pesato per il regressore NB_statenisland
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_statenisland)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore NB_statenisland
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_statenisland)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore NB_statenisland
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore NB_bronx
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_bronx)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore NB_bronx
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$NB_bronx)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore NB_bronx
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#---

#Modello pesato per il regressore RM_private
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$RM_private)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore RM_private
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$RM_private)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore RM_private
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)


#---

#Modello pesato per il regressore RM_shared
wModel_regressor <- lm(formula = price ~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$RM_shared)
summary(wModel_regressor)


# Test di Breusch-Pagan per il modello pesato rispetto al regressore RM_shared
res1 <- residuals(wModel_regressor); res12<- res1^2
modBPtest <- lm(formula = res12~ ., data = wAirbnb_regressor, weights = wAirbnb_regressor$RM_shared)
summary(modBPtest)

#Test di White per il modello pesato  rispetto al regressore RM_shared
fit1<-fitted(wModel_regressor); fit12<-fit1^2
modWtest <- lm(res12~fit1+fit12)
summary(modWtest)

#RIDGE REGRESSION
library(glmnet)
set.seed(123) 

X<-as.matrix(airbnb_final[,-1])
y <- airbnb_final$price
new_grid = 10^seq (5,-4, length = 100)
rr = glmnet (X, y, alpha=0,lambda=new_grid, standardize=FALSE)

plot(rr, main = "Ridge regression",  xvar = "lambda")

#Cerchiamo il valore di lambda ottimale al fine di minimizzare l'errore quadratico medio (MSE)
# Creazione del modello con cross-validation attraverso la funzione cv di glmnet
crossval_model <- cv.glmnet(X, y, nfolds = 10, alpha = 0) 
plot(crossval_model)

#min mse
min_mse <- min(crossval_model$cvm)
print(paste("Minimum MSE:", min_mse))

# Addestramento del modello ridge finale con il miglior lambda
best_lambda <- crossval_model$lambda.min # Ricerca del valore di lambda ottimale
print(paste("Best Lambda:", best_lambda))
final_model <- glmnet(X, y, nfolds=10, alpha = 0, lambda = best_lambda)

#La stima del modello ridge risultante
coefficients <- coef(final_model, s = best_lambda, exact = TRUE)
print(coefficients[,1])

#LASSO REGRESSION
new_grid = 10^seq (4,-4, length = 100)
model_lasso <- glmnet(X,y, lambda = new_grid, alpha = 1, standardize = FALSE)
plot(model_lasso, main="Lasso regression", xvar="lambda", label = TRUE)

#Cerchiamo il valore di lambda ottimale al fine di minimizzare l'errore quadratico medio (MSE)
set.seed(123)
new_grid_lasso <- 10^seq(4, -4, length = 200)
cross_lasso= cv.glmnet( X, y, nfolds= 10, lambda = new_grid_lasso, alpha=1 )
plot(cross_lasso,  main="MSE Lasso Regression", xvar="lambda", label = TRUE)

#min mse
min_mse <- min(cross_lasso$cvm)
print(min_mse)

#Addestramento del modello lasso finale con il miglior lambda 
best_lambda <- cross_lasso$lambda.min
best_lasso <- glmnet(X, y, nfolds=10, alpha=1, lambda=best_lambda)
print (coef (best_lasso)[,1])
print(best_lambda)

#La stima del modello lasso risultante
coefficients <- coef(best_lasso, s = best_lambda, exact = TRUE)
print(coefficients[,1])

#ELASTIC NET REGRESSION
set.seed(123)
new_grid_elastic <- 10^seq(3, -3, length = 200)

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.2, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)
print(coef(model_elastic))

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.4, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)
print(coef(model_elastic)[,1])

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.6, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.8, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)


#Cross val 
# Imposta una sequenza di valori di lambda 
set.seed(123)
lambda_values <- 10^seq(3, -3, length = 200)

#alpha = 0.2
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 10, alpha=0.2)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)

best_elastic <- glmnet(X, y, nfolds=10, alpha=0.2, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.4
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 10, alpha=0.4)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=10, alpha=0.4, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.6
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 10, alpha=0.6)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=10, alpha=0.6, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.8
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 10, alpha=0.8)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=10, alpha=0.8, lambda=best_lambda1)
print (coef (best_elastic)[,1])


#5-fold cross validation
set.seed(123) 

X<-as.matrix(airbnb_final[,-1])
y <- airbnb_final$price
new_grid = 5^seq (5,-4, length = 50)

#RIDGE REGRESSION
rr = glmnet (X, y, alpha=0,lambda=new_grid, standardize=FALSE)
plot(rr, main = "Ridge regression",  xvar = "lambda")
crossval_model <- cv.glmnet(X, y, nfolds = 5, alpha = 0) 
plot(crossval_model)
min_mse <- min(crossval_model$cvm)
print(paste("Minimum MSE:", min_mse))
best_lambda <- crossval_model$lambda.min 
print(paste("Best Lambda:", best_lambda))
final_model <- glmnet(X, y, nfolds=5, alpha = 0, lambda = best_lambda)
coefficients <- coef(final_model, s = best_lambda, exact = TRUE)
print(coefficients[,1])

#LASSO REGRESSION
new_grid = 5^seq (4,-4, length = 50)
model_lasso <- glmnet(X,y, lambda = new_grid, alpha = 1, standardize = FALSE)
plot(model_lasso, main="Lasso regression", xvar="lambda", label = TRUE)
new_grid_lasso <- 5^seq(4, -4, length = 200)
cross_lasso= cv.glmnet( X, y, nfolds= 5, lambda = new_grid_lasso, alpha=1 )
plot(cross_lasso,  main="MSE Lasso Regression", xvar="lambda", label = TRUE)
min_mse <- min(cross_lasso$cvm)
print(min_mse)
best_lambda <- cross_lasso$lambda.min
best_lasso <- glmnet(X, y, nfolds=5, alpha=1, lambda=best_lambda)
print (coef (best_lasso)[,1])
print(best_lambda)
coefficients <- coef(best_lasso, s = best_lambda, exact = TRUE)
print(coefficients[,1])

#ELASTIC NET REGRESSION
set.seed(123)
new_grid_elastic <- 5^seq(3, -3, length = 200)

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.2, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)
print(coef(model_elastic))

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.4, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)
print(coef(model_elastic)[,1])

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.6, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.8, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)

lambda_values <- 5^seq(3, -3, length = 200)

#alpha = 0.2
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 5, alpha=0.2)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)

best_elastic <- glmnet(X, y, nfolds=5, alpha=0.2, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.4
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 5, alpha=0.4)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=5, alpha=0.4, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.6
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 5, alpha=0.6)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=5, alpha=0.6, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.8
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds = 5, alpha=0.8)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=5, alpha=0.8, lambda=best_lambda1)
print (coef (best_elastic)[,1])


#leave-one-out cross validation
set.seed(123) 

X<-as.matrix(airbnb_final[,-1])
y <- airbnb_final$price
new_grid = 5^seq (5,-4, length = 50)
obs <- nrow(X) #numero di osservazioni

#RIDGE REGRESSION
rr = glmnet (X, y, alpha=0,lambda=new_grid, standardize=FALSE)
plot(rr, main = "Ridge regression",  xvar = "lambda")
crossval_model <- cv.glmnet(X, y, nfolds = obs, alpha = 0) 
plot(crossval_model)
min_mse <- min(crossval_model$cvm)
print(paste("Minimum MSE:", min_mse))
best_lambda <- crossval_model$lambda.min 
print(paste("Best Lambda:", best_lambda))
final_model <- glmnet(X, y, nfolds=obs, alpha = 0, lambda = best_lambda)
coefficients <- coef(final_model, s = best_lambda, exact = TRUE)
print(coefficients[,1])

#LASSO REGRESSION
new_grid = 5^seq (4,-4, length = 50)
model_lasso <- glmnet(X,y, lambda = new_grid, alpha = 1, standardize = FALSE)
plot(model_lasso, main="Lasso regression", xvar="lambda", label = TRUE)
new_grid_lasso <- 5^seq(4, -4, length = 200)
cross_lasso= cv.glmnet( X, y, nfolds=obs, lambda = new_grid_lasso, alpha=1 )
plot(cross_lasso,  main="MSE Lasso Regression", xvar="lambda", label = TRUE)
min_mse <- min(cross_lasso$cvm)
print(min_mse)
best_lambda <- cross_lasso$lambda.min
best_lasso <- glmnet(X, y, nfolds=obs, alpha=1, lambda=best_lambda)
print (coef (best_lasso)[,1])
print(best_lambda)
coefficients <- coef(best_lasso, s = best_lambda, exact = TRUE)
print(coefficients[,1])

#ELASTIC NET REGRESSION
set.seed(123)
new_grid_elastic <- 5^seq(3, -3, length = 200)

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.2, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)
print(coef(model_elastic))

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.4, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)
print(coef(model_elastic)[,1])

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.6, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)

model_elastic <- glmnet(X, y, lambda = new_grid_elastic, alpha=0.8, standardize=FALSE)
plot(model_elastic, xvar="lambda", label=TRUE)

lambda_values <- 5^seq(3, -3, length = 200)

#alpha = 0.2
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds=obs, alpha=0.2)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)

best_elastic <- glmnet(X, y, nfolds=obs, alpha=0.2, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.4
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds=obs, alpha=0.4)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=obs, alpha=0.4, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.6
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds=obs, alpha=0.6)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=obs, alpha=0.6, lambda=best_lambda1)
print (coef (best_elastic)[,1])

#alpha = 0.8
elastic_crossval = cv.glmnet(X, y, lambda = lambda_values, nfolds=obs, alpha=0.8)
plot(elastic_crossval)
best_lambda1 <- elastic_crossval$lambda.min
print(best_lambda1)
min_MSE <- min(elastic_crossval$cvm)
print(min_MSE)
best_elastic <- glmnet(X, y, nfolds=obs, alpha=0.8, lambda=best_lambda1)
print (coef (best_elastic)[,1])



