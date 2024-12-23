library(caret)
library(lime)
library(iml)
library(ggplot2)

# Percorso del file dello script da eseguire
file_path <- "C:/R/ConversioneValori.R"


# Esegui lo script
source(file_path)

result <- data$result
gender <- data$gender
age <- data$age

# Prendi le prime 10 colonne relative ai punteggi
variables <- data[, 1:10]

data <- data.frame(cbind(variables, result, gender)) # Aggiungi 'genere' e 'età' al dataset

# Imposta il seed per la riproducibilità
set.seed(0)

gender_formula <- as.formula("result ~ (A1_Score + A2_Score + A3_Score + A4_Score + A5_Score + A6_Score + A7_Score + A8_Score + A9_Score + A10_Score) * gender")

# 2. Dividi il Dataset in Training e Testing
trainIndex <- createDataPartition(data$result, p = .8, list = FALSE)
dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

# Addestra il modello
model <- train(result ~ ., data = dataTrain, method = "lm")

# LIME: Crea un oggetto explainer e spiega le predizioni
explainer <- lime(dataTrain, model)
explanation <- explain(dataTest, explainer, n_labels = 1, n_features = 5)
plot_features(explanation)

# SHAP: Crea un oggetto predictor e calcola i valori SHAP
predictor <- Predictor$new(model, data = dataTrain, y = dataTrain$result)
shapley <- Shapley$new(predictor, x.interest = dataTest[1,])
plot(shapley)