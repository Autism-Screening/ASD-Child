library(caret)

# Preprocessamento del dataset
file_path <- "C:/R/ConversioneValori.R"

source(file_path)

# Creazione copia temporanea del dataframe
df <- data
df$result <- as.factor(df$result)

# Creazione del train e del test set
# Suddivisione in base alla percentuale percentage
train_index <- sample(1:nrow(df), percentage * nrow(df))

# Creazione del set di training e di test
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

svm_linear_train <- function(training_data) {
  # Seleziona i predittori e la variabile di risposta
  predictor_names <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", 
                         "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score")
  
  predictors <- training_data[, predictor_names]
  response <- as.factor(training_data$result)  # Converti in fattore per la classificazione
  
  # Definizione del metodo di training
  control <- trainControl(method = "cv", number = 20)  # 20-fold cross-validation
  
  # Addestramento del modello SVM con kernel lineare
  trained_model <- caret::train(predictors, response,
                         method = "svmLinear",
                         trControl = control,
                         preProcess = c("center", "scale"))
  
  # Funzione di previsione
  predict_function <- function(new_data) {
    predict(trained_model, new_data)
  }
  
  # Accuratezza della validazione incrociata
  validation_accuracy <- max(trained_model$results$Accuracy) * 100
  
  # Ritorna il modello e l'accuratezza
  list(trained_model = trained_model, 
       predict_function = predict_function, 
       validation_accuracy = validation_accuracy)
}

svm_linear_train(train_data)