set.seed(8885)

library(nnet)
library(caret)

# Preprocessamento del dataset
file_path <- "C:/R/ConversioneValori.R"
source(file_path)

# Creazione copia temporanea del dataframe
df <- data
df$result <- as.factor(df$result)  # Assicuriamoci che sia un fattore

# Creazione del train e del test set
train_index <- sample(1:nrow(df), percentage * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

neural_network_train <- function(trainingData) {
  # Selezione dei predittori e della variabile di risposta
  predictorNames <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", 
                      "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score")
  
  # Creazione del dataset corretto con i predittori + target
  trainingData <- trainingData[, c(predictorNames, "result")]
  
  # Definizione del modello di rete neurale
  classificationNeuralNetwork <- nnet(result ~ ., data = trainingData,
                                      size = 10, linout = FALSE, 
                                      maxit = 1000, decay = 0, act.fct = "relu")
  
  print(colnames(trainingData))  # Debug: Controlla le colonne effettive
  
  # Funzione di predizione
  predictFcn <- function(newdata) {
    predict(classificationNeuralNetwork, newdata, type = "class")
  }
  
  # Creazione della struttura di output
  trainedClassifier <- list(
    predictFcn = predictFcn,
    ClassificationNeuralNetwork = classificationNeuralNetwork,
    RequiredVariables = predictorNames
  )
  
  # Validazione incrociata
  control <- trainControl(method = "cv", number = 20)
  model_cv <- caret::train(result ~ ., data = trainingData, method = "nnet", trControl = control, 
                           trace = FALSE, linout = FALSE, maxit = 1000)
  
  # Accuratezza della validazione
  validationAccuracy <- max(model_cv$results$Accuracy) * 100
  
  return(list(trainedClassifier = trainedClassifier, validationAccuracy = validationAccuracy))
}

neural_network_train(train_data)
