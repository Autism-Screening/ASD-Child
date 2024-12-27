library(caret)
library(lime)
library(iml)

# Preprocessamento del dataset
file_path <- "C:/R/Codice/Pushate/ConversioneValori.R"

# Esegui lo script
source(file_path)

# Prendi le prime 10 colonne relative ai punteggi
variables <- colnames(data)[1:10]
isFirst <- TRUE
best_rmse <- Inf
best_subset <- NULL
best_model <- NULL
best_accuracy <- 0
temp_accuracy <- 0

for (k in 1:length(variables)) {
  subsets <- combn(variables, k, simplify = FALSE)
  
  for (subset in subsets) {
    cat(accuracy, subset, "\n")
    set.seed(0) # Per riproducibilità
    # Suddivisione del dataset in 2 partizioni: una di addestramento, l'altra di testing
    trainIndex <- createDataPartition(data$result, p = .8, list = FALSE)
    training <- data[trainIndex, ]
    testing <- data[-trainIndex, ]
    
    train_data <- training[, c(subset, "result")]
    test_data <- testing[, subset, drop = FALSE]
    
    # Utilizzo di un modello per addestrare i dati con il metodo linear model
    model <- train(
      result ~ ., data = train_data,
      method = "lm",
      trControl = trainControl(method = "cv", number = 5)
    )
    
    predictions <- predict(model, test_data)
    # Calcolo la RMSE, ovvero la radice dell'errore quadratico medio
    rmse <- sqrt(mean((predictions - testing$result)^2))
    
    # Calcolo l'accuratezza sulla base di R², ovvero il Coefficiente di Determinazione 
    accuracy <- 1 - (sum((predictions - testing$result)^2) / sum((testing$result - mean(testing$result))^2))
    # Salve momentaneamente il subset con la relativa accuracy per salvarlo definitivamente poco prima del prossimo k
    if (accuracy > temp_accuracy) {
      temp_accuracy <- accuracy
      temp_subset <- subset
      temp_model <- model  # Aggiungi il modello corrente
      temp_rmse <- rmse    # Aggiungi anche il RMSE corrente
    }
  }
  # Considera il modello solo se supera il 80% di accuratezza
  if (temp_accuracy > 0.8 && isFirst) {
    isFirst = FALSE
    best_accuracy <- temp_accuracy
    best_rmse <- temp_rmse
    best_subset <- temp_subset
    best_model <- temp_model
    cat("Migliore subset:", best_subset, "con accuratezza di:", best_accuracy,"\n")
  }
}

# Diagnostica: verifica il comportamento delle previsioni
cat("\nDiagnostica delle previsioni sul test set:\n")
cat("Predizioni:", head(predict(best_model, testing[, best_subset])), "\n")
 
# Matrici di verità
validation_predictions <- round(predict(best_model, training[, best_subset]))
test_predictions <- round(predict(best_model, testing[, best_subset]))

cat("\nMatrice di verità - Validation Set:\n")
validation_conf_matrix <- table(Predicted = validation_predictions, Actual = training$result)
print(validation_conf_matrix)

cat("\nMatrice di verità - Test Set:\n")
test_conf_matrix <- table(Predicted = test_predictions, Actual = testing$result)
print(test_conf_matrix)

calc_confusion_metrics <- function(conf_matrix) {
  fp <- sum(rowSums(conf_matrix) - diag(conf_matrix))
  fn <- sum(colSums(conf_matrix) - diag(conf_matrix))
  tp <- sum(diag(conf_matrix))
  tn <- sum(conf_matrix) - (fp + fn + tp)
  
  return(list(FP = fp, FN = fn, TP = tp, TN = tn))
}

# Stampa le metriche in modo chiaro
print_confusion_metrics <- function(metrics) {
  cat(sprintf("Falsi Positivi: %d\n", metrics$FP))
  cat(sprintf("Veri Positivi: %d\n", metrics$TP))
}

cat("\nMetriche di confusione - Validation Set:\n")
validation_metrics <- calc_confusion_metrics(validation_conf_matrix)
print_confusion_metrics(validation_metrics)

cat("\nMetriche di confusione - Test Set:\n")
test_metrics <- calc_confusion_metrics(test_conf_matrix)
print_confusion_metrics(test_metrics)

# Spiegazioni con LIME
scaled_data <- as.data.frame(scale(training[, best_subset]))
explainer <- lime(scaled_data, best_model)
test_data_scaled <- as.data.frame(scale(testing[, best_subset]))
explanation <- explain(test_data_scaled, explainer, n_labels = 1, n_features = 5, bin_continuous = TRUE)
cat("\nSpiegazioni LIME per il sottoinsieme ottimale:", paste(best_subset, collapse = ", "), "\n")
print(explanation)

# Spiegazioni con SHAP
predictor <- Predictor$new(best_model, data = training[, best_subset], y = training$result)
shapley <- Shapley$new(predictor, x.interest = testing[1, best_subset])
cat("\nSpiegazioni SHAP per il sottoinsieme ottimale:", paste(best_subset, collapse = ", "), "\n")
plot(shapley)

# Spiegazioni con SHAP di tutte le features
predictor <- Predictor$new(temp_model, data = training[, temp_subset], y = training$result)
shapley <- Shapley$new(predictor, x.interest = testing[1, temp_subset])
cat("\nSpiegazioni SHAP per tutte le caratteristiche comportamentali:", paste(best_subset, collapse = ", "), "\n")
plot(shapley)

cat("RMSE minimo:", best_rmse, "\n")
cat("Accuratezza raggiunta:", best_accuracy, "\n")
cat("Il sottoinsieme ottimale è:", paste(best_subset, collapse = ", "), "\n")

# Serve accuratezza su validation e sul test set. Serve matrice di verità dai 
# modelli presenti per visualizzare cosa succede con il predittore
# Accuracy su validation e test. Matrice di verità su validation e test.
# Utilizzare il clustering utilizzando quelle features ottenute da SHAP. 
# Non è detto che gli altri regressori non lineari funzionino meglio/peggio. 
# Provarle tutte per avere una visione generale di cosa succede nel dataset