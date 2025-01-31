# Riprodubicilità dell'esperimento
set.seed(8885)

# Caricamento le librerie dopo aver impostato il seed
library(caret)
library(class)
library(kknn)
library(lime)
library(iml)
library(randomForest)

# Preprocessamento del dataset
file_path <- "C:/ASD-Child/ConversioneValori.R"

source(file_path)

# Inizializzazione delle variabili
variables <- c("A1_Score", "A2_Score", 
               "A6_Score", "A7_Score", "A8_Score", "age")

percentage <- 0.70

k <- 7  # Numero di vicini da considerare

# Creazione copia temporanea del dataframe
df <- data
df$result <- as.factor(df$result)

# Creazione del train e del test set
# Suddivisione in base alla percentuale percentage
train_index <- sample(1:nrow(df), percentage * nrow(df))

# Creazione del set di training e di test
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Creazione di due subset distinti per il training e per il test
X_train <- train_data[, variables]
y_train <- train_data$result

X_test <- test_data[, variables]
y_test <- test_data$result

# Normalizzare le variabili numeriche per permettere KNN
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

X_train <- as.data.frame(lapply(X_train, normalize))
X_test <- as.data.frame(lapply(X_test, normalize))

# Addestramento e predizione con KNN
train_predictions <- knn(train = X_train, test = X_train, cl = y_train, k = k)
print(train_predictions)

# Creazione della confusion matrix per il training
confusion_matrix_training <- table(train_predictions, y_train)
print(confusion_matrix_training)

# Funzione per il calcolo delle metriche
calculate_multiclass_metrics <- function(confusion_matrix) {
  # Dimensioni matrice
  n <- nrow(confusion_matrix)
  
  # Metriche per ogni livello
  metrics <- data.frame(
    level = 1:n,
    precision = numeric(n),
    recall = numeric(n),
    f1_score = numeric(n)
  )
  
  # Calcolo metriche
  for (i in 1:n) {
    # Precisione: TP / (TP + FP)
    tp <- confusion_matrix[i, i]
    fp <- sum(confusion_matrix[, i]) - tp
    precision <- if(tp + fp > 0) tp / (tp + fp) else 0
    
    # Recall: TP / (TP + FN)
    fn <- sum(confusion_matrix[i, ]) - tp
    recall <- if(tp + fn > 0) tp / (tp + fn) else 0
    
    # F1-Score: 2 * (precision * recall) / (precision + recall)
    f1_score <- if(precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0
    
    # Accuracy globale
    total_correct <- sum(diag(confusion_matrix))
    total_samples <- sum(confusion_matrix)
    global_accuracy <- total_correct / total_samples
    
    # Salvataggio metriche
    metrics$precision[i] <- precision
    metrics$recall[i] <- recall
    metrics$f1_score[i] <- f1_score
  }
  
  return(list(
    global_accuracy = global_accuracy,
    per_class_metrics = metrics
  ))
}

# Salvataggio dei risultati della matrice di confusione del training
risultati <- calculate_multiclass_metrics(confusion_matrix_training)

# Stampa dell'accuratezza del training set
cat("Accuratezza training", risultati$global_accuracy)

# Stampa delle metriche per ogni livello
risultati[["per_class_metrics"]][["precision"]]
risultati[["per_class_metrics"]][["recall"]]
risultati[["per_class_metrics"]][["f1_score"]]

# Esecuzione del modello KNN
test_predictions <- knn(train = X_train, test = X_test, cl = y_train, k = k)
print(test_predictions)

# Creazione della confusion matrix per il test set
confusion_matrix_test <- table(test_predictions, y_test)
print(confusion_matrix_test)

# Salvataggio dei risultati
risultati <- calculate_multiclass_metrics(confusion_matrix_test)

# Stampa dell'accuratezza del test set
cat("Accuratezza test", risultati$global_accuracy)

# Stampa metriche per ogni livello
risultati[["per_class_metrics"]][["precision"]]
risultati[["per_class_metrics"]][["recall"]]
risultati[["per_class_metrics"]][["f1_score"]]

# Salvataggio della confusion matrix per il test set
confusion_matrix_test <- as.data.frame(as.table(confusion_matrix_test))

# Stampa dell'accuratezza del test set
cat("Accuratezza test", risultati$global_accuracy)