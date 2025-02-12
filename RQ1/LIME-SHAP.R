# Riprodubicilità dell'esperimento
set.seed(8885)

# Caricamento le librerie dopo aver impostato il seed
library(caret)
library(lime)
library(iml)
library(randomForest)

# Preprocessamento del dataset
file_path <- "C:/ASD-Child/ConversioneValori.R"

source(file_path)

# Stampa dei nomi delle colonne
colnames(data)

# Inizializzazione delle variabili
variables <- c("A1_Score", "A2_Score", 
               "A6_Score", "A7_Score", "A8_Score", "age")
percentage <- 0.70
number_of_trees <- 50

# Creazione copia temporanea del dataframe
df <- data;
df$result <- as.factor(df$result)

# Creazione del train e del test set
# Suddivisione in base alla percentuale percentage
train_index <- sample(1:nrow(df), percentage * nrow(df))

# Creazione del set di training
train_data <- df[train_index, ]

# Creazione del set di test
test_data <- df[-train_index, ]

# Creazione di due subset distinti per il training e per il test
X_train <- train_data[, variables]
y_train <- train_data$result

# Addestramento del modello sul dataset di training
model <- randomForest(x = X_train, y = y_train, ntree = number_of_trees) #cercate di capire se è possibile ridurre questo 100 creando un modello più piccolo e rapido
print(model)

# Predizione del modello sul train set
predictions <- predict(model, X_train)

# Creazione della confusion matrix per il training
confusion_matrix_training <- table(predictions, y_train)
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

# Salvataggio della confusion matrix per il training
confusion_matrix_training <- as.data.frame(as.table(confusion_matrix_training))

# Salvataggio in png e creazione del plot per heatmap della confusion matrix
png("rf_confusion_matrix_training.png", width = 700, height = 567)
ggplot(confusion_matrix_training, aes(predictions, y_train, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Predicted", y = "Actual", fill = "Count") +
  ggtitle("Confusion Matrix")
dev.off()  # Salva il file
ggplot(confusion_matrix_training, aes(predictions, y_train, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Predicted", y = "Actual", fill = "Count") +
  ggtitle("Confusion Matrix")

# Esecuzione del modello sul test set
X_test <- test_data[, variables]
y_test <- test_data$result

# Predizione del modello sul test set
predictions <- predict(model, X_test)

# Creazione della confusion matrix per il test set
confusion_matrix_test <- table(predictions, y_test)
print(confusion_matrix_test)

# Salvataggio dei risultati
risultati <- calculate_multiclass_metrics(confusion_matrix_test)

# Stampa dell'accuratezza del test set
cat("Accuratezza test", risultati$global_accuracy)

# Stampa delle metriche per ogni livello
risultati[["per_class_metrics"]][["precision"]]
risultati[["per_class_metrics"]][["recall"]]
risultati[["per_class_metrics"]][["f1_score"]]

# Salvataggio della confusion matrix per il test set
confusion_matrix_test <- as.data.frame(as.table(confusion_matrix_test))

# Salvataggio in png e creazione del plot per heatmap della confusion matrix
png("rf_confusion_matrix_test.png", width = 700, height = 567)
ggplot(confusion_matrix_test, aes(predictions, y_test, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Predicted", y = "Actual", fill = "Count") +
  ggtitle("Confusion Matrix")
dev.off()  # Salva il file
ggplot(confusion_matrix_test, aes(predictions, y_test, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(x = "Predicted", y = "Actual", fill = "Count") +
  ggtitle("Confusion Matrix")

# Esecuzione SHAP per un singolo campione per analizzare
# come ogni feature contribuisce al risultato predetto
predictor <- Predictor$new(model, data = X_train, y = y_train)
shapley <- Shapley$new(predictor, x.interest = X_test[1, , drop = FALSE])
png("shapley_random_forest.png", width = 700, height = 567)   
shapley$plot()
dev.off()   
shapley$plot()

# Shap sui primi 10 anziché su tutti i campioni del test
shapley_values <- Shapley$new(predictor, x.interest = X_test[1:10, ])
png("shapley_random_forest_first10.png", width = 700, height = 567)   
shapley_values$plot()
dev.off()   
shapley_values$plot()

# Stampa dell'accuratezza del test set
cat("Accuratezza test", risultati$global_accuracy)