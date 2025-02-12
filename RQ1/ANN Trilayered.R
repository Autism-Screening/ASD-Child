set.seed(8885)

library(nnet)
library(caret)
library(iml)
library(ggplot2)
library(reshape2)

# Preprocessamento del dataset
file_path <- "C:/ASD-Child/ConversioneValori.R"

source(file_path)

# Creazione copia temporanea del dataframe
df <- data
df$result <- as.factor(df$result)

percentage <- 0.70

# Creazione del train e del test set
# Suddivisione in base alla percentuale percentage
train_index <- sample(1:nrow(df), percentage * nrow(df))

# Creazione del set di training e di test
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Definizione delle variabili predittive e della risposta
predictorNames <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", 
                    "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score")

neural_network_train <- function(trainingData) {
  
  predictors <- trainingData[, predictorNames]
  response <- as.factor(trainingData$result)  # Conversione in fattore per la classificazione
  
  # Creazione della formula per il modello
  formula <- as.formula(paste('result ~', paste(predictorNames, collapse = ' + ')))
  
  # Addestramento del modello di rete neurale
  classificationNeuralNetwork <- nnet(
    formula,
    data = trainingData,
    size = 11,       # Numero di neuroni negli strati nascosti
    maxit = 1000,    # Numero massimo di iterazioni
    decay = 0,       # Regolarizzazione L2 (equivalente a Lambda)
    linout = FALSE   # Modello di classificazione
  )
  
  # Funzione di previsione
  predictFcn <- function(newdata) {
    predict(classificationNeuralNetwork, newdata[, predictorNames], type = "class")
  }
  
  # Validazione incrociata
  trainControl <- trainControl(method = "cv", number = 20)  # K-Fold con k=20
  
  validationModel <- caret::train(
    formula,
    data = trainingData,
    method = "nnet",
    trControl = trainControl,
    tuneGrid = expand.grid(size = 10, decay = 0),
    maxit = 1000,
    linout = FALSE
  )
  
  # Accuratezza della validazione
  validationAccuracy <- max(validationModel$results$Accuracy) * 100
  
  # Risultati finali
  trainedClassifier <- list(
    model = classificationNeuralNetwork,
    predictFcn = predictFcn,
    validationAccuracy = validationAccuracy,
    predictorNames = predictorNames
  )
  
  return(trainedClassifier)
}

trainedClassifier <- neural_network_train(train_data)

df_shapley <- test_data

# Creare l'oggetto Predictor di iml
predictor <- Predictor$new(
  model = trainedClassifier$model,
  data = df_shapley[, predictorNames], 
  y = df_shapley$result
)

shapley <- Shapley$new(predictor, x.interest = df_shapley[1, predictorNames])

# Stampare i valori di Shapley
print(shapley)

# Assegniamo un colore univoco a ciascuna feature
unique_features <- unique(shapley$results$feature)
color_palette <- setNames(rainbow(length(unique_features)), unique_features)

# Creiamo il grafico
ggplot(shapley$results, aes(x = phi, y = feature, fill = feature)) +
  geom_col(color = "black") +  # `geom_col()` è preferibile a `geom_bar(stat = "identity")`
  facet_wrap(vars(class), ncol = 1, scales = "free_y") +  # Modifica qui
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 2),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "right"
  ) +
  labs(x = "Shapley Value", y = "Feature", title = "SHAP Values per Class")

# Convertire le previsioni in fattori con gli stessi livelli di train_data$result
train_predictions <- factor(predict(trainedClassifier$model, train_data[, predictorNames], type = "class"),
                            levels = levels(train_data$result))

test_predictions <- factor(predict(trainedClassifier$model, test_data[, predictorNames], type = "class"),
                           levels = levels(test_data$result))

# Creare le matrici di confusione
train_conf_matrix <- confusionMatrix(train_predictions, train_data$result)
test_conf_matrix <- confusionMatrix(test_predictions, test_data$result)


# Stampare le matrici di confusione
cat("📊 Matrice di Confusione - Training Set:\n")
print(train_conf_matrix)

cat("\n📊 Matrice di Confusione - Test Set:\n")
print(test_conf_matrix)

# Estrazione della matrice di confusione come tabella
train_conf_df <- as.data.frame(train_conf_matrix$table)
test_conf_df  <- as.data.frame(test_conf_matrix$table)

colnames(train_conf_df) <- c("Predicted", "Actual", "Freq")  # Rinominare le colonne
colnames(test_conf_df) <- c("Predicted", "Actual", "Freq")  # Rinominare le colonne

train_conf_df$Actual <- factor(train_conf_df$Actual, levels = rev(levels(train_conf_df$Actual)))
test_conf_df$Actual <- factor(test_conf_df$Actual, levels = rev(levels(test_conf_df$Actual)))

# Creazione della heatmap con ggplot2
ggplot(train_conf_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = ifelse(Freq == 0, "", Freq)), color = "black", size = 5) +  # Aggiunge i valori nella heatmap
  labs(title = "Matrice di Confusione - Training Set", x = "Predicted", y = "Actual") +
  theme_minimal()

# Creazione della heatmap con ggplot2
ggplot(test_conf_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = ifelse(Freq == 0, "", Freq)), color = "black", size = 5) +  # Aggiunge i valori nella heatmap
  labs(title = "Matrice di Confusione - Test Set", x = "Predicted", y = "Actual") +
  theme_minimal()

trainedClassifier$validationAccuracy