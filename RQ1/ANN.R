# Riprodubicilità dell'esperimento
set.seed(8885)

# Caricamento le librerie dopo aver impostato il seed
library(keras)
library(tensorflow)
library(caret)
library(dplyr)

# Preprocessamento del dataset
file_path <- "C:/ASD-Child/ConversioneValori.R"

source(file_path)

# Definizione delle variabili
variables <- c("A2_Score", "A4_Score", "A7_Score", "A8_Score", "A9_Score")

percentage <- 0.70
epochs <- 100  # Numero di epoche
batch_size <- 16  # Dimensione del batch
neurons_per_layer <- c(64, 32, 16)  # Numero di neuroni per layer

# Creazione del train/test split
df <- data
df$result <- as.factor(df$result)  # Come fattore

train_index <- sample(1:nrow(df), percentage * nrow(df))

# Creazione del set di training e test
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Selezione delle variabili indipendenti
X_train <- as.matrix(train_data[, variables])  # Conversione in matrice
X_test <- as.matrix(test_data[, variables])    # Conversione in matrice

# Normalizzazione delle caratteristiche
X_train <- scale(X_train)  # Normalizzazione del training set
X_test <- scale(X_test)    # Normalizzazione del test set

# Conversione delle etichette
y_train <- as.integer(as.numeric(as.factor(train_data$result)) - 1)  # Conversione a interi
y_test <- as.integer(as.numeric(as.factor(test_data$result)) - 1)    # Conversione a interi

# Applicazione del one-hot encoding
num_classes <- length(unique(y_train))  # Calcolo del numero di classi
y_train <- to_categorical(y_train, num_classes)  # Conversione in one-hot encoding
y_test <- to_categorical(y_test, num_classes)    # Conversione in one-hot encoding

# Creazione del modello ANN con 3 hidden layers
model <- keras_model_sequential() %>%
  layer_dense(units = neurons_per_layer[1], activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dense(units = neurons_per_layer[2], activation = "relu") %>%
  layer_dense(units = neurons_per_layer[3], activation = "relu") %>%
  layer_dense(units = num_classes, activation = "softmax")  # Output layer per classificazione

# Compilazione del modello
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_sgd(lr = 0.01, momentum = 0.9, nesterov = TRUE),
  metrics = c("accuracy")
)

# Addestramento della rete neurale
history <- model %>% fit(
  X_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.2,
  verbose = 1
)

# Valutazione del modello sul set di test
test_loss_and_accuracy <- model %>% evaluate(X_test, y_test)

# Stampa dei risultati
cat("Test Accuracy:", test_loss_and_accuracy[2])

# Grafico della storia di addestramento
plot(history)

# Valutazione sul Training Set
train_predictions <- model %>% predict(X_train)
predicted_classes_train <- apply(train_predictions, 1, which.max) - 1  # Convertiamo in classi originali

# Salvataggio della matrice di confusione per il Training Set
confusion_matrix_train <- table(predicted_classes_train, apply(y_train, 1, which.max))
cat("\n🔹 Matrice di Confusione - Training Set:\n")
print(confusion_matrix_train)

# Valutazione sul Test Set
test_predictions <- model %>% predict(X_test)
predicted_classes_test <- apply(test_predictions, 1, which.max) - 1  # Convertiamo in classi originali

# Matrice di confusione per il Test Set
confusion_matrix_test <- table(predicted_classes_test, apply(y_test, 1, which.max))
cat("\n🔹 Matrice di Confusione - Test Set:\n")
print(confusion_matrix_test)

# Stampa dell'accuratezza finale sul Test Set
cat("Test Accuracy:", test_loss_and_accuracy[2])