# Riprodubicilità dell'esperimento
set.seed(8885)

# Caricamento le librerie dopo aver impostato il seed
library(keras)
library(tensorflow)
library(caret)
library(dplyr)
library(stringr)

# Preprocessamento del dataset
source(file_path)

i <- 0

# Definizione delle variabili
variables <- c("A1_Score", "A2_Score", 
               "A6_Score", "A7_Score", "A8_Score", "age")

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
X_train <- scale(X_train)  # Normalizzazione il training set
X_test <- scale(X_test)    # Normalizzazione il test set

# Conversione delle etichette
y_train <- as.integer(as.numeric(as.factor(train_data$result)) - 1)  # Conversione a interi
y_test <- as.integer(as.numeric(as.factor(test_data$result)) - 1)    # Conversione a interi

# Applicazione del one-hot encoding
num_classes <- length(unique(y_train))  # Calcolo del numero di classi
y_train <- to_categorical(y_train, num_classes)  # Conversione in one-hot encoding
y_test <- to_categorical(y_test, num_classes)    # Conversione in one-hot encoding

for (i in 1:20) {
  # Creazione del modello ANN con 3 hidden layers
  model <- keras_model_sequential() %>%
    layer_dense(units = neurons_per_layer[1], activation = "relu", input_shape = ncol(X_train)) %>%
    layer_dense(units = neurons_per_layer[2], activation = "relu") %>%
    layer_dense(units = neurons_per_layer[3], activation = "relu") %>%
    layer_dense(units = num_classes, activation = "softmax")  # Output layer per classificazione
  
  # Compilazione del modello
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
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
  cat(test_loss_and_accuracy[2], file="C:/ASD-Child/ANN-result.txt", append =TRUE, sep = "\n")
}

# Lettura del file riga per riga e rispettivo salvataggio in una lista
lines <- readLines("C:/ASD-Child/ANN-result.txt")

# Funzione per estrarre numeri da una riga
estrai_numeri <- function(riga) {
  numeri <- as.numeric(str_extract_all(riga, "-?\\d+\\.?\\d*")[[1]])  # Estrai numeri decimali
  return(numeri)
}

# Applicazione della funzione a ogni riga e salvataggio dei risultati in una lista
lista_numeri <- lapply(lines, estrai_numeri)

# Salvataggio dei numeri in un unico vettore
tutti_numeri <- unlist(lista_numeri)

# Calcolo della media totale
media_totale <- mean(tutti_numeri, na.rm = TRUE)

# Stampa della media
print(media_totale)