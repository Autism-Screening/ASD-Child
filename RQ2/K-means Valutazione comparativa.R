# Carica le librerie necessarie
library(cluster)

# Funzione per calcolare il coefficiente di silhouette medio per diversi k
silhouette_analysis <- function(k) {
  km <- kmeans(features, centers = k, nstart = 25)
  sil <- silhouette (km$cluster, dist(features))
  mean(sil[, 3]) # Restituisce il coefficiente di silhouette medio
}

# Eseguiamo il calcolo per k da 2 a 10 cluster
k_values <- 2
silhouette_scores <- sapply(k_values, silhouette_analysis)

# Centroidi dei cluster
centroids <- kmeans_result$centers

# Calcolare WCSS (Within-Cluster Sum of Squares)
wcss <- sum(kmeans_result$withinss)

# Calcolare BCSS (Between-Cluster Sum of Squares)
global_centroid <- colMeans (features)
cat("global_centroid", global_centroid, "\n")
bcss <- 0
for (i in 1:2) {
  cluster_points <- features[kmeans_result$cluster == i, ]
  cluster_size <- nrow(cluster_points)
  cluster_centroid <- centroids [i, ]
  bcss <- bcss + cluster_size* sum((cluster_centroid - global_centroid)^2)
}

# Calcolare la media normalizzata degli scores di Silhouette
mean_silhouette_all <- mean(silhouette_scores) * 100

# Calcolare l'indice di Calinski-Harabasz
n <- nrow(features)
k <- length(unique (kmeans_result$cluster))
ch_index <- (bcss/ (k-1)) / (wcss/ (n - k))

#Risultati
cat("Silhouette-score", mean_silhouette_all, "\n")
cat("WCSS:", wcss, "\n")
cat("BCSS:", bcss, "\n")
cat("Calinski-Harabasz Index:", ch_index, "\n")

# Nome del file CSV per salvare i dati
file_name <- "valori_barplot.csv"

CH_min <- 0
CH_max <- 800

Silhouette_min <- -1
Silhouette_max <- 1

# Controlla se il file esiste già
if (file.exists(file_name)) {
  # Se esiste, carica i dati precedenti
  temp <- read.csv(file_name)
} else {
  # Se non esiste, crea un dataframe vuoto con due colonne
  temp <- data.frame(CH_Index = numeric(0), Mean_Silhouette = numeric(0))
}

# Verifica se ci sono già 2 valori salvati
if (nrow(temp) >= 2) {
  # Se ci sono almeno 2 valori, resetta i dati
  temp <- data.frame(CH_Index = numeric(0), Mean_Silhouette = numeric(0))
}

# Sostituisci con i tuoi metodi di clustering gerarchico
cluster_method <- ifelse(nrow(temp) == 0, "Originale", "Generato")

# Calcola i nuovi valori (da sostituire con i tuoi veri calcoli)
ch_index <- (bcss / (k - 1)) / (wcss / (n - k))  # CH Index
mean_silhouette_all <- mean(silhouette_scores)  # Mean Silhouette

# Normalizzazione del valore CH Index tra 0 e 100
ch_index_normalized <- (ch_index - CH_min) / (CH_max - CH_min) * 100
ch_index_normalized <- max(0, min(100, ch_index_normalized))

# Normalizzazione del valore Silhouette tra 0 e 100
silhouette_normalized <- (mean_silhouette_all - Silhouette_min) / (Silhouette_max - Silhouette_min) * 100
silhouette_normalized <- max(0, min(100, silhouette_normalized))

# Aggiungi i nuovi valori ai dati esistenti
temp <- rbind(temp, data.frame(Cluster_Method = cluster_method, CH_Index = ch_index_normalized, Mean_Silhouette = silhouette_normalized))

# Salva i dati aggiornati nel file CSV
write.csv(temp, file_name, row.names = FALSE)

# Crea il barplot con la scala da 0 a 100
if (nrow(temp) == 2) {
  barplot(t(as.matrix(temp[, 2:3])), beside = TRUE, ylim = c(0, 100), col = c("cornflowerblue", "lightcoral"),
          legend.text = c("Calinski-Harabasz", "Media Silhouette"),
          main = "K-means Confronto metriche originale-generato",
          names.arg = temp$Cluster_Method,  # Mostra i nomi dei metodi
          ylab = "Valore metriche", xlab = "Clusters")
}