# Carica le librerie necessarie
library(cluster)
library(fpc)

CH_min <- 0
CH_max <- 800

Silhouette_min <- -1
Silhouette_max <- 1

# Calcolo dell'indice Calinski-Harabasz
# La funzione cluster.stats() richiede una matrice delle distanze e un vettore di cluster
ch_stats <- cluster.stats(dist(features_eval), clusters_eval)
calinski_harabasz <- ch_stats$ch
print(paste("Calinski-Harabasz index:", calinski_harabasz))

# Calcolo dello silhouette score
# La funzione silhouette() calcola il coefficiente silhouette per ogni punto
sil <- silhouette(clusters_eval, dist(features_eval))
avg_silhouette <- mean(sil[, "sil_width"])
print(paste("Average Silhouette width:", avg_silhouette))

# Nome del file CSV per salvare i dati
file_name <- "valori_dendo.csv"

# Controlla se il file esiste già
if (file.exists(file_name)) {
  # Se esiste, carica i dati precedenti
  temp <- read.csv(file_name)
} else {
  # Se non esiste, crea un dataframe vuoto con due colonne
  temp <- data.frame(Cluster_Method = character(0), CH_Index = numeric(0), Mean_Silhouette = numeric(0))
}

# Verifica se ci sono già 2 metodi salvati (uno per ciascun clustering)
if (nrow(temp) >= 2) {
  # Se ci sono già due risultati, resetta il file
  temp <- data.frame(Cluster_Method = character(0), CH_Index = numeric(0), Mean_Silhouette = numeric(0))
}

# Sostituisci con i tuoi metodi di clustering gerarchico
cluster_method <- ifelse(nrow(temp) == 0, "Originale", "Generato")

# Calcola le metriche per il clustering attuale
ch_index <- calinhara(df_pca, clusters, cn = k)

# Normalizzazione del valore CH Index tra 0 e 100
ch_index_normalized <- (ch_index - CH_min) / (CH_max - CH_min) * 100
ch_index_normalized <- max(0, min(100, ch_index_normalized))

# Normalizzazione del valore Silhouette tra 0 e 100
silhouette_normalized <- (mean_silhouette_dendo - Silhouette_min) / (Silhouette_max - Silhouette_min) * 100
silhouette_normalized <- max(0, min(100, silhouette_normalized))

# Aggiungi il nuovo valore ai dati esistenti
temp <- rbind(temp, data.frame(Cluster_Method = cluster_method, CH_Index = ch_index_normalized, Mean_Silhouette = silhouette_normalized))

# Salva i dati aggiornati nel file CSV
write.csv(temp, file_name, row.names = FALSE)

# Se ci sono due clustering, crea il barplot di confronto
if (nrow(temp) == 2) {
  barplot(t(as.matrix(temp[, 2:3])), beside = TRUE, ylim = c(0, 100), col = c("cornflowerblue", "lightcoral"),
          legend.text = c("CH Index", "Mean Silhouette"),
          main = "Dendograms Confronto metriche originale-generato",
          names.arg = temp$Cluster_Method,  # Mostra i nomi dei metodi
          ylab = "Valore metriche", xlab = "Metodo di Clustering")
}