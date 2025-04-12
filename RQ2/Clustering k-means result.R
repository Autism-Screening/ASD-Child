set.seed(8885)

library(ggplot2)
library(cluster)

# Preprocessamento del dataset
file_path <- "C:/ASD-Child-main/ConversioneValori.R"

source(file_path)

cluster_variables <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", 
                       "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score")

centers <- 2

nstart <- 25

# Prendere solo le prime 10 variabili che corrispondono alle caratteristiche comportamentali
data_subset <- data[, cluster_variables]

# Standardizzare le caratteristiche comportamentali (An_Score)
features <- scale(data_subset)

# Eseguire k-means clustering
kmeans_result <- kmeans(features, centers = centers, nstart = nstart)

# Aggiungere i cluster al dataset
data_subset$cluster <- as.factor(kmeans_result$cluster)

# Calcolare le distanze dai centroidi
centroids <- kmeans_result$centers
distances_to_centroids <- apply(features, 1, function(row) {
  sqrt(rowSums((t(centroids) - row)^2))
})

# Trasporre la matrice per ottenere distanze separate per cluster
distances_to_centroids <- t(distances_to_centroids)

# Aggiungere le distanze come colonne al dataset
data_subset$distance_cluster_1 <- distances_to_centroids[, 1]
data_subset$distance_cluster_2 <- distances_to_centroids[, 2]

# Calcolare il Silhouette Score
silhouette_result <- silhouette(kmeans_result$cluster, dist(features))
mean_silhouette_all <- mean(silhouette_result[, 3])

data_subset$result <- data$result

# Tabella di contingenza tra cluster e result
contingency_asd <- table(data_subset$cluster, data_subset$result)

# Creare un dataframe per ggplot2
df <- data.frame(
  Cluster = factor(rownames(contingency_asd)),
  Freq = cluster_totals
)

# Creare il grafico a torta con etichette spostate
ggplot(df, aes(x = "", y = Freq, fill = Cluster)) +  # Convertiamo Cluster in fattore
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Grafico a torta
  geom_text(aes(label = paste("Cluster", Cluster, "\n", Freq)), 
            position = position_stack(vjust = 0.6), 
            size = 5) +
  scale_fill_manual(values = rep(c("cornflowerblue", "lightcoral"), length.out = length(unique(df$Cluster)))) +  # Alterna colori
  labs(title = "Distribuzione dei Cluster") +
  theme_void()

# Eseguire PCA
pca_result <- prcomp(features, center = TRUE, scale. = TRUE)

# Creare un dataframe per il grafico
pca_data <- data.frame(PC1 = pca_result$x[, 1], 
                       PC2 = pca_result$x[, 2], 
                       cluster = as.factor(data_subset$cluster)  # Convertiamo cluster in fattore
                      )
                       
# Calcolare i centroidi nello spazio PCA
centroids_pca <- pca_data %>%
  group_by(cluster) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2))

palette_colors <- rep(c("cornflowerblue", "lightcoral"), length.out = length(unique(pca_data$cluster)))

# Creare il grafico PCA
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +  # Nuvola di punti con trasparenza
  geom_point(data = centroids_pca, aes(x = PC1, y = PC2), color = "black", shape = 4, size = 5, stroke = 2) +  # Centroidi in rosso
  scale_color_manual(values = palette_colors) +  # Applicare la palette
  labs(title = "Nuvola di Punti del Cluster (PCA) con Centroidi",
       x = "Prima Componente Principale",
       y = "Seconda Componente Principale",
       color = "Cluster") +
  theme_minimal()

# Creare un dataframe dalla tabella di contingenza
contingency_df <- as.data.frame(contingency_asd)

df <- as.data.frame(as.table(dbscan_table))

# Rinominare le colonne per ggplot
colnames(contingency_df) <- c("Cluster", "result", "Frequenza")

# Creiamo una colonna binaria basata sul range di result
contingency_df$ClassASD <- ifelse(as.numeric(contingency_df$result) <= 6, 0, 1)

# Creazione della heatmap con colorazione basata sul range di result
ggplot(contingency_df, aes(x = as.factor(result), y = as.factor(Cluster), fill = as.factor(ClassASD))) +
  geom_tile(aes(alpha = Frequenza), color = "white") +  # Heatmap con trasparenza in base alla frequenza
  geom_text(aes(label = ifelse(Frequenza == 0, "", Frequenza)), color = "black", size = 5) +  # Mostra la frequenza solo se > 0
  scale_fill_manual(values = c("0" = "cornflowerblue", "1" = "lightcoral")) +  # Colorazione per range result
  theme_minimal() +
  labs(title = "Heatmap della Tabella di Contingenza",
       x = "Result",
       y = "Cluster",
       fill = "ClassASD") +
  theme(legend.position = "right",
        panel.grid = element_blank())

cat("Silhouette Score medio (intero dataset):", mean_silhouette_all, "\n")

cat("Tabella di contingenza tra cluster e result:\n")

print(contingency_asd)