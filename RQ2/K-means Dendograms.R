set.seed(8885)

library(cluster)
library(factoextra)
library(dendextend)

# 1. Caricare un dataset di esempio (o usa il tuo dataset)
file_path <- "C:/ASD-Child-main/ConversioneValori.R"

source(file_path)

cluster_variables <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score", 
                       "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score")

# 2. Normalizzazione dei dati
df_scaled <- scale(data[, cluster_variables])

# 3. Applicare PCA per ridurre la dimensionalità a 2 o 3 componenti
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
df_pca <- as.data.frame(pca_result$x[, 1:2])

# 4. Calcolare la matrice di distanza (Euclidea)
dist_matrix <- dist(df_pca, method = "euclidean")

# 5. Clustering gerarchico con metodo di collegamento Complete
hc_complete <- hclust(dist_matrix, method = "complete")

# 6. Generare e visualizzare il dendrogramma
# Creazione del dendrogramma dal clustering gerarchico
dend <- as.dendrogram(hc_complete)

# Definire i colori personalizzati per i rami
colors <- rep(c("cornflowerblue", "lightcoral", "lightseagreen"), length.out = length(labels(dend)))

# Applicare i colori ai rami del dendrogramma
dend <- dend %>%
  set("branches_k_color", colors, k = k) %>%  # Coloriamo i rami in base a 2 gruppi
  set("branches_lwd", 2) %>%  # Aumentiamo lo spessore dei rami per visibilità
  set("hang", 0) %>%  # Imposta il valore di hang desiderato
  set("labels_cex", 0.6)  # Impostiamo la dimensione delle etichette

# Plot del dendrogramma colorato
plot(dend, 
     main = "Dendrogramma - Complete Linkage", 
     cex = 0.4,       # dimensione delle etichette dei nodi
     cex.main = 1)    # dimensione del titolo

# 7. Determinare il numero ottimale di cluster (Elbow Method o Dendrogramma)
p <- fviz_nbclust(df_pca, FUN = hcut, method = "silhouette")

# Supponiamo che il grafico abbia una colonna 'y' con i valori della silhouette
# e che il numero ottimale di cluster corrisponda al massimo valore:
optimal_k <- p$data$clusters[which.max(p$data$y)]

# Personalizziamo il grafico:
p_custom <- p +
  geom_line(color = "cornflowerblue") +                # linea in cornflowerblue
  geom_point(color = "cornflowerblue", size = 3) +       # punti in cornflowerblue
  geom_vline(xintercept = optimal_k,                    # linea verticale per il k ottimale
             linetype = 2, color = "lightcoral") +         # in lightcoral
  labs(title = "Calcolo ottimale di k", x = "Numero di cluster k", y = "Ampiezza di silhoutte media")

p_custom

# 8. Tagliare l'albero per ottenere K cluster
k <- 2
clusters <- cutree(hc_complete, k = k)

# 9. Aggiungere i cluster ai dati e visualizzare i gruppi
df_pca$Cluster <- as.factor(clusters)

n_clusters <- length(unique(clusters))

# Definisci il vettore di colori in base al numero di cluster
colors <- rep(c("cornflowerblue", "lightcoral", "lightseagreen"), length.out = n_clusters)

fviz_cluster(list(data = df_pca[, 1:2], cluster = clusters), palette = colors)

# 10. Calcolare il Silhouette Score
silhouette_score <- silhouette(clusters, dist_matrix)
mean_silhouette <- mean(silhouette_score[, 3])
cat("Silhouette Score Medio:", mean_silhouette, "\n")

# 11. Visualizzare il diagramma del Silhouette Score
fviz_silhouette(silhouette_score, palette = colors) +
  labs(title = "Dendograms silhouette", x = "", y = "Ampiezza di silhoutte media") +
  annotate("text", x = 262, y = 0.46, label = "media = 0,42", size = 5, color = "black")

df_pca$result <- data$result

pca_table <- table(df_pca$Cluster, df_pca$result)

# Convertire la tabella in un dataframe long
df <- as.data.frame(as.table(pca_table))
colnames(df) <- c("Cluster", "result", "Frequenza")

# Creiamo una colonna binaria basata sul range di result
df$ClassASD <- ifelse(as.numeric(df$result) <= 6, 0, 1)

# Creazione della heatmap con colorazione basata sul range di result
ggplot(df, aes(x = as.factor(result), y = as.factor(Cluster), fill = as.factor(ClassASD))) +
  geom_tile(aes(alpha = Frequenza), color = "white") +  # Heatmap con trasparenza in base alla frequenza
  geom_text(aes(label = ifelse(Frequenza == 0, "", Frequenza)), color = "black", size = 5) +  # Mostra la frequenza solo se > 0
  scale_fill_manual(values = c("0" = "cornflowerblue", "1" = "lightcoral", "2" = "lightseagreen")) +  # Colorazione per range result
  theme_minimal() +
  labs(title = "Heatmap Frequenza di Dendograms con divisione ClassASD",
       x = "Result",
       y = "Cluster",
       fill = "ClassASD") +
  theme(legend.position = "right",
        panel.grid = element_blank())

df_pca$result <- data$result
df_pca <- as.data.frame(lapply(df_pca, as.numeric))

pca_table <- table(df_pca$Cluster, df_pca$result)

pca_table