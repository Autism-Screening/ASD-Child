set.seed(8885)

# Caricare i pacchetti
library(dbscan)
library(ggplot2)
library(dplyr)
library(tidyr)

# Preprocessamento del dataset
file_path <- "C:/ASD-Child-main/ConversioneValori.R"

source(file_path)

eps <- 2.4
minPts <- 11

# Selezionare solo le colonne An_Score (caratteristiche comportamentali)
score_columns <- paste0("A", 1:10, "_Score")
features <- data[, score_columns]

# Normalizzare i dati (opzionale, ma utile per DBScan)
features_scaled <- scale(features)

# Esegui PCA
pca_result <- prcomp(features_scaled, center = TRUE, scale. = TRUE)
df_pca <- as.data.frame(pca_result$x)

# Applicare DBScan (tuning necessario per eps e minPts)
db <- dbscan::dbscan(df_pca, eps = eps, minPts = minPts)

# Aggiungere i cluster al dataset originale
data$Cluster <- as.factor(db$cluster)

# Scegli un valore di k = minPts - 1
k <- 9
kNNdistances <- kNNdist(features_scaled, k = k)

# Plotta le distanze ordinate con l'estetica di ggplot
distance_data <- data.frame(Punti = 1:length(kNNdistances), Distanza = sort(kNNdistances))

ggplot(distance_data, aes(x = Punti, y = Distanza)) +
  geom_line(color = "cornflowerblue", size = 1) +  # Imposta il colore e la dimensione della linea
  theme_minimal() +  # Usa un tema minimalista
  labs(title = "Grafico del gomito per eps",
       x = "Punti ordinati",
       y = "Distanza k-NN") +
  theme(legend.position = "right")

# Visualizzare la distribuzione dei cluster rispetto a ClassASD con colori personalizzati
ggplot(data, aes(x = result, y = Cluster, color = as.factor(ClassASD))) +
  geom_jitter(size = 2, alpha = 0.7) +  # Scatter plot con jitter per evitare sovrapposizioni
  scale_color_manual(values = c("0" = "cornflowerblue", "1" = "lightcoral")) +  # Assegna colori personalizzati
  theme_minimal() +
  labs(title = "Clustering con DBScan e confronto con ClassASD",
       x = "Somma dei punteggi (result)",
       y = "Cluster",
       color = "ClassASD") +
  theme(legend.position = "right")

# Creazione della tabella da data
dbscan_table <- table(data$Cluster, data$result)

dbscan_table

# Convertire la tabella in un dataframe long
df <- as.data.frame(as.table(dbscan_table))
colnames(df) <- c("Cluster", "result", "Frequenza")

# Creiamo una colonna binaria basata sul range di result
df$ClassASD <- ifelse(as.numeric(df$result) <= 6, 0, 1)

# Creazione della heatmap con colorazione basata sul range di result
ggplot(df, aes(x = as.factor(result), y = as.factor(Cluster), fill = as.factor(ClassASD))) +
  geom_tile(aes(alpha = Frequenza), color = "white") +  # Heatmap con trasparenza in base alla frequenza
  geom_text(aes(label = ifelse(Frequenza == 0, "", Frequenza)), color = "black", size = 5) +  # Mostra la frequenza solo se > 0
  scale_fill_manual(values = c("0" = "cornflowerblue", "1" = "lightcoral")) +  # Colorazione per range result
  theme_minimal() +
  labs(title = "Heatmap Frequenza di DBSCAN con divisione ClassASD",
       x = "Result",
       y = "Cluster",
       fill = "ClassASD") +
  theme(legend.position = "right",
        panel.grid = element_blank())