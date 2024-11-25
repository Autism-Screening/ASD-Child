# Carica il file CSV
data <- read.csv("Autism-Child-Data.csv")

# Rimuovi le colonne delle variabili age_desc e relation
data <- subset(data, select = -c(age_desc, relation))

ittero <- data$jundice

positività <- data$ClassASD

ittero_positivo <- table(ittero, positività)

label_ittero <- c("Assenza di ittero", "Presenza di ittero")

legenda_positività <- c("positivi", "negativi")

somma_positivi <- colSums(ittero_positivo)

colori_legenda <- c("orange", "limegreen")

altezza_barre <- barplot(ittero_positivo, main="Correlazione ittero e ASD",
                         col=colori_legenda, names.arg = c("", ""), ylim = c(0, max(somma_positivi) + 50))  # Nasconde gli assi
text(x = altezza_barre,
     y = somma_positivi + 5,
     label = somma_positivi,
     cex = 0.8,
     col = "black")

axis(1, at = altezza_barre, labels = label_ittero, tick = FALSE, line = -1)

legend(
  "topright",
  legend = legenda_positività,
  fill = colori_legenda,
  title = "Positività",
  xpd = TRUE
)