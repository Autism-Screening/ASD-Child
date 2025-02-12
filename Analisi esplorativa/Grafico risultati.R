# Carica il file CSV
data <- read.csv(file_path)

# Rimuovi le colonne delle variabili age_desc e relation
data <- subset(data, select = -c(age_desc, relation))

colori <- c("blue","blue","blue","blue","blue","blue","blue","red","red","red","red")

colori_legenda <- c("blue", "red")

testo_legenda <- c("negatività", "positività")

risultati <- data$result

intervallo_risultati <- c(0:10)

risultati_interi <- table(risultati)

altezza_barre <- barplot(risultati_interi, xlab = "Risultati", main = "Bar Plot sui risultati",
                         ylab = "Quantità", ylim = c(0,50), col = colori)

text(x = altezza_barre,
     y = risultati_interi + 2,
     label = risultati_interi,
     cex = 0.8,
     col = "black")

segments(x0 = altezza_barre[7] + 0.6, y0 = risultati_interi[7] + 10,
         x1 = altezza_barre[8] - 0.6, y1 = 0, col = "red", lwd = 2)

text(x = altezza_barre[7] + 2,
     y = risultati_interi[7] + 8,
     label = "Soglia di positività",
     cex = 0.8,
     col = "black")

legend("topleft", legend = testo_legenda, fill = colori_legenda, title = "Status", inset = c(0.01, 0.01), cex=0.9)