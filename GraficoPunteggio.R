# Carica il file CSV
data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

# Rimuovi le colonne delle variabili age_desc e relation
data <- subset(data, select = -c(age_desc, relation))

# Prendi le prime 10 colonne relative ai punteggi
data <- data[, 1:10]

punteggio_interi<-c()

for (i in 1:10) {
  punteggio_interi[i] <- sum(data[, i]== 1)
}

intervallo_punteggi <- 1:10

altezza_barre <- barplot(punteggio_interi, xlab = "Punteggi",
                         ylab = "Quantità", ylim = c(0,230), col = rainbow(length(intervallo_punteggi)))

#Aggiungo manualmente le etichette dei punteggi sotto le barre
axis(1, at = altezza_barre, labels = intervallo_punteggi, tick = FALSE, line = -1) 

text(x = altezza_barre,
     y = punteggio_interi + 5,
     label = punteggio_interi,
     cex = 0.8,
     col = "black")
