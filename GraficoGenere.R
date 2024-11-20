# Carica il file CSV
data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

# Rimuovi le colonne delle variabili age_desc e relation
data <- subset(data, select = -c(age_desc, relation))

# Campiona solamente i soggetti affetti dal disturbo dello spettro autistico
genere <- data$gender[data$ClassASD == "YES"]

intervallo_genere <- c("femmine", "maschi")

# Conta le occorrenze dei campioni
genere_interi <- table(genere)

genere_totale <- data$gender
genere_interi_totale <- table(genere_totale)

#altezza_barre <- barplot(genere_interi, names.arg = intervallo_genere, main = "Bar Plot sul genere",
                        # xlab = "Genere", ylab = "Quantità", ylim = c(0, 220), col = rainbow(length(genere_interi)))

colori_legenda<-c("orange","limegreen")

altezza_torta <- pie(genere_interi, labels = genere_interi, main = "Grafico sul numero di positivi", col = colori_legenda)

legend("topleft", legend = intervallo_genere, fill = colori_legenda, title = "Genere", inset = c(0.01, 0.01))


text(x = altezza_barre, 
     y = genere_interi + 6,  # Posiziona il testo sopra la barra
     labels = genere_interi, 
     cex = 0.8,  # Dimensione del testo
     col = "black")  # Colore del testo