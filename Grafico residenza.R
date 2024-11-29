# Carica il file CSV
data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

# Rimuovi le colonne delle variabili age_desc e relation
data <- subset(data, select = -c(age_desc, relation))

residenza_senza_apici <- gsub("'", "", data$country_of_res)
residenza_senza_apici[residenza_senza_apici == "United Arab Emirates"] <- "UAE"

# Conta le occorrenze dei campioni
residenza_interi <- table(residenza_senza_apici)

intervallo_residenza <- sort(unique(residenza_senza_apici))[residenza_interi > 5]

#Aggiungo il margine inferiore
par(mar = c(9, 4, 4, 2))

# Visualizza il file con View
altezza_barre <- barplot(residenza_interi[residenza_interi > 5], names.arg = intervallo_residenza, main = "Bar Plot sulla residenza",
                         ylab = "Quantità", ylim = c(0, 60), col = rainbow(length(residenza_interi[residenza_interi > 5])), las=2)

# Aggiungi l'etichetta manualmente
mtext("Residenza", side = 1, line = 7)

# Aggiungi i valori sopra le barre
text(x = altezza_barre, 
     y = residenza_interi[residenza_interi > 5] + 2,  # Posiziona il testo sopra la barra
     labels = residenza_interi[residenza_interi > 5], 
     cex = 0.8,  # Dimensione del testo
     col = "black")  # Colore del testo