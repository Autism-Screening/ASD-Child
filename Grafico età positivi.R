# Carica il file CSV
data <- read.csv("Autism-Child-Data.csv")

data <- subset(data, select = -c(age_desc, relation))

eta <- data$age
eta_positivi <- data$age[data$ClassASD=="YES"]

intervallo_eta <- c(4:11) # Intervallo di età

# Converti "?" in NA
eta[eta == "?"] <- NA

# Converte i valori a numerico
eta <- as.numeric(eta)

# Rimuovi i valori non interi
eta_interi <- table(eta[!is.na(eta)])

# Crea il boxplot per la variabile "ClassASD"
boxplot(eta ~ ClassASD, data = data, 
        main = "Distribuzione dell'età per ASD (ClassASD)",
        xlab = "Classificazione ASD", 
        ylab = "Età",
        col = c("skyblue", "salmon"),
        names = c("Negativi", "Positivi"))

# Aggiungi media dell'età per ciascun gruppo
means <- tapply(eta, data$ClassASD, mean, na.rm = TRUE)
points(1:length(means), means, col = "darkblue", pch = 18, cex = 1.5)

print(means)

text(x = altezza_barre,
     y = punteggio_interi + 5,
     label = punteggio_interi,
     cex = 0.8,
     col = "black")