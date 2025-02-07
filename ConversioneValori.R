# Carica il file CSV
data<-read.csv(file="C:/R/Autism-Child-Data.csv",sep=",",header=T)

# Eliminazione colonne age_desc e relation
data <- subset(data, select = -c(age_desc, relation))

# Sostituzione valori "?" in ethnicity
data$ethnicity[data$ethnicity == "?"] <- "Others"

# Rimozione apici singoli in ethnicity e country_of_res
data$ethnicity <- gsub("'", "", data$ethnicity)
data$country_of_res <- gsub("'", "", data$country_of_res)

# Sostituzione "United Arab Emirates" con "UAE"
data$country_of_res[data$country_of_res == "United Arab Emirates"] <- "UAE"

data<-data[data$result != 0, ]


# Conversione dei valori YES e NO in 0 e 1
data$jundice[data$jundice == "yes"] <- 1
data$jundice[data$jundice == "no"] <- 0
data$autism[data$autism == "yes"] <- 1
data$autism[data$autism == "no"] <- 0
data$used_app_before[data$used_app_before == "yes"] <- 1
data$used_app_before[data$used_app_before == "no"] <- 0
data$ClassASD[data$ClassASD == "YES"] <- 1
data$ClassASD[data$ClassASD == "NO"] <- 0
data$gender[data$gender == "m"] <- 0
data$gender[data$gender == "f"] <- 1

# Conversione della colonna Età in numerico (gestendo i valori non numerici)
data$age[data$age == "?"] <- NA  # Imposta i "?" a NA
data$age <- as.numeric(data$age)  # Converti in numerico, NA se non convertibile

# Identificazione delle righe valide (Età numerica e Genere maschile)
eta_maschile_valida <- data$age[data$gender == 0 & !is.na(data$age)]

# Calcolo della media delle età maschili valide
media_eta_maschile <- mean(eta_maschile_valida, na.rm = TRUE)

# Sostituzione dei NA per il genere maschile con la media calcolata
data$age[is.na(data$age) & data$gender == 0] <- media_eta_maschile

# Conversione della colonna Età in intero
data$age <- as.integer(data$age)

# Verifica se ci sono ancora NA
if (any(is.na(data$age))) {
  warning("Ci sono ancora NA nella colonna 'age' dopo le sostituzioni.")
}

print("Conversioni effettuate!")