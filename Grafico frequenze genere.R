# Calcolo delle frequenze assolute
freq_totale <- table(data$gender)
freq_asd <- table(data$gender[data$ClassASD == 1])

# Percentuali rispetto alla totalità del campione
perc_totale <- prop.table(freq_totale) * 100
perc_asd <- prop.table(freq_asd) * 100

perc_totale <- round(perc_totale, 2)
perc_asd <- round(perc_asd, 2)

# Frequenza relativa per Totale e ASD
freq_relativa_totale <- freq_totale / 2.92
freq_relativa_asd <- freq_asd / 2.92

freq_relativa_totale <- round(freq_relativa_totale)
freq_relativa_asd <- round(freq_relativa_asd)

# Creazione di una tabella riassuntiva
risultati <- data.frame(
  Genere = names(freq_totale),
  Totale_Frequenza = as.numeric(freq_relativa_totale),
  Totale_Percentuale = as.numeric(perc_totale),
  ASD_Frequenza = as.numeric(freq_relativa_asd),
  ASD_Percentuale = as.numeric(perc_asd)
)

# Mostra i risultati
print("Tabella riassuntiva:")
print(risultati)

# Visualizzazione delle differenze tramite grafico
altezza_barre <- barplot(
  t(as.matrix(risultati[, c("Totale_Frequenza", "ASD_Frequenza", "Totale_Percentuale", "ASD_Percentuale")])),
  beside = TRUE,
  names.arg = risultati$Genere,
  col = c("lightblue", "salmon"),
  main = "Distribuzione Percentuale per Genere",
  xlab = "Genere",
  ylab = "Percentuale",
  ylim = c(0, 100)
)

legend(
  "topleft",
  legend = c("Percentuale Totale", "Percentuale Positività ASD"),
  fill = c("lightblue", "salmon"),
  title = "Differenze",
  xpd = TRUE, 
  inset = c(0.01, 0.01),
)

text(x = altezza_barre,
     y =   t(as.matrix(risultati[, c("Totale_Frequenza", "ASD_Frequenza", "Totale_Percentuale", "ASD_Percentuale")])) +2,
     label =   t(as.matrix(risultati[, c("Totale_Frequenza", "ASD_Frequenza", "Totale_Percentuale", "ASD_Percentuale")])),
     cex = 0.8,
     col = "black")