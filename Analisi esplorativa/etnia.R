data<-read.csv(file=file_path,sep=",",header=T)

data<-subset(data,select=-c(age_desc,relation))

etnia<-data$ethnicity

etnia_senza_apici<-gsub("'","",etnia)

#Convertiamo i ? in Others
etnia_senza_apici[etnia_senza_apici=="?"]<-"Others"

#Conta le occorrenze dei campioni
etnia_interi<-table(etnia_senza_apici)

intervallo_etnia<-sort(unique(etnia_senza_apici))

# Ordina le frequenze in ordine decrescente
etnia_interi <- etnia_interi[order(etnia_interi, decreasing = TRUE)]

#Aggiungo il margine inferiore
par(mar = c(9, 4, 4, 2))

#Visualizza il file con la view
altezza_barre<-barplot(etnia_interi,names.arg=names(etnia_interi),main="BarPlot sull'etnia", ylab="Quantità",ylim=c(0,120), col=rainbow(length(etnia_interi)),las=2)

# Aggiungi l'etichetta manualmente
mtext("Etnia", side = 1, line = 7)

#Aggiungere i vvalori sopra le barre
text(x=altezza_barre,y=etnia_interi+2,labels=etnia_interi,cex=0.8,col="black")

