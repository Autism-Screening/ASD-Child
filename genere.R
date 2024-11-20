data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

data<-subset(data,select=-c(age_desc,relation))

residenza_senza_apici<-gsub("'","",data$country_of_res)

genere<-data$gender

genere_interi<-table(genere)

intervallo_genere<-c("femmine","maschi")

#Visualizza il file con la view
#altezza_barre<-barplot(genere_interi,names.arg=intervallo_genere,main="BarPlot sul genere",xlab="Genere",ylab="Quantità",ylim=c(0,220),col=rainbow(length(genere_interi)))

colori_legenda<-c("orange","limegreen")

altezza_torta <- pie(genere_interi, labels = genere_interi, main = "Grafico totale sul genere", col = colori_legenda)

legend("topleft", legend = intervallo_genere, fill = colori_legenda, title = "Genere", inset = c(0.01, 0.01))

#Aggiungere i vvalori sopra le barre
text(x=altezza_barre,y=genere_interi+2,labels=genere_interi,cex=0.8,col="black")
