data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

data<-subset(data,select=-c(age_desc,relation))

etnia<-data$ethnicity

etnia_senza_apici<-gsub("'","",etnia)

#Convertiamo i ? in Others
etnia_senza_apici[etnia_senza_apici=="?"]<-"Others"

#Conta le occorrenze dei campioni
etnia_interi<-table(etnia_senza_apici)

intervallo_etnia<-sort(unique(etnia_senza_apici))

#Visualizza il file con la view
altezza_barre<-barplot(etnia_interi,names.arg=intervallo_etnia,main="BarPlot sull'etnia",xlab="Etnia",ylab="Quantità",ylim=c(0,120),col=rainbow(length(etnia_interi)))

#Aggiungere i vvalori sopra le barre
text(x=altezza_barre,y=etnia_interi+2,labels=etnia_interi,cex=0.8,col="black")

print(etnia)