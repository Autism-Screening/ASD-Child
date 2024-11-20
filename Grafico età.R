data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

#Rimozione delle colonne age_desc relation poichè contengono informazioni ridondanti
data<-subset(data,select=-c(age_desc,relation))

print(data$age)

età_ordinata<-data$age

range_età<-c(4:11) #sto rappresentando il range dell'età

#Sto convertendo i ? in NA
età_ordinata[età_ordinata=="?"]<-NA

#Converte i valori in numeri
età_ordinata<-as.numeric(età_ordinata)

#Rimozione dei valori non interi
età_interi<-table(età_ordinata[!is.na(età_ordinata)])

altezza_barre<-barplot(età_interi, names.arg=range_età, main="BarPlot dell'età", xlab="Età",ylab="Valori",ylim=c(0,100),col=rainbow(length(età_interi)))

#Aggiunta dei valori sopra le barre
text(x=altezza_barre,y=età_interi+2,labels=età_interi,cex=0.8,col="black")