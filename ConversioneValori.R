data<-read.csv(file="C:\\Users\\39327\\Documents\\ProgettoSAD\\Autism-Child-Data.csv",sep=",",header=T)

#Eliminazione colonne age desc e relation
data<-subset(data,select=-c(age_desc,relation))

data$ethnicity[data$ethnicity=="?"]<-"Others"

#Sto togliendo gli apici singoli in alcuni valori di etnia
data$ethnicity<-gsub("'","",data$ethnicity)

##Sto togliendo gli apici singoli in alcuni valori di country of res
data$country_of_res<-gsub("'","",data$country_of_res)

#Sto cambiando il valore di United Arab Emirates in UAE
data$country_of_res[data$country_of_res=="United Arab Emirates"]<-"UAE"

#Sto convertendo i valori YES e NO in 0 ed 1 della colonna jundice
data$jundice[data$jundice=="yes"]<-1
data$jundice[data$jundice=="no"]<-0

#Sto convertendo i valori YES e NO in 0 ed 1 della colonna autism
data$autism[data$autism=="yes"]<-1
data$autism[data$autism=="no"]<-0

#Sto convertendo i valori YES e NO in 0 ed 1 della colonna used app before
data$used_app_before[data$used_app_before=="yes"]<-1
data$used_app_before[data$used_app_before=="no"]<-0

#Sto convertendo i valori YES e NO in 0 ed 1 della colonna classASD
data$ClassASD[data$ClassASD=="YES"]<-1
data$ClassASD[data$ClassASD=="NO"]<-0