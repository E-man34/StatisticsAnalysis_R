library("sas7bdat") #leggere file SAS
library("ggplot2")

#SETTARE PRIMA DI TUTTO LA WORKING DIRECTORY

dati<-read.sas7bdat("italia_18.sas7bdat") # comando per leggere il dataset SAS, viene salvato in un dataframe chiamato "dati"
dati[dati=="NaN"]<-NA # "NaN" non va bene per R, perchè in R i dati mancanti sono identificati con NA 



                                    # CONCENTRIAMOCI SOLO SU ALCUNE VARIABILI #


# Tempo trascorso nella lettura di news (nwspol)
str(dati$nwspol) # vediamo che nwspol è una var di numeri

#dati$nwspol<-as.numeric(dati$nwspol) # trasformazione in numeri


# Tempo trascorso nella consultazione di Internet (netustm)
str(dati$netustm)

#dati$netustm<-as.numeric(dati$netustm)



                            ##### Analisi statistica descrittiva per VAR QUANTITATIVE


# 1- calcolo degli indici (summary, deviazione standard, coefficente di variazione)

summary(dati$nwspol)

range(dati$nwspol, na.rm=T) # range rimuovendo i valori nulli

IQR(dati$nwspol, na.rm=T) # scarto interquantile = differenza tra primo e terzo quartile rimuovendo valori nulli


# PRIMO QUARTILE: valore > o = del 25% dei dati
# SECONDO QUARTILE: valore > o = del 75% dei dati (mediana)
# TERZO QUARTILE: valore > o = del 75% dei dati


sd(dati$nwspol, na.rm=T) # deviazione standard

cv<-sd(dati$nwspol, na.rm=T)/mean(dati$nwspol, na.rm=T) # coefficente di variazione = rapporto tra deviazione standard e valore assoluto della media (cv è sempre positivo)
# più è piccolo, minore è la dispersuone dei dati intorno alla media, e viceversa
# se > di 1 c'è una buona dispersione






# 2- rappresentazione grafica

# Istogramma = rappresentazione campionaria della densità della var
hist(dati$nwspol, freq=F, col="light blue", main="Istogramma di Nwspol", xlab="Nwspol", ylab="Densità approssimata", breaks=30) #breaks è un indicazione del numero di bins (classi)


# Boxplot = rappresentazione campionaria della distribuzione (primo-terzo quartile, mediana)
boxplot(dati$nwspol, col="light green", main="Boxplot di Nwspol", ylab="Nwspol") # il box verde contiene una linea nera che rappresenta la mediana


# Q-Q plot = grafico quantile-quantile per stabilire graficamente se i dati si possono considerare estratti da una distribuzione normale o no 
# data una var normale x con un tot di media e di varianza, si può passare a una var normale standard Z (media = 0 e varianza = 1) tramite questa trasformazione: Z=(x-mu)/sigma --> x= sigma*z + mu 
qqnorm(dati$nwspol)
qqline(dati$nwspol, col="red")
#questo Q-Q plot ha una coda molto lunga, l'ideale di Q-Q plot è una bisettrice retta che taglia in due il piano (come si vede da qqline che mostra come dovrebbe essere)


                # finestra grafica per mettere insieme più grafici

par(mfrow=c(1,3)) # 3 grafici su 1 riga 






                  ##### Analisi statistica descrittiva per VAR QUALITATIVE


# Implicazione dei cittadini nella politica (dal minore al maggiore)
dati$psppsgva<-as.factor(dati$psppsgva)
str(dati$psppsgva)



# 1 - tabella delle frequenze

t1<-table(dati$psppsgva)
t1 # freq assolute delle persone implicate nella politica (ma mancano i valori mancanti!!)
sum(is.na(dati$psppsgva)) # conta i mancanti
sum(!is.na(dati$psppsgva)) # con i non mancanti

#frequenza di chi ha risposto al questionario
t2<-(t1/sum(t1))*100 #abbiamo escluso dal totale i mancanti
t2



# 2 - grafico
barplot(t1, col=rainbow(12)) #freq assoluta
barplot(t2, col=rainbow(5), main="Psppsgva", ylab="Frequenza relativa") #freq relative
pie(t2, col=rainbow(8))






                                              ######### ESERCIZIO #########

#eseguire analisi statistica descrittiva completa delle var NETUSTM (anal. quantitativa) e POLINTR (anal. qualitativa)




                                                    # NETUSTM

# 1- calcolo indici

str(dati$netustm)

summary(dati$netustm) # fornisce una serie di info sulla var specificata --> ci sono tantissimi NA (1064 è quasi la metà), la media è di 165 e la mediana di 120 (non sono molto vicine, significa che la distribuzione potrebbe essere simmetrica)

range(dati$netustm, na.rm=T) # range

IQR(dati$netustm, na.rm=T) # scarto interquantile = 3 quart - 1 quart + rimoz NA


# PRIMO QUARTILE: valore > o = del 25% dei dati
# SECONDO QUARTILE: valore > o = del 75% dei dati (mediana)
# TERZO QUARTILE: valore > o = del 75% dei dati


sd(dati$netustm, na.rm=T) # deviazione standard

cv<-sd(dati$netustm, na.rm=T)/mean(dati$netustm, na.rm=T) # coefficente di variazione 
# più è piccolo, minore è la dispersuone dei dati intorno alla media, e viceversa (< di 1) --> non c'è una grande dispersione

  

                                    # rappresentazione grafica

# Istogramma
hist(dati$netustm, freq=F, col="gold", main="Istogramma di Netustm", xlab="Netustm", ylab="Densità approssimata", breaks=20)


# Boxplot
boxplot(dati$netustm, col="red", main="Boxplot di Netustm", ylab="Netustm") #coda a sx quasi inesistenete, mentre coda a dx molto lunga, outliers non sono moltissimi (pallini)


# Q-Q plot
qqnorm(dati$netustm, main="Q-Q plot di Netustm") # la curva del qq plot mostra che non è una distribuzione normale --> i gradini mostrano che ci sono tanti valori ripetuti nei dati, perchè quando il valore Y cambia, siamo già a un quantile diverso
qqline(dati$netustm, col="red")


# finestra grafica per mettere insieme più grafici

par(mfrow=c(1,3)) # 3 grafici su 1 riga 


#CONCLUSIONE = distribuzione dei dati non è molto dispersa dalla media, distribuzione non è per niente normale



                                              #POLINTR

dati$polintr<-as.factor(dati$polintr) #trasformare in fattore
str(dati$polintr)



# 1 - tabella delle frequenze

t1<-table(dati$polintr)
t1 #mancano i NA

#summary (di un fattore!) restituisce anche gli NA, table no
sum(is.na(dati$polintr)) # conta NA
sum(!is.na(dati$polintr)) # con i non NAN

#frequenza di chi ha risposto al questionario
t2<-(t1/sum(t1))*100 #abbiamo escluso dal totale i mancanti
t2



# 2 - grafico
barplot(t1, col=rainbow(12), main = "Polintr", names.arg=c("Tanto","Abbastanza","Poco","Niente","Mancanti")) #freq assoluta
barplot(t2, col=rainbow(5), main="Polintr", ylab="Frequenza relativa", names.arg=c("Tanto","Abbastanza","Poco","Niente","Mancanti")) #freq relative
pie(t1, col=rainbow(8), main = "Polintr", labels=c("Tanto","Abbastanza","Poco","Niente","Mancanti"))



