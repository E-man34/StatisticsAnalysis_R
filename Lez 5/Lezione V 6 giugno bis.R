####### LEZIONE V 6 giugno
#######

####################################
##### Esercitazione su dati COVID-19

library("readxl")
library("zoo")
library("data.table")

#################
# Importazione dati


download.file('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv',destfile='italia.csv')
download.file('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv',destfile='regioni.csv')
download.file('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv',destfile = 'province.csv')
italia <- fread('italia.csv',fill=T)
regioni <- fread('regioni.csv',fill=T)
province <- fread('province.csv',fill=T)

# Eseguendo le 6 righe di comando precedenti si ottengono i file dei dati
# nella cartella di lavoro

# In alternativa rispetto all'importazione dei file tramite
# data.table

dati<-regioni
attach(dati)
write.csv2(regioni, file="Regioni1.csv")
detach(dati)

### Modificare le date cancellando l'ora manualmente

dati2<-read.csv2("Regioni1.csv", header=T)
attach(dati2)
str(dati2)

data<-as.factor(data)
data1<-as.Date(data, format="%d/%m/%Y")
str(data1)
dati2$data<-data1

#### PREPARAZIONE DATI
## Scelta eventuale periodo

data_1<-as.Date("2022-02-01")
data_2<-as.Date("2022-06-05")
tempi<-seq(data_1,data_2, by="days")
length(tempi)

data3<-subset(dati2, data>=data_1 & data<=data_2)
# nuovo dataframe che contiene solo le date selezionate

#####################
# Consideriamo in particolare i dati di nuovi_positivi
# del Piemonte (codice_regione==1), della Lombardia (codice_regione==3)
# e del Veneto (codice_regione==5)

detach(dati2)
attach(data3)

Piemonte_pos<-regioni$nuovi_positivi[regioni$codice_regione==1]
Lombardia_pos<-regioni$nuovi_positivi[regioni$codice_regione==3]
Veneto_pos<-regioni$nuovi_positivi[regioni$codice_regione==5]

# Se si vuole usare solo data3:

Piemonte_pos3<-nuovi_positivi[codice_regione==1]
Lombardia_pos3<-nuovi_positivi[codice_regione==3]
Veneto_pos3<-nuovi_positivi[codice_regione==5]





####  Effettuiamo come prima cosa un'analisi statistica descrittiva completa
# dei tre campioni

##### Piemonte

summary(Piemonte_pos)
length(Piemonte_pos)

summary(Piemonte_pos3)
length(Piemonte_pos3)

# anomalia: ci sono dati negativi, vanno eliminati

sum(Piemonte_pos<0)
Piemonte_pos<-Piemonte_pos[Piemonte_pos>=0]
length(Piemonte_pos)
summary(Piemonte_pos)
sd(Piemonte_pos)

plot(Piemonte_pos, type="l")
boxplot(Piemonte_pos)
hist(Piemonte_pos, freq=F)

plot(Piemonte_pos3, type="l")
boxplot(Piemonte_pos3)
hist(Piemonte_pos3, freq=F)


##### Lombardia

summary(Lombardia_pos)
sd(Lombardia_pos)

length(Lombardia_pos)

plot(Lombardia_pos, type="l")
boxplot(Lombardia_pos)
hist(Lombardia_pos, freq=F)

# copnfronto plot Lombardia, Piemonte
plot(Lombardia_pos, col="red", type="l")
lines(Piemonte_pos, col="blue")


##### Veneto

summary(Veneto_pos)
sd(Veneto_pos)

length(Veneto_pos)

plot(Veneto_pos, type="l")
boxplot(Veneto_pos)
hist(Veneto_pos, freq=F)

# confronto plot Lombardia, Piemonte, Veneto
plot(Lombardia_pos, col="red", type="l")
lines(Piemonte_pos, col="blue")
lines(Veneto_pos, col="green")

# confronto plot Piemonte, Veneto
plot(Piemonte_pos, col="red", type="l")
lines(Veneto_pos, col="blue")


####  Confrontiamo i valori medi e mediani di nuovi_positivi
# nelle tre regioni a coppie

t.test(Piemonte_pos, Lombardia_pos)
t.test(Piemonte_pos, Veneto_pos)
t.test(Veneto_pos, Lombardia_pos)
  
wilcox.test(Piemonte_pos, Lombardia_pos)
wilcox.test(Piemonte_pos, Veneto_pos)
wilcox.test(Veneto_pos, Lombardia_pos) 


#### Rappresentiamo graficamente i dati tramite ggplot2

library("ggplot2")
library("reshape2")

dati2<-data.frame(Piemonte_pos)
Hist<-ggplot(dati2, aes(Piemonte_pos, y=..density..))+
  geom_histogram(color="darkgreen",
  fill="green", binwidth=80)
Hist1<-Hist+labs(x="Value", y="Density")+ggtitle("Histogram")
Hist1

regioni<-as.data.frame(regioni)
dati3<-regioni[regioni$denominazione_regione=="Piemonte"|regioni$denominazione_regione=="Lombardia"|
                 regioni$denominazione_regione=="Veneto",]

dim(dati3)

ggDensity <- ggplot(data=dati3, aes(nuovi_positivi)) +
  geom_density(aes(fill=factor(denominazione_regione)), alpha=0.5) +
  labs(title="Nuovi positivi",
       x="no nuovi positivi",
       y="")
plot(ggDensity)


Hist<-ggplot(dati3, aes(nuovi_positivi, y=..density..))+
  geom_histogram(color="darkgreen",
  fill="green", binwidth=80)


Hist1<-Hist+facet_grid(~denominazione_regione)
Hist1

                                                                       
dati4<-data.frame(delta=Piemonte_pos)
Boxp<-ggplot(dati4, aes(y=delta))  +
  geom_boxplot(color="darkgreen",fill="green")
Boxp

Boxp<-ggplot(dati3, aes(denominazione_regione, nuovi_positivi, 
  fill=denominazione_regione))+
  geom_boxplot()
Boxp


Violp<-ggplot(dati4, aes(x="", y=delta))  +
  geom_violin(color="darkgreen",fill="green")
Violp

Violp<-ggplot(dati3, aes(denominazione_regione, nuovi_positivi, 
    fill=denominazione_regione))+geom_violin()
Violp


####### CONFRONTO DELLE INCIDENZE GIORNALIERE
####### (NUOVI POSITIVI DIVISI PER LA POPOLAZIONE X10^5)

pop_piem<-4433832
pop_ven<-4851181
pop_lomb<-9963188
inc_piem<-Piemonte_pos/pop_piem*10^5
inc_ven<-Veneto_pos/pop_ven*10^5
inc_lomb<-Lombardia_pos/pop_lomb*10^5

plot(inc_piem, col="red", type="l")
lines(inc_ven, col="blue")
lines(inc_lomb, col="green")


##### CALCOLO DELLA MEDIA MOBILE

pos7_piem <- rollmeanr(Piemonte_pos,7,fill=NA, 
                  align="center")
head(pos7_piem)
tail(pos7_piem)
pos7_piem<-pos7_piem[which(!is.na(pos7_piem))]
length(Piemonte_pos)
length(pos7_piem)
plot(pos7_piem, type="l", col="red", lwd=2)

pos7_ven <- rollmeanr(Veneto_pos,7,fill=NA, 
                       align="center")
pos7_ven<-pos7_ven[which(!is.na(pos7_ven))]

pos7_lomb<- rollmeanr(Lombardia_pos,7,fill=NA, 
                      align="center")
pos7_lomb<-pos7_lomb[which(!is.na(pos7_lomb))]

inc_piem_7<-pos7_piem/pop_piem*10^5
inc_ven_7<-pos7_ven/pop_ven*10^5
inc_lomb_7<-pos7_lomb/pop_lomb*10^5

plot(inc_piem_7, type="l", col="red", lwd=2, ylim=c(0,450))
lines(inc_ven_7, col="blue", lwd=2)
lines(inc_lomb_7, col="green", lwd=2)


######  TEST

# Si può affermare che l'incidenza media dei positivi negli ultimi
# 3 mesi sia stata la stessa in Piemonte e in Lombardia?

# E in lombardia e Veneto?



  
  
  





