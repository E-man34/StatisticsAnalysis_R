####### LEZIONE VII 9 giugno
#######

##################################
##################################
##### Esercitazione su dati COVID-19

library("readxl")
library("zoo")
library("data.table")
library("ggplot2")

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

###### PARTIRE DIRETTAMENTE DA QUI SE SIMPORTANO I DATI
# DEL FILE REGION1.CSV CREATO PER ELIMINARE L'ORA

dati2<-read.csv2("Regioni1.csv", header=T)
attach(dati2)
str(dati2)

data<-as.factor(data)
data1<-as.Date(data, format="%d/%m/%Y")
str(data1)
dati2$data<-data1


######################
###### ANALISI DELLA VARIANZA
#####################

# Consideriamo in particolare i dati di nuovi_positivi
# del Piemonte (codice_regione==1), della Lombardia (codice_regione==3)
# e del Veneto (codice_regione==5)

Piemonte_pos<-nuovi_positivi[codice_regione==1]
Lombardia_pos<-nuovi_positivi[codice_regione==3]
Veneto_pos<-nuovi_positivi[codice_regione==5]


####### DOMANDA: 
# Possiamo dire che la media del numero di nuovi positivi giornalieri
# nelle 3 regioni sia la stessa o ci sono delle differenze tra le medie
# nelle regioni considerate?

dati_pos<-c(Piemonte_pos, Lombardia_pos, Veneto_pos)
leg_pos<-rep(c("P","L","V"),c(length(Piemonte_pos),
              length(Lombardia_pos),length(Veneto_pos)))
# Per eseguire l'ANOVA in R è necessario creare un vettore unico con
# tutti i valori numerici e un altro vettore unico con i nomi
# dei gruppi che corrispondono ai singoli campioni

#### Step 1. Test di Bartlett
# H0: sigma2_P = sigma2_V = sigma2_L
# H1: le tre varianze non sono uguali

bartlett.test(dati_pos~leg_pos)
# p-value < 2.2e-16, l'ipotesi H0 va rifiutata, NON potremmo usare 
# l'ANOVA

#### Step 2. Verifica della normalità di ogni campione

qqnorm(Piemonte_pos)
# l'ipotesi di normalità non è verificata (basta che non lo sia
# anche per uno solo dei campioni), quindi NON potremmo usare
# l'ANOVA.
# PER ESERCIZIO procediamo ugualmente anche se non è corretto


anova(lm(dati_pos~leg_pos))
# H0: mu_pos_P = mu_poS_v = mu_pos_L
# H1: almeno una coppia di medie non siano uguali

# Analysis of Variance Table
# 
# Response: dati_pos
#              Df     Sum Sq   Mean Sq   F value    Pr(>F)    
# leg_pos      2  283054576    141527288  63.358    < 2.2e-16 ***
# Residuals  1407 3142912524   2233769      

# p-va.ue < 2.2e-16, quindi H0 va rifiutata

# Se avessimo potuto utilizzare l'ANOVA, avendo un risultato significativo
# procederemmo con il summary per ottenere i coefficienti corrispondenti
# ai vari livelli del trattamento e la loro significatività

summary(lm(dati_pos~leg_pos))
# l'interecetta è la media del livello preso come riferimento,
# qui leg_posL
# Gli altri coefficienti indicano la variazione nella media tra 
# il livello a cui si riferiscono e il primo

# Se avessimo potuto utilizzare l'ANOVA, avendo un risultato significativo
# procederemmo con il pairwise.t.test per confrontare le medie a coppie
# con la correzione del p-value per evitare l'amplificazione
# dell'errore di I specie

pairwise.t.test(dati_pos, leg_pos, p.adj="bonferroni")

#### Test non parametrico di Kruskal-Wallis

kruskal.test(dati_pos~leg_pos)
# H0: mediana_posP = mediana_posV = mediana_posL
# H1: le mediane non sono tutte uguali per i tre gruppi
# p-value < 2.2e-16, rifiutiamo H0

tapply(dati_pos,leg_pos,mean,na.rm=T)
tapply(dati_pos,leg_pos,median,na.rm=T)

# Dopo il test non parametrico può essere utile rappresentare
# le distribuzioni tramite boxplot affiancati

boxplot(dati_pos~leg_pos, col=rainbow(12))



######################
###### REGRESSIONE LINEARE

####  Domanda giocattolo:
# è possibile pensare all'esistenza di relazioni di tipo lineare
# tra qualcuna delle variabili comprese nel dataset?

posp<-nuovi_positivi[codice_regione==1]
posl<-nuovi_positivi[codice_regione==3]
posv<-nuovi_positivi[codice_regione==5]
plot(posv,posp)
model<-lm(posp~posv)
summary(model)
abline(model, col="red", lwd=2)

plot(posp~posl)
model<-lm(posp~posl)
summary(model)
abline(model, col="red", lwd=2)
r<-residuals(model)
hist(r, freq=F)
shapiro.test(r)


##################
######## COSTRUZIONE DI ISTOGRAMMI PER DATI SUDDIVISI IN GRUPPI

dati3<-dati2[denominazione_regione=="Piemonte"|denominazione_regione=="Lombardia"|
      denominazione_regione=="Veneto",]

Hist<-ggplot(dati3, aes(nuovi_positivi, y=..density..))+
  geom_histogram(color="darkgreen",
                 fill="green", binwidth=120)
Hist
# istogramma dei dati completi, quindi non suddivisi per regione

Hist1<-Hist+facet_grid(~denominazione_regione)
Hist1
