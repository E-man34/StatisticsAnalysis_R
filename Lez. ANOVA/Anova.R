library("readxl")
library("zoo")
library("data.table")
library("ggplot2")

# Scaricare dati nazionali, regionali e provinciali

download.file('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv', destfile='italia.csv')
download.file('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv', destfile='regioni.csv')
download.file('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv',destfile = 'province.csv')

italia <- fread('italia.csv',fill=T)
regioni <- fread('regioni.csv',fill=T)
province <- fread('province.csv',fill=T)

##### PARTIRE DIRETTAMENTE DA QUI SE SIMPORTANO I DATI
# DEL FILE REGION1.CSV CREATO PER ELIMINARE L'ORA

dati2<-read.csv2("Regioni1.csv", header=T)

str(dati2)

dati2$data<-as.factor(dati2$data)
dati2$data1<-as.Date(dati2$data, format="%d/%m/%Y")
str(dati2$data1)




#### DATA PREPARATION 
## Scelta di un singolo periodo (gli ultimi 3 mesi)

data_1<-as.Date("2022-03-01")
data_2<-as.Date("2022-05-31")
tempi<-seq(data_1,data_2, by="days") # consente di creare un vettore di date del periodo specificato
length(tempi) #sapere quanto è lungo

data3<-subset(dati2, dati2$data1>=data_1 & dati2$data1<=data_2) # si crea un dataset nuovo del dataset di partenza(dati2) che contenga solo questo periodo di tempo





                                                  ######################
                                                  ###### ANALISI DELLA VARIANZA
                                                  #####################

# Consideriamo in particolare i dati di nuovi_positivi:
# del Piemonte (codice_regione==1)
# della Lombardia (codice_regione==3)
# del Veneto (codice_regione==5)

Piemonte_pos<-nuovi_positivi[codice_regione==1]
Lombardia_pos<-nuovi_positivi[codice_regione==3]
Veneto_pos<-nuovi_positivi[codice_regione==5]

length(Piemonte_pos)




                                                          ####### DOMANDA 1: ########

# Possiamo dire che la media del numero di nuovi positivi giornalieri
# nelle 3 regioni sia la stessa o ci sono delle differenze tra le medie nelle regioni considerate?

dati_pos<-c(Piemonte_pos, Lombardia_pos, Veneto_pos)
leg_pos<-rep(c("P","L","V"), c(length(Piemonte_pos),length(Lombardia_pos),length(Veneto_pos)))

# Per eseguire l'ANOVA in R è necessario creare un vettore unico con
# tutti i valori numerici e un altro vettore unico con i nomi dei gruppi che corrispondono ai singoli campioni



#### Step 1. TEST DI BARTLETT
# H0: sigma2_Piemonte = sigma2_Veneto = sigma2_Lombardia
# H1: le tre varianze non sono uguali

bartlett.test(dati_pos~leg_pos)
# p-value < 2.2e-16 --> è bassissimo e l'ipotesi H0 va rifiutata, NON potremmo usare l'ANOVA 

#(PROCEDIAMO UGUALMENTE SOLO PER ESERCIZIO)



#### Step 2. Verifica della normalità di ogni campione

qqnorm(Piemonte_pos)
# l'ipotesi di normalità non è verificata (basta che non lo sia
# anche per uno solo dei campioni), quindi NON potremmo usare l'ANOVA.



                                                      # ANOVA 
# H0: mu_pos_P = mu_poS_v = mu_pos_L
# H1: almeno una coppia di medie non siano uguali
anova(lm(dati_pos~leg_pos))
# p-value < 2.2e-16 --> è molto molto piccolo, quindi H0 va rifiutata


summary(lm(dati_pos~leg_pos))
# summary per ottenere i coefficienti corrispondenti ai vari livelli del trattamento e la loro significatività
# ha preso la lombardia come riferimento perchè è la prima in ordine alfabetico


pairwise.t.test(dati_pos, leg_pos, p.adj="bonferroni")
# pairwise test per confrontare le medie a coppie
# con la correzione del p-value per evitare l'amplificazione dell'errore di 1° tipo



# H0: mediana_posP = mediana_posV = mediana_posL
# H1: le mediane non sono tutte uguali per i tre gruppi
kruskal.test(dati_pos~leg_pos)
# p-value < 2.2e-16 --> rifiutiamo H0


pairwise.wilcox.test(dati_pos, leg_pos, p.adj="bonferroni")




tapply(dati_pos,leg_pos,mean,na.rm=T) #per calcolare la media delle 3 regioni
tapply(dati_pos,leg_pos,median,na.rm=T) #per calcolare le mediane delle 3 regioni


# Dopo il test non parametrico può essere utile rappresentare le distribuzioni tramite boxplot affiancati

boxplot(dati_pos~leg_pos, col=rainbow(12))


#rappresentazione dei positivi per regione
plot(Piemonte_pos, col="red", type="l", ylim=c(0,15000))
lines(Veneto_pos, col="blue")
lines(Lombardia_pos, col="green")



                                            ######################
                                            ###### REGRESSIONE LINEARE

####  DOMANDA GIOCATTOLO:
# è possibile pensare all'esistenza di relazioni di tipo lineare tra qualcuna delle variabili comprese nel dataset?

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










                                ####### CONFRONTO DELLE INCIDENZE GIORNALIERE
                                ####### (NUOVI POSITIVI DIVISI PER LA POPOLAZIONE X10^5)

pop_piem<-4433832
pop_ven<-4851181
pop_lomb<-9963188
inc_piem<-Piemonte_pos/pop_piem*10^5
inc_ven<-Veneto_pos/pop_ven*10^5
inc_lomb<-Lombardia_pos/pop_lomb*10^5

plot(inc_piem, col="red", type="l",ylim=c(0,200))
lines(inc_ven, col="blue")
lines(inc_lomb, col="green")

