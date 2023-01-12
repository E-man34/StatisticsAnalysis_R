####### LEZIONE V 7 giugno
#######

##### Per installare un nuovo pacchetto:
library("foreign")
#####


##### Importazione dei dati dal file kuiper

dati<-read.csv2("kuiper.csv", header=T)
dim(dati)
str(dati)
attach(dati)

table(Doors)


######### TEST STATISTICI DI IPOTESI
########
# Domanda 1.: E' possibile affermare con un certo livello di significatività che
# il prezzo medio delle automibili del tipo considerato
# nel campione sia di 20000 dollari?

# Domanda 2: E' possibile affermare con un certo livello di significatività che
# i prezzi medi delle automibili del tipo considerato
# nel campione con e senza sedili in pelle siano uguali?

# Domanda 3: E' possibile affermare con un certo livello di significatività che
# la distribuzione delle miglia percorse dalle macchine del tipo considerato
# nel campione sia normale?

# Domanda 4: E' possibile affermare che la presenza del cruise control
# sia indipendente dalla presenza di sedili in pelle nelle macchinbe
# del tipo considerato nel campione?

#########

##### RISPOSTA 1

# H0; mu = 20000   (mu: prezzo medio nella popolazione)
# H1: mu != 20000

# n>30, quindi non è necessario verificare la normalità
# della distribuzione della popolazione

t.test(Price, mu=20000)
# p-value = 0.0001261, quindi rifiutiamo l'ipotesi nulla
# 95 percent confidence interval: 20658.85 22027.44
# non contiene il valore previsto da H0, quindi conferma il rifiuto
# Se l'IC per la media NON contiene il valore previsto da H0,
# si rifiuta H0

# H0; mu <= 20000   (mu: prezzo medio nella popolazione)
# H1: mu > 20000  (test a coda destra; il verso è quello di H1)
t.test(Price, mu=20000, alternative="greater")
# p-value = 6.303e-05 ovvero p-value < 0.0001, si rifiuta H0

# H0; mu >= 20000   (mu: prezzo medio nella popolazione)
# H1: mu < 20000  (test a coda sinistra; il verso è quello di H1)
t.test(Price, mu=20000, alternative="less")
# p-value = 0.9999 ovvero p-value >>0.05, NON si rifiuta H0
# NON si ha sufficiente evidenza statistica per rifiutare H0


##### RISPOSTA 2

Leather<-as.factor(Leather)

price0<-Price[Leather==0]
price1<-Price[Leather==1]

# H0: mu_price0 = mu_price1;  mu_price0 - mu_price1 = 0
# H1: mu_price0 != mu_price1

t.test(price0, price1, mu=0)   # mu indica mu_1 - mu_2
# Test di Welch per il caso in cui le varianze delle 2 popolazioni
# non si suppongono uguali
# p-value = 1.435e-08, quindi si rifiuta H0

t.test(Price~Leather)
# se si ha già una variabile dicotomica che divide nei 2 sottocampioni
# il campione della variabile quantitativa

tapply(Price,Leather,mean,na.rm=T)
tapply(Price,Leather,var,na.rm=T)
tapply(Price,Leather,sd,na.rm=T)
# per calcolare singoli indici su sottogruppi

table(Liter)

tapply(Price,Liter,mean,na.rm=T)



# Per verificare l'uguaglianza delle varianze:
var.test(price0, price1)
# H0: varianza_price0 = varianza_price1
# H1: varianza_price0 != varianza_price1
# p-value < 2.2e-16, si rifiuta H0 e si esegue il test di Welch

# Per eseguire il t-test nel caso di varianze uguali (qui non si
# dovrebbe) si usa
t.test(price0, price1, var.equal=T)

#### Test per il confronto tra medie a 1 coda

# H0: mu_price0 <= mu_price1;  mu_price0 - mu_price1 <= 0
# H1: mu_price0 > mu_price1    coda destra

t.test(Price~Leather, mu=0, alternative="greater")   # mu indica mu_1 - mu_2
# Test di Welch per il caso in cui le varianze delle 2 popolazioni
# non si suppongono uguali
# p-value = 0.9999, quindi non si rifiuta


# H0: mu_price0 >= mu_price1;  mu_price0 - mu_price1 <= 0
# H1: mu_price0 < mu_price1    coda sinistra

t.test(Price~Leather, mu=0, alternative="less")   # mu indica mu_1 - mu_2
# Test di Welch per il caso in cui le varianze delle 2 popolazioni
# non si suppongono uguali
# p-value = 7.177e-09, quindi si rìfiuta


##################
####### TEST NON PARAMETRICI

x<-c(2.1,5.2,7.4,1.9,3.1,8.6)
# ranghi(x) = (2,4,5,1,3,6)  numeri d'ordine dei valori dal più piccolo
# al più grande

# I test sono del tipo wilcox.test() e hanno gli stessi argomenti
# dei test t 
# Confrontano le mediane e non le medie delle distribuzioni

##################


##### RISPOSTA 3

#### Verifica grafica di normalità

hist(Mileage, freq=F, col="yellow")
boxplot(Mileage, col="lightblue")
qqnorm(Mileage)
qqline(Mileage, col="red", lwd=3)

#### Verifica di normalità tramite test
# Test di Shapiro-Wilk
shapiro.test(Mileage)
# H0: la distribuzione della popolazione da cui sono estratti i dati 
# del campione è normale
# H1: la distribuzione della popolazione non è normale
# p-value = 1.25e-07 quindi rifiutiamo H0

# Test di Kolmogorov-Smirnov
ks.test(Mileage, "pnorm", mean(Mileage, na.rm=T), sd(Mileage,na.rm=T))
# qui conta l'ordine degli argomenti della funzione


# p-value < 2.2e-16 indica un p-value molto piccolo,
# si potrebbe riportare per esempio p-value<0.0001
# p-value = 0.002904, quindi si rifiuta H0

###### 
# Test di KS per il confronto di due distribuzioni

#### DOMANDA 5
# Possaimo affermare che la distribuzione dei prezzi per le auto con sedili
# in pelle è la stessa di quella dei prezzi per le auto con sedili non in pelle
ks.test(price0, price1)
# H0: i 2 campioni sono estratti dalla stessa distribuzione
# H1: i 2 campioni non sono estratti dalla stessa distribuzione
# p-value = 2.23e-06, si rifiuta l'ipotesi che i campioni siano estratti
# dalla stessa distribuzione


###############
##### RISPOSTA 3

###### Esempio test del chi quadro

# Possiamo affermare che l'allestimento con sedili in pelle è
# indipendente dalla presenza di cruise control?

Leather<-as.factor(Leather)
Cruise<-as.factor(Cruise)

tlt<-table(Leather, Cruise)
tlt

# le frequenze in ogni cella dovrebbero essere tutte pari almeno a 5
# in quanto il test si basa sull'uso di una statistica asintotica
# e il test non può venire applicato se si hanno delle frequenze più
# basse, tanto meno con frequenze nulle
# Si potrebbe usare in alternativa il test esatto di Fisher oppure
# escludere le categorie che hanno prodotto le frequenze nulle
# oppure raggrupare opportunamente le categorie
# Escludiamo la Cadillac


chisq.test(tlt)
# H0: Leather è indipendente da Cruise
# H1: C'è una dipendenza tra Leather e Cruide
# l'argomento del test chisq.test è una tabella o una matrice

# Risposta: p-value = 0.05617, quindi non rifiutiamo l'ipotesi che 
# l'allestimento con cruie control non dipenda dal fatto che ci sia 
# o no l'allestimento con sedili in pelle

detach(dati)

#############
# ESERCIZIO
# Dataset DATASET

# Sottoporre a test l'ipotesi che la presenza di nodi (node) sia indipendente
# dallo stadio della malattia (grade)
#############







