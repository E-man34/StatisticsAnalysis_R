library("foreign")

dati<-read.csv2("kuiper.csv", header=T)
dim(dati) # per sapere quanti sono le obs e le var
str(dati)
#attach(dati) --> utile per omettere dati$ prima della var da accedere

table(dati$Doors)


# DOMANDA 1.
# E' possibile affermare con un certo livello di significatività che
# il prezzo medio delle automibili del tipo considerato
# nel campione sia di 20.000 dollari?


##### RISPOSTA 1

# H0; mu = 20000   (mu: prezzo medio nella popolazione)
# H1: mu ≠ 20000


# CONDIZIONI NECESSARIE PER ESEGUIRE IL TEST T:
  # taglia grande oppure distribuzione normale

# n>30, quindi non è necessario verificare la normalità
# della distribuzione della popolazione


t.test(Price, mu=20000)
# p-value = 0.0001261 --> è molto piccolo quindi possiamo rifiutare l'ipotesi nulla a favore di quella alternativa
# se intervallo di confidenza (20658.85-22027.44)non contiene il valore previsto dall'ipotesi H0 (20000), si può rifiutare l'ipotesi H0

# qui non è necessario calcolare il t-test con "greater" o "less" perchè non viene chiesto se l'ipotesi nulla sia 
    # - H0: mu <= 20.000 (greater)
    # - H0: mu >= 20.000 (less)











# DOMANDA 2:

# E' possibile affermare con un certo livello di significatività che i prezzi medi delle automibili del tipo considerato
# nel campione con e senza sedili in pelle siano uguali?



##### RISPOSTA 2

Leather<-as.factor(dati$Leather)

price0<-Price[dati$Leather==0]
price1<-Price[dati$Leather==1]

# H0: mu_price0 = mu_price1;  mu_price0 - mu_price1 = 0
# H1: mu_price0 ≠ mu_price1
# la taglia di n1 > 30 e n2 > 30, quindi non è necessario verificare la distribuzione normale dei 2 campioni



t.test(price0, price1, mu=0)   # mu indica mu_1 - mu_2 --> anche se non è necessario scriverlo quando è = 0
# Test di Welch per il caso in cui le varianze delle 2 popolazioni non si suppongono uguali
# p-value = 1.435e-08, quindi si rifiuta H0

# intervallo di confidenza è tra -4662.164 e -2284.749 --> non contiene lo zero, quindi il test risulta significativo
# perchè c'è una differenza significativa tra le medie (se contenesse lo 0, non si potrebbe dire che c'è differenza
# tra le due medie)


t.test(dati$Price~dati$Leather) # t-test dei prezzi "in base a" (la tilde) sedili in pelle
# se si ha già una variabile dicotomica che divide nei 2 sottocampioni il campione della variabile quantitativa



tapply(dati$Price,dati$Leather,mean,na.rm=T) #calcolare la media per ciascun indice di prezzo
tapply(dati$Price,dati$Leather,var,na.rm=T) #idem per la varianza
tapply(dati$Price,dati$Leather,sd,na.rm=T) #idem per la devz std
# per calcolare singoli indici su sottogruppi

table(dati$Liter)

tapply(dati$Price,dati$Liter,mean,na.rm=T)



# Per verificare l'uguaglianza delle varianze (NON E' INDISPENSABILE):
var.test(price0, price1)
# H0: varianza_price0 = varianza_price1
# H1: varianza_price0 ≠ varianza_price1
# p-value < 2.2e-16, si rifiuta H0 e si esegue il test di Welch

# Per eseguire il t-test nel caso di varianze uguali (qui non si dovrebbe) si usa
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







                                    ####### TEST NON PARAMETRICI ###########

x<-c(2.1,5.2,7.4,1.9,3.1,8.6) # valori del campione

# ranghi(x) = (2,4,5,1,3,6)  numeri d'ordine dei valori dal più piccolo al più grande

# 1.9   2.1   3.1   5.2   7.4   8.6
# 1     2     3     4     5     6

# quindi si avrà ranghi(x) = (2,4,5,1,3,6)

# I test sono del tipo wilcox.test() e hanno gli stessi argomenti dei test t 
# Confrontano le mediane e non le medie delle distribuzioni




                                      # DOMANDA 1 CON TEST NON PARAMETRICO
# H0: mediana = 20.000
# H1: mediana ≠ 20.000
wilcox.test(dati$Price, mu=20000)
# p-value = 0.9695 quindi non si rifiuta H0



# H0: mediana <= 20.000
# H1: mediana > 20.000
wilcox.test(dati$Price, mu=20000, alternative="greater")
# p-value = 0.5153 quindi non si rifiuta H0




# H0: mediana >= 20.000
# H1: mediana < 20.000
wilcox.test(dati$Price, mu=20000, alternative="less")
# p-value = 0.4848 quindi non si rifiuta H0






                                      # DOMANDA 2 CON TEST NON PARAMETRICO
# H0: mediana_price0 = mediana_price1
# H1: mediana_price0 ≠ mediana_price1
wilcox.test(dati$Price~dati$Leather)
# p-value = 0.01524






# TEST PER LA NORMALITA' DI UN CAMPIONE


hist(dati$Mileage, col="yellow") # ad occhio la distribuzione non è normale perchè la coda a sx è più popolata di quella a dx

boxplot(dati$Mileage, col="blue")

qqnorm(dati$Mileage)
qqline(dati$Mileage, col="red")



# TEST SHAPIRO
shapiro.test(Mileage)
# p-value = 1.25e-07 quindi si rifiuta H0



# TEST KOLMOGOROV

ks.test(Mileage, "pnorm", mean(dati$Mileage, na.rm=T), sd(dati$Mileage,na.rm=T))
# p-value = 0.002904 quindi si rifiuta H0

# KOLMOGOROV per confronto tra 2 campioni
ks.test(price0, price1)
# p-value = 2.23e-06 quindi si rifiuta che i campioni siano estratti dalla stessa distribuzione










##### DOMANDA 3

# Possiamo affermare che l'allestimento con sedili in pelle (Leather) è indipendente dalla presenza di cruise control (Cruise)?


###à RISPOSTA 3
Leather<-as.factor(dati$Leather)
Cruise<-as.factor(dati$Cruise)

tlt<-table(Leather, Cruise)
tlt

# le frequenze in ogni cella dovrebbero essere tutte pari almeno a 5
# in quanto il test si basa sull'uso di una statistica asintotica
# e il test non può venire applicato se si hanno delle frequenze più basse, tanto meno con frequenze nulle.
# Si potrebbe usare in alternativa il test esatto di Fisher oppure
# escludere le categorie che hanno prodotto le frequenze nulle
# oppure raggrupare opportunamente le categorie
# Alternativamente si può usare un altro test non basato su chi quadro che è il test di Fisher

#fisher.test(tlt)


chisq.test(tlt)
# H0: Leather è indipendente da Cruise
# H1: c'è dipendenza tra Leather e Cruise
# l'argomento del test chisq.test è una tabella o una matrice

# Risposta: p-value = 0.05617, è al limite con 0.05 quindi non rifiutiamo H0