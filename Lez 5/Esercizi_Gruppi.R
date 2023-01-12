                    ### ESERCIZI

# 1- E' possibile affermare con un certo livello di significatività che
# le miglia percorse dalle automobili del tipo considerato siano in media pari a 15000?


# H0: mu = 15000
# H1: mu ≠ 15000

t.test(dati$Mileage, mu=15000)
# p-value < 2.2e-16 --> quindi si può rifiutare H0


# H0: mu >= 15000
# H1: mu < 15000
t.test(dati$Mileage, mu=15000, alternative = "less")
# p-value = 1 --> quindi non si può rifiutare H0


# H0: mu <= 15000
# H1: mu > 15000
t.test(dati$Mileage, mu=15000, alternative = "greater")
# p-value < 2.2e-16 --> quindi si può rifiutare H0







# 2- E' possibile affermare con un certo livello di significatività che
# i prezzi medi delle automobili del tipo considerato nel campione con e senza Sound (impianto stereo) siano uguali?


# H0: mu_priceS = mu_priceNS;  mu_priceS - mu_priceNS = 0
# H1: mu_priceS ≠ mu_priceNS

priceS<-Price[dati$Sound==1]
priceNS<-Price[dati$Sound==0]

t.test(priceS, priceNS, mu=0)  
# intervallo di confidenza è tra -4081.028 e -1181.787 -->  non contiene lo zero, quindi il test risulta significativo

t.test(dati$Price~dati$Sound)
# intervallo di confidenza è tra 1181.787 4081.028 




# 3- E' possibile affermare con un certo livello di significatività che
# la distribuzione dei prezzi delle macchine del tipo considerato nel campione sia normale?


# H0: distribuzione prezzi è normale
# H1: distribuzione non è normale

shapiro.test(dati$Price)
# p-value < 2.2e-16 --> quindi si rifiuta H0 --> distribuzione non è normale (lo si vede anche dai grafici qui sotto)


ks.test(dati$Price, "pnorm", mean(dati$Price, na.rm = T), sd(dati$Price, na.rm = T))
# p-value = 4.485e-14 --> quindi si rifiuta H0

hist(dati$Price, col="yellow")
boxplot(dati$Price, col="blue")
qqnorm(dati$Price)
qqline(dati$Price, col="red")







# 4- E' possibile affermare che la presenza del cruise control
# sia indipendente dalla presenza di impianto stereo (Sound) nelle macchine del tipo considerato nel campione?


# H0: Sound è indipendente da Cruise
# H1: c'è dipendenza tra Sound e Cruise

Sound<-as.factor(dati$Sound)
Cruise<-as.factor(dati$Cruise)

tableS_C<-table(Sound, Cruise)
tableS_C

#dopo aver verificato la tabella, posso usare il chi square
chisq.test(tableS_C)
# p-value = 0.01195 --> è maggiore di 0.05 quindi si può rifiutare H0 = c'è dipendenza tra Sound e Cruise














# ESERCIZIO DATASET

# DOMANDE:

# 1 - Eseguire un'analisi statistica descrittiva completa della variabile ZWINT.

# 2 - Si può affermare che il livello di ZWINT medio sia lo stesso per i pazienti con ER 0 oppure 1?

# 3 - Sottoporre a test l'ipotesi che la presenza di nodi (node) sia indipendente dallo stadio della malattia (grade)



# 1
es2<-read.csv2("dataset.csv", header=T)

str(es2$ZWINT)

summary(es2$ZWINT) # vediamo che è var numerica

range(es2$ZWINT, na.rm=T) # range

IQR(es2$ZWINT, na.rm=T) # scarto interquantile

sd(es2$ZWINT, na.rm=T) # deviazione standard

cv<-sd(es2$ZWINT, na.rm=T)/mean(es2$ZWINT, na.rm=T) # coefficente di variazione

hist(es2$ZWINT, freq=F, col="light blue", main="Istogramma di ZWINT", xlab="ZWINT", ylab="Densità approssimata", breaks=30)
boxplot(es2$ZWINT, col="light green", main="Boxplot di ZWINT", ylab="ZWINT")
qqnorm(es2$ZWINT)
qqline(es2$ZWINT, col="red")




# 2

# H0: mu_ZWINT: ER = 0; mu ZWINT: ER = 1
# H0: mu_ZWINT: ER ≠ 0; mu ZWINT: ER ≠ 1
ER0<-es2$ZWINT[es2$ER==0]
ER1<-es2$ZWINT[es2$ER==1]

t.test(ER0, ER1, mu=0)  
# intervallo di confidenza è tra -0.1490144  0.4895859 -->  c'è lo zero, quindi il test non risulta significativo

t.test(es2$ZWINT~es2$ER)
# intervallo di confidenza è tra -0.1490144  0.4895859 






# 3 

# H0: Node è indipendente da Grade
# H1: c'è dipendenza tra Node e Grade

Grade<-as.factor(es2$grade)
Node<-as.factor(es2$node)

tableNG<-table(Node, Grade)
tableNG

#dubbio su applicare chisq o l'altro


