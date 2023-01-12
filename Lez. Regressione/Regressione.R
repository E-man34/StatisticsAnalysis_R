# REGRESSIONE LINEARE #


dati<-read.csv2("DATASET.csv", header=T)

attach(dati) #per comodità per togliere dati$ ogni volta


# DOMANDA:
# esiste una relazione lineare tra ZWINT ed età del paziente?


#PASSO 1- grafico
plot(age,ZWINT)
abline(lm(age~ZWINT), col="red")

# sembra esistere un blando andamento di aumento dei valori di ZWINT all'aumentare dei valori di età, quindi procediamo
# con l'analisi di una possibile relazione lineare tra le due variabili



## PASSO 2- calcolo e test per il coefficiente di correlazione lineare

cor.test(ZWINT,age)
# cor = 0.114
# H0: rho = 0 
# H1: rho ≠ 0
# non rifiutiamo l'ipotesi nulla di assenza di correlazione lineare tra età e ZWINT
# quindi non ha senso cercare una relazione lineare tra ZWINT e AGE




# DOMANDA 2: esiste una relazione (lineare) tra il livello di proteina ZWINT e IGF2?

#1
plot(IGF2, ZWINT)


#2
cor.test(ZWINT, IGF2)
# cor = -0.465
# p-value < 0.0001, procediamo con l'analisi di regressione lineare



## PASSO 3: stima dei coefficienti della retta di regressione e loro significatività

plot(ZWINT~IGF2)
model<-lm(ZWINT~IGF2)

summary(model)
# Test t per il coefficiente angolare beta_1
# H0: beta_1 = 0  
# H1: beta_1 != 0
# il p-value dovrebbe essere < 0.05 per affermare che esista una relazione lineare tra le due variabili


# Test t per l'intercetta beta_0
# H0: beta_0 = 0
# H1: beta_0 != 0
# se il p-value dell'intercetta è > 0.05 si rielabora un modello senza l'intercetta --> qui però è maggiore di 0.05 (=)



# A TITOLO DI ESEMPIO (perchè qui non va bene visto che intercetta è significativa) si esegue il modello senza intercetta
model1<-lm(ZWINT~IGF2 -1)
summary(model1)
# c'è solo più IGF2 (è sparito intercept)






## PASSO 4: analisi del coefficiente di determinazione R2 e dei residui per calcolare la bontà del modello costruito

# R2 dev'essere il più possibile vicino a 1 e i residui devono avere 
# media nulla. Se la taglia del campione è piccola dovrebbero anche
# avere una distribuzione normale e omoschedasticità, ovvero
# varianza uguale per ogni valore della x, per poter valutare la significatività
# dei coefficienti e calcolare l'intervallo di confidenza

res<-residuals(model)
# assegniamo i residui del modello al vettore res

mean(res) # la media è piccolissima
hist(res, freq=F)
qqnorm(res)
qqline(res, col="red")

str(model)
model$coefficients
boxplot(model$residuals) # boxplot dei residui

confint(model)
# calcolo degli intervalli di confidenza per i parametri






## PASSO 5: PREVISIONE
# Vogliamo calcolare l'intervallo di previsione per valori di ZWINT interni al range
# o esterni ma non troppo al range (maggiori dell'estremo superiore di IGF2)

range(IGF2)
# il range è tra 6.032 e 9.357

newdata<-data.frame(IGF2=seq(9.4,10,0.1)) # seq crea una successione di valori da 9.4 a 10 con incremento di 0.1
# i valori inseriti sono i valori di X in corrispondenza dei quali si vuole stimare i valori di Y con intervallo di previsione
# si prendono i valori subito esterni al range calcolato prima (dopo 9.357)

pr<-predict(model,newdata,interval="predict")
# man mano che ci allontaniamo dal range, il valore della previsione diventa più impreciso, i valori 
# dell'intervallo di previsione tra lwr e upr aumenta perdendo di precisione 

#      fit      lwr      upr
# 1 7.443384 5.961331 8.925437
# 2 7.385976 5.900220 8.871731
# 3 7.328567 5.838950 8.818184
# 4 7.271159 5.777522 8.764796
# 5 7.213751 5.715938 8.711564
# 6 7.156343 5.654198 8.658487
# 7 7.098934 5.592304 8.605565




plot(seq(9.4,10,0.1), pr[,1], type="l", col="red", lwd=2, ylim=c(0,10))
lines(seq(9.4,10,0.1), pr[,2], type="l", col="green", lwd=2, ylim=c(0,10))
lines(seq(9.4,10,0.1), pr[,3], type="l", col="blue", lwd=2, ylim=c(0,10))




