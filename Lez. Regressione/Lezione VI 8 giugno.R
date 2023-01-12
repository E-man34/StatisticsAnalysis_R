####### LEZIONE VI 8 giugno
#######

##################################
# REGRESSIONE LINEARE
# Dataset DATASET

dati<-read.csv2("DATASET.csv", header=T)
attach(dati)

# Domanda: esiste una relazione (lineare) tra il livello di 
# proteina ZWINT e l'età del paziente?

## PASSO 1: scatterplot dei dati campionari

plot(age, ZWINT)
abline(lm(ZWINT~age), col="red")
# sembra esistere un blando andamento di aumento dei valori
# di ZWINT all'aumentare dei valori di età, quindi procediamo
# con l'analisi di una possibile relazione lineare tra 
# le due variabili


## PASSO 2: calcolo e test per il coefficiente di 
# correlazione lineare

cor.test(age, ZWINT)
# il valore del coefficiente campionario r viene restituito
# eseguendo il test (cor è il valore di r)
# rho è il coefficiente di correlazione lineare
# della popolazione

# H0: rho = 0 
# H1: rho != 0
# non rifiutiamo l'ipotesi nulla di assenza di correlazione
# lineare tra age e ZWINT, quindi non ha senso cercare
# una relazione lineare tra le due variabili

# Domanda: esiste una relazione (lineare) tra il livello di proteina ZWINT
# e IGF2?

plot(IGF2, ZWINT)

cor.test(ZWINT, IGF2)
# p-value < 0.0001, procediamo con l'analisi di regressione lineare
# cor=-0.4653218, discreto

## PASSO 3: stima dei coefficienti della retta di regressione
# e loro significatività

# plot(ZWINT~IGF2)
model<-lm(ZWINT~IGF2)
# lm(y ~ x) le variabili sono in ordine!
# lm, linear model, ha come argomento variabile_dipendente~
# variabile_indipendente
summary(model) # per visualizzare gli elementi principali
# dell'output

# Test t per il coefficiente angolare beta_1
# H0: beta_1 = 0  
# H1: beta_1 != 0
# il p-value dovrebbe essere <0.05 per affermare che esista
# una relazione lineare tra le due variabili

# Test t per l'intercetta beta_0
# H0: beta_0 = 0
# H1: beta_0 != 0
# se il p-value dell'intercetta è >0.05 si rielabora
# un modello senza l'intercetta


## PASSO 4: analisi del coefficiente di determinazione R2 e dei
# residui

# R2 dev'essere il più possibile vicino a 1 e i residui devono avere 
# media nulla. Se la taglia del campione è piccola dovrebbero anche
# avere una distribuzione normale e omoschedasticità, ovvero
# varianza uguale per ogni valore della x, per poter valutare la significatività
# dei coefficienti e calcolare l'intervallo di confidenza

res<-residuals(model)
# assegniamo i residui del modello al vettore res

mean(res)
hist(res, freq=F)
shapiro.test(res)

str(model)
model$coefficients
boxplot(model$residuals) # boxplot dei residui

confint(model)
# calcolo degli intervalli di confidenza per 
# i parametri


# Se lo scatterplot è ancora aperto si può rappresentare 
# su di esso la retta di regressione stimata
plot(IGF2,ZWINT)
abline(model, col="red", lwd=2)

plot(model)


## PASSO 5: previsione
# Vogliamo calcolare l'intervallo di previsione per valori di
# IGF2 esterni al range (superiori)

range(IGF2)

newdata<-data.frame(IGF2=seq(9.4,10,0.1))
predict(model,newdata,interval="predict")



