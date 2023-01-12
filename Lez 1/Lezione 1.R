library(rtweet)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(stringr)
library(openxlsx)
library(tuber)
library(syuzhet)
library(topicmodels)
library(tibble)
library(reshape2)
library(wordcloud2)
library(tidyr)
library(readxl)
library(ISwR)
library(UsingR)


##### LEZIONE I
# INTRODUZIONE A R

# R è prima di tutto una calcolatrice

exp(-2)   # e^(-2)

rnorm(20)

# R lavora per vettori e matrici

log(23.45)
# il separatore decimale è il punto

#### Assegnazione in R

x<-rnorm(10000) # assegno all'oggetto x, che è un
  # vettore, l'output del comando a destra 
x  # per stampare nella console l'oggetto creato

plot(x)
abline(h=0, col="red", lwd=2)

# istogramma dei valori x
hist(x, freq=F, col="red")
lines(density(x), col="blue", lwd=2)
x1 <- seq(min(x), max(x), 0.01) #creazione dell'asse X = 
y1 <- dnorm(x1)
lines(x1,y1, col="green", lwd=2.5)

#per calcolare l'area di una porzione dell'istogramma
abline(v=0, lwd=3)
abline(v=1, lwd=3)


####### I VETTORI

weight<-c(60,72,57,90,95,72)
# c: concatenate
height<-c(1.75,1.80,1.65,1.90,1.74,1.91)

length(height) # lunghezza o taglia o dimensione
# del vettore
str(height) # struttura, quindi tipologia della
# variabile

## Operazione tra vettori
# Calcolo BMI = weight/(height^2)
bmi<-weight/(height^2)
bmi
sort(bmi)  # ordina dal min al max
# Nelle operazioni tra vettori R procede elemento
# per elemento

height_5<-c(1.75,1.80,1.65,1.90,1.74)
bmi_n<-weight/(height_5^2)
bmi_n
72/(1.75^2)

height_3<-c(1.75,1.80,1.65)
bmi_2<-weight/(height_3^2)
bmi_2

nuovo_vettore<-sqrt(weight)
nuovo_vettore
# sqrt è la funzione di R che calcola la radice quadrata
# log è la funzione di r che calcola il logaritmo naturale
vettore_logaritmi<-log(weight)
vettore_logaritmi
# operazione fatta su un vettore

m<-1.34  # vettore di taglia 1
weight/m  # dividiamo un vettore di taglia 6 per 
# un vettore di taglia 1, che quindi viene ripetuto
# 6 volte (1 è un sottomultiplo di 6)




### SEQ (sequenza numeri)
# creare il vettore dei valori da 0 a 10 con passo 0.1, quindi di 101 elementi
v1<-seq(0,10,0.1)

# seq è una funzione con 3 argomenti: min, max, passo
v1
# Se la sequenza è di numeri interi consecutivi, si 
# può usare min:max
v2<-10:20
v2



### REP (ripetizione di numeri)

v3<-rep(1:2,c(10,12))
# rep è una funzione con 2 argomenti: il primocontiene gli oggetti da ripetere, il secondo il numero di volte in cui ripetere ognuno
# L'output è un vettore
str(v3)


