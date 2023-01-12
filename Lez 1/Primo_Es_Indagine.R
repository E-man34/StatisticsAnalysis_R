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


#SELEZIONARE SOLO ALCUNE COLONNE DATASET
Prov <- select(Indagine, "Provincia")
Corsi <- select(Indagine, "Corsi")
R <- select(Indagine, "R")


#PROVINCIA 
Prov <- table(Prov)
head(sort(table(Prov), decreasing = TRUE), n = 30)
Prov <- as.data.frame(Prov) #convertire la tabella in dataframe
Prov <- Prov[order(-Prov$Freq),] #ordinare dataframe in ordine decrescente

ggplot(Prov, aes(x=reorder(Prov,Freq),y=Freq, fill=Prov)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Le province\n", x="Province", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali


#INDAGINE 
Corsi <- table(Corsi)
head(sort(table(Corsi), decreasing = TRUE), n = 30)
Corsi <- as.data.frame(Corsi) #convertire la tabella in dataframe
Corsi <- Corsi[order(-Corsi$Freq),] #ordinare dataframe in ordine decrescente

ggplot(Corsi, aes(x=reorder(Corsi,Freq),y=Freq, fill=Corsi)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Corso statistica\n", x="Corsi", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali


#R 
R <- table(R)
head(sort(table(R), decreasing = TRUE), n = 30)
R <- as.data.frame(R) #convertire la tabella in dataframe
R <- R[order(-R$Freq),] #ordinare dataframe in ordine decrescente

ggplot(R, aes(x=reorder(R,Freq),y=Freq, fill=R)) + #rappresentiamo graficamente i primi 20 valori e mettiamo come assi gli # e la frequenza, ma X avrà gli #  ordinati per frequenza e su Y la frequenza
  geom_bar(stat = "identity", color = "black", show.legend = FALSE)+ 
  labs(title = "Corso R\n", x="Corsi R", y="Frequenza")+
  theme_classic() +  #impostare un tema
  coord_flip() #ordinamento barre orizzontali

