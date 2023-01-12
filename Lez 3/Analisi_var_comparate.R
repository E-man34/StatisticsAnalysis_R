dati<-read.sas7bdat("italia_18.sas7bdat")
dati[dati=="NaN"]<-NA

                                            # ANALISI DI 2 VAR QUALITATIVE CONGIUNTE!

########### TABELLA DI CONTINGENZA (tab a doppio ingresso) = matrice che ha come entries le frequenze assolute congiunte dei livelli delle 2 var considerate corrispondenti alle righe (prima var) e alle colonne (seconda var)

t2<-table(dati$psppsgva,dati$polintr) #psppsgva va nelle righe e polintr nelle colonne
t2 # in ogni cella c'è la frequenza congiunta tra le 2 var

# PROC TRANSPOSE
t3<-t(t2) # per scambiare devo fare un transpose --> con "t()" inverto righe con colonne
t3



margin.table(t3, 1) # frequenze marginali di riga (1 rappresenta le righe)

margin.table(t3,2) # frequenze marginali di colonne (2 rappresenta le colonne)



# "dato un livello della var X, come ci si comporta nella var Y"


prop.table(t3, 1)*100 # frequenze relative di occorrenza del carattere di colonna nelle righe --> si leggono le distribuzioni della var colonna su ogni riga (1 si riferisce alle righe, quindi stiamo cercando la distribuzione della var colonna per le righe)

prop.table(t3, 2)*100 # frequenze relative di occorrenza del carattere di riga nelle colonne (contrario di sopra)





########### TABELLA DI CONTINGENZA per 3 var







## GRAFICI TAB DI CONTINGENZA

barplot(t3, col=rainbow(15), main="Barplot doppio sovrapposto", xlab="psppsgva-polintr", ylab="Frequenze assolute") # in ogni colonna relativa a 1 delle 2 var, c'è la suddivisione in base alle freq dell'altra
barplot(t3, col=rainbow(15), main="Barplot doppio affiancato", beside=T, xlab="psppsgva-polintr", ylab="Frequenze assolute") # barre non sono sovrapposte ma affiancate




                                # ANALISI DI 2 VAR QUANTITATIVE CONGIUNTE!

# dati$nwspol = numero di ore trascorse a leggere news
# dati$netustm = numero di ore trascorse su internet


#posso solo rappresentarle graficamente con uno scatter

plot(dati$netustm, dati$nwspol, main="Ore trascorse su internet e a leggere news", xlab="ore su internet", ylab="ore a leggere news") #per ogni coppia di dati completi, per ogni persona mette in relazione il numero di minuti trascorsi su internet con il num di minuti trascorsi a leggere news






# T-APPLY = comando per fare tabelle 


# media e varianza del tempo a leggere news in base ai 4 livelli di interesse nella politica

tapply(dati$nwspol, dati$polintr, mean, na.rm=T) # con la media vanno SEMPRE rimossi NA
tapply(dati$nwspol, dati$polintr, sd, na.rm=T) # con la varianza vanno SEMPRE rimossi NA
tapply(dati$nwspol, dati$polintr, IQR, na.rm=T) # scarto interquantile


# GRAFICO
boxplot(dati$nwspol~dati$polintr, col=rainbow(10), xlab="Interesse politico", ylab="Tempo lettura news") #boxplot per ogni livello delle var


