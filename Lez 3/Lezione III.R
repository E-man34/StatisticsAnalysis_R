####
# LEZIONE III 
####

#### Analisi statistica descrittiva per coppie di variab ili qualitative

## Dataset 


#### Tabella di contingenza

# E' una matrice che ha come entries le frequenze assolute
# congiunte dei livelli delle due variabili onsiderate 
# corrispondenti alle righe (prima variabile) e alle colonne
# (seconda variabile)

# Tabella delle frequenze congiunte 

 
margin.table(t3, 1)   # frequenze marginali di riga
# equivale a table ()

margin.table(t3, 2)   # frequenze marginali di colonna
# equivale a table ()

prop.table(t3, 1)
# frequenze relative di occorrenza del carattere di colonna
# nelle righe

prop.table(t3, 2)
# frequenze relative di occorrenza del carattere di riga
# nelle colonne


## Tabelle di contingenza per tre variabili

# tabella di contingenza separate per le prime due
# variabili, una per ogni livello della terza variabile


## Grafici per tabelle di contingenza

barplot(t3,col=rainbow(15),main="",
        ylim=c(0,150),xlab="node",ylab="Frequenze assolute")

barplot(t4,col=rainbow(15),main="",
        ylim=c(0,80),xlab="grade",ylab="Frequenze assolute")

barplot(t3,col=rainbow(15),main="",
        ylim=c(0,50),xlab="node",
        ylab="Frequenze assolute", beside=T)

barplot(t4,col=rainbow(10),main="",
        ylim=c(0,50),xlab="grade",
        ylab="Frequenze assolute", beside=T)



##########################
# Comandi del tipo apply

# Calcolare media e varianza di  in base ai livelli di 

tapply(, na.rm=T)
tapply(, sd, na.rm=T)
tapply(, length)

#######
# Costruzione di boxplot per dati suddivisi in gruppi
# in base ai livelli di una variabile qualitativa

boxplot(, col=c())


########################
########################


#### STATISTICA INFERENZIALE
#### INTERVALLI DI CONFIDENZA

# Ci interessa stimare la media 

# stima puntuale della media mu 
# nella popolazione

# Calcolo dell'intervallo di confidenza per la media in R

t.test()  # confidenza al 95%

t.test(, conf.level=0.99) # confidenza al 99%
