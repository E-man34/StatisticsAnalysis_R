################ LEZIONE 2

##### Installazione di libreria

install.packages("sas7bdat")
library("sas7bdat")


### Vettori di variabili qualitative
# (in R si dicono fattori)

v1<-c("Pippo", "Paperino", "Pluto")
length(v1)
str(v1)  # structure, tipologia dell'oggetto

# Vettori di elementi logici
v2<-rep(c(T,F), c(10,10))
str(v2)
v2

weight<-c(60,72,57,90,95,72)  

height<-c(1.75,1.80,1.65,1.90,1.74,1.91)
str(weight)
str(height)

v3<-(weight>70)
# è un vettore logico che restituisce la risposta
# per ogni elemento di weight
str(v3)

sum(weight)
# somma algebrica degli elementi di weight
sum(weight>70)
# dà il totale delle risposte TRUE
# Infatti, in R TRUE equivale a 1 e FALSE a 0
# sum eseguita su un comando logico fornisce
# il numero di elementi TRUE, cioè che possiedono
# quella caratteristica





################
### Matrici

ls()
x<-rnorm(100)
x
rm(x)  # cancella l'oggetto
x<-rexp(100)
# R sovrascrive sugli oggetti
  

m1<-matrix(1:40, nrow=5)
m1  
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    6   11   16   21   26   31   36
## [2,]    2    7   12   17   22   27   32   37
## [3,]    3    8   13   18   23   28   33   38
## [4,]    4    9   14   19   24   29   34   39
## [5,]    5   10   15   20   25   30   35   40
m2<-matrix(1:40, nrow=5, byrow=T) #di default R riempie per colonne, altrimenti va specificato con byrow
m2 
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    2    3    4    5    6    7    8
## [2,]    9   10   11   12   13   14   15   16
## [3,]   17   18   19   20   21   22   23   24
## [4,]   25   26   27   28   29   30   31   32
## [5,]   33   34   35   36   37   38   39   40
t(m2)
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    9   17   25   33
## [2,]    2   10   18   26   34
## [3,]    3   11   19   27   35
## [4,]    4   12   20   28   36
## [5,]    5   13   21   29   37
## [6,]    6   14   22   30   38
## [7,]    7   15   23   31   39
## [8,]    8   16   24   32   40
x<-rnorm(10)
y<-rnorm(10)
z<-rnorm(10)
m3<-cbind(x,y,z) #incolla le colonne
m3
##                x           y          z
##  [1,] -0.6107913  0.90026533  0.8098301
##  [2,] -1.5363321  0.34567504 -2.2365046
##  [3,]  2.3610081  0.41648350 -1.1820123
##  [4,]  1.3907806 -0.97996730  0.6338609
##  [5,]  1.1246970  0.57116716 -0.9843877
##  [6,] -1.2847755 -0.65509856  0.3824648
##  [7,] -2.0640100  0.26321506  0.7115888
##  [8,] -0.4097408  0.07973312 -0.8298603
##  [9,] -0.8787758 -1.00378439  1.6276111
## [10,] -1.5346221  0.96140320 -0.2709436
m4<-rbind(x,y,z) #incolla le righe
m4
##         [,1]      [,2]       [,3]       [,4]       [,5]       [,6]
## x -0.6107913 -1.536332  2.3610081  1.3907806  1.1246970 -1.2847755
## y  0.9002653  0.345675  0.4164835 -0.9799673  0.5711672 -0.6550986
## z  0.8098301 -2.236505 -1.1820123  0.6338609 -0.9843877  0.3824648
##         [,7]        [,8]       [,9]      [,10]
## x -2.0640100 -0.40974078 -0.8787758 -1.5346221
## y  0.2632151  0.07973312 -1.0037844  0.9614032
## z  0.7115888 -0.82986030  1.6276111 -0.2709436
################
### Liste

l1<-list(g1=x, g2=y, g3=z, g4=m1)

l1$g1  # è un vettore
##  [1] -0.6107913 -1.5363321  2.3610081  1.3907806  1.1246970 -1.2847755
##  [7] -2.0640100 -0.4097408 -0.8787758 -1.5346221
l1$g4
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    6   11   16   21   26   31   36
## [2,]    2    7   12   17   22   27   32   37
## [3,]    3    8   13   18   23   28   33   38
## [4,]    4    9   14   19   24   29   34   39
## [5,]    5   10   15   20   25   30   35   40
l1$g1[1]  # primo elemento del vettore x
## [1] -0.6107913
l1$g1[3:5]
## [1] 2.361008 1.390781 1.124697
l1$g4   # è una matrice
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    6   11   16   21   26   31   36
## [2,]    2    7   12   17   22   27   32   37
## [3,]    3    8   13   18   23   28   33   38
## [4,]    4    9   14   19   24   29   34   39
## [5,]    5   10   15   20   25   30   35   40
dim(l1$g4)  # dimensioni della matrice
## [1] 5 8
l1$g4[5,2] # la matrice ha 2 indici, il primo per la riga e il secondo per la colonna
## [1] 10
l1$g4[1:2,]   # prime 2 righe, tutte le colonne
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    6   11   16   21   26   31   36
## [2,]    2    7   12   17   22   27   32   37
l1$g4[,6:8]  # tutte le righe, ultime 3 colonne
##      [,1] [,2] [,3]
## [1,]   26   31   36
## [2,]   27   32   37
## [3,]   28   33   38
## [4,]   29   34   39
## [5,]   30   35   40
l1$g4[1:3, 4:5] # righe da 1 a 3, colonne 4 e 5
##      [,1] [,2]
## [1,]   16   21
## [2,]   17   22
## [3,]   18   23
# questo metodo di selezione condizionata è utilzzato
# anche per le matrici singole e i dataframe


##### Importazione di file SAS

dati<-read.sas7bdat("italia_18.sas7bdat")
dati[dati=="NaN"]<-"NA"
dim(dati)

# Tempo trascorso nella lettura di news
dati$nwspol<-as.numeric(dati$nwspol)
str(dati$nwspol)

# Tempo trascorso nella consultazione di Internet
dati$netustm<-as.numeric(dati$netustm)
str(dati$netustm)

##### Analisi statistica descrittiva per variabili quantitative
####


# Interesse nella politica (dal maggiore al minore)
dati$polintr<-as.factor(dati$polintr)
str(dati$polintr)

# Implicazione dei cittadini nella politica (dal minore al maggiore)
dati$psppsgva<-as.factor(dati$psppsgva)
str(dati$psppsgva)

##### Analisi statistica descrittiva per variabili qualitative
####


library("ggplot2")
