# INTERVALLO CONFIDENZA per la media

# tempo di lettura news (nwspol)

mean(dati$nwspol, na.rm=T) #media

# questo test non ha senso perchè l'ipotesi è di media = 0
t.test(dati$nwspol)  # confidenza al 95% (è di default)


t.test(dati$nwspol)$conf.int  # restituire elemento conf.int del test

t.test(dati$nwspol, conf.level=0.99) # confidenza al 99%

t.test(dati$nwspol, conf.level=0.95) # confidenza al 95%

t.test(dati$nwspol, conf.level=0.90) # confidenza al 90%






                                          # ESERCIZIO

# calcolare IC al 95%, 90% e 99% della var numero di ore trascorse su internet (netustm)

mean(dati$netustm, na.rm=T) #media


t.test(dati$netustm)$conf.int  # restituire elemento conf.int del test

t.test(dati$netustm, conf.level=0.99) # confidenza al 99%

t.test(dati$netustm, conf.level=0.95) # confidenza al 95%

t.test(dati$netustm, conf.level=0.90) # confidenza al 90%





# INTERVALLO DI CONFIDENZA per relazione tra 2 var


# rapporto tra 2 varianze = IC * (sigma_prima_pop^2 / sigma_sec_pop^2)
# Test per l'uguaglianza delle varianze delle due variabili

var.test(dati$nwspol, dati$netustm)$conf.int # mette a test l'ipotesi che varianza_prima_pop/varianza_sec_pop = 1 


#Test per l'uguaglianza delle medie delle due variabili
# differenza tra le medie
t.test(dati$nwspol, dati$netustm)$conf.int # mette a test l'ipotesi che varianza_prima_pop/varianza_sec_pop = 1 




### IC per la differenza tra proporzioni
# supponiamo di voler sottoporre a test l'ipotesi che le proporzioni di persone favorevoli a una certa causa siano = in 2 città
# Dobbiamo conoscere la frequenza delle persone favorevoli e la taglia del campione nelle due città

p_favorevoli<-c(250,320) #250 favorevoli nella prima città e 320 favorevoli nella seconda città
taglia_campione<-c(1000,1200) #vettore delle 2 taglie


# ipotesi da verificare --> proporzione prima città = propozione seconda città
prop.test(p_favorevoli,taglia_campione)$conf.int #test sulle proporzioni --> quando lo 0 è compreso nell'intervallo di confidenza, vuol dire che non c'è grossa differenza




### IC per una proporzione
# supponiamo di voler sottoporre a test l'ipotesi che le proporzioni di persone favorevoli in una città.
#Dobbiamo conoscere la frequenza dei favorevoli(x) sul campione totale(n)
prop.test(x=200,n=1000)$conf.int





                                            # ESERCIZIO

# Calcolare IC al 95%, 90% e 99% dei molto interessati alla politica (polintr liv 1)

# 1- devo trovare il numero di casi di liv 1(x)

liv1<-table(dati$polintr)[1]

# 2 - devo trovare il totale dei dati(n)

tot<-sum(!is.na(dati$polintr))

# posso calcolare IC per una proporzione
prop.test(x=liv1, n=tot)$conf.int # 95%
prop.test(x=liv1, n=tot, conf.level=0.90)$conf.int # 90%
prop.test(x=liv1, n=tot, conf.level=0.99)$conf.int # 99%







                                            ## TEST DI IPOTESI


### Vogliamo verificare l'ipotesi che la media (mu) della distribuzione di nwspol sia = a 100 vs ipotesi alternativa che sia != 100 
# H0: mu_nwspol = 100
# H1: mu_nwspol != 100

# il livello di significatività solitamente è il 5%

# Essendo un test per una media useremo il t-test (la taglia del campione è sufficiente) --> test a 2 code
t.test(dati$nwspol, mu=100) #senza mu=100 viene preso il valore di default = a 0
# p-value = 2.2*10^-16 --> per R questo valore è un modo di dire che è piccolissimo 
# p-value così basso significa che c'è evidenza statistica per rifiutare l'ipotesi nulla




#NUOVA IPOTESI DI TEST --> vogliamo verificare l'ipotesi che la media>100
# H0: mu_nwspol <= 100
# H1: mu_nwspol > 100

# t-test a 1 coda (destra)
t.test(dati$nwspol, mu=100, alternative="greater") #verso della coda (dx) va verso l'ipotesi alternativa (> 100)

#anche qui il p-value è piccolo, per cui c'è sufficiente evidenza statistica per rifiutare l'ipotesi nulla che la media <=100




#NUOVA IPOTESI DI TEST --> vogliamo verificare l'ipotesi che la media<100
# H0: mu_nwspol >= 100
# H1: mu_nwspol < 100

# t-test a 1 coda (sinistra)

t.test(dati$nwspol, mu=100, alternative="less")

# qui il p-value = 1, ovvero è alto --> non c'è sufficenza evidenza statistica per rifiutare ipotesi nulla 





                     #T-TEST PER IL CONFRONTO TRA 2 MEDIE --> ipotesi che le medie delle 2 var nwspol e netustm siano uguali
# H0: mu_nwspol = mu_netustm
# H1: mu_nwspol != mu_netustm

t.test(dati$nwspol, dati$netustm) #non ci va la media mu

#p-value = 0.15, quindi non c'è sufficiente evidenza statistica per rifiutare l'ipotesi nulla H0




#NUOVA IPOTESI DI TEST --> vogliamo verificare l'ipotesi che mu_nwspol > mu_netustm
# H0: mu_nwspol <= mu_netustm
# H1: mu_nwspol > mu_netustm

# t-test a 1 coda (destra)
t.test(dati$nwspol, dati$netustm, alternative="greater")

# qui il p-value è di poco maggiore di 0, per cui non c'è sufficiente evidenza statistica per rifiutare ipotesi nulla 




#NUOVA IPOTESI DI TEST --> vogliamo verificare l'ipotesi che mu_nwspol < mu_netustm
# H0: mu_nwspol >= mu_netustm
# H1: mu_nwspol < mu_netustm

# t-test a 1 coda (sinistra)
t.test(dati$nwspol, dati$netustm, alternative="less")

# qui il p-value è circa 1, ovvero è alto --> non c'è sufficenza evidenza statistica per rifiutare ipotesi nulla 


# poichè tutti i p-value non rifiutano, si guarda quello con valore più alto, ovvero quello che non rifiuta l'ipotesi nulla in modo più forte (cioè la coda a sx)


