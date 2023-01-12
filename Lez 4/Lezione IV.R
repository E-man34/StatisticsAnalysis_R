#### LEZIONE IV 
#########################


#######
# Calcolo degli intervalli di confidenza per le medie

t.test() # IC al 95%
t.test(, conf.level=0.90)  # IC al 90%
t.test(, conf.level=0.99)  # IC al 99%

### IC per il rapporto di due varianze
# Test per l'uguaglianza delle varianze delle due variabili

var.test()
# viene fornito l'intervallo di confidenza per il
# rapporto tra le due varianze



### IC per la differenza tra proporzioni
# supponiamo di voler sottoporre a test l'ipotesi che 
# le proporzioni di persone favorevoli a una certa causa
# siano uguali in due città. Dobbiamo conoscere la
# frequenza delle persone favorevoli e la taglia
# del campione nelle due città

p_favorevoli<-c(250,320)
taglia_campione<-c(1000,1200)

prop.test(p_favorevoli,taglia_campione)

# l'intervallo di confidenza che viene fornito è 
# quello per la differenza tra le due proporzioni,
# quella nella prima popolazione da cui è estratto il
# primo campione e quella nella seconda popolazione 
# da cui è estratto il secondo campione


############################
############################
## TEST DI IPOTESI


### Vogliamo verificare l'ipotesi che 

# mu sia la media della distribuzione di 

# H0: 
# H1: 

# il livello di significatività solitamente è il 5%

# Essendo un test per una media useremo il t-test
# (la taglia del campione è sufficiente)


# t-test a coda sinistra

t.test(, alternative="less")



# test a coda destra
# H0: 
# H1: 
t.test(, alternative="greater")




