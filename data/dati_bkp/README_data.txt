## DATA README

## file originali mandati da Bruno via mail
facebook.csv
voti_original_file.xlsx

## elaborazione dati da tesi di Claudia Stocchi
voti_claudia_version.xlsx


## output intermediari dello script 0_dataCleaning.R
voti.csv
voti_long.csv

## dati puliti (voti e connessioni) per tutti gli studenti
voti.rds
networkData.rds

## lista dati per paper
dataForIRTCAR.RData

contiene:
- isolatedStudents	: lista identificativi studenti senza connessioni
- nIsolated      	: numero studenti senza senza connessioni
- nConnected		: numero studenti con connessioni 
- networkMatrix  	: matrice di adiacenza delle amicizie FB tra studenti (nConnected X nConnected)
- studentMapping 	: dataframe con id_studente e id_numerico
				  	  Nota che id_studente e id_numerico sono in un ordine ben preciso, per dividere tra studenti con connessioni e senza
- K 				: matrice contenente il punteggio massimo ottenibile per ciascuna domanda di ciascun appello (appello = righe, domanda = colonne)
- marks				: dataframe contenente i punteggi degli studenti alle domande d'esamte
					- student: id numerico studente
					- y: punteggio (da 1 a K[exam, item])
					- item: id numerico domanda
					- exam: id numerico esame


