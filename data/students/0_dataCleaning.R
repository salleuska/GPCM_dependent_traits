################ 
## data cleaning
################
library(igraph)
library(stringr)
library(tidyverse)
library(here)

################
## exam data
################
exam <- read.csv(here("data/voti.csv"), header = TRUE, sep = ",", check.names=F)

## somma domande divise in a, b 

colnames(exam)
colnames(exam)[grep("[a-z]", colnames(exam))]

exam$'20140703-2' <- exam$'20140703-2a' + exam$'20140703-2b'
exam$'20140703-2a' <- NULL
exam$'20140703-2b' <- NULL

exam$'20140915-1' <- exam$'20140915-1a' + exam$'20140915-1b'
exam$'20140915-1a'  <- NULL
exam$'20140915-1b'  <- NULL

exam$'20140915-5' <- exam$'20140915-5a' +exam$'20140915-5b'
exam$'20140915-5a' <- NULL
exam$'20140915-5b' <- NULL

exam$'20160905-2' <- exam$'20160905-2a'  + exam$'20160905-2b'  +  exam$'20160905-2c'
exam$'20160905-2a' <- NULL
exam$'20160905-2b' <- NULL
exam$'20160905-2c' <- NULL

exam$'20170622-4' <- exam$'20170622-4a'  + exam$'20170622-4b' +  exam$'20170622-4c'  + exam$'20170622-d'

exam$'20170622-4a'   <- NULL  
exam$'20170622-4b'   <- NULL  
exam$'20170622-4c'   <- NULL  
exam$'20170622-d'    <- NULL 


exam$"20180123-1" <- exam$"20180123-1a" + exam$"20180123-1b"

exam$"20180123-1a" <- NULL
exam$"20180123-1b" <- NULL

## convert to "long" format - each row is a student

df <- reshape2::melt(exam, id.vars = 1:5)

df$score <- as.numeric(df$value)

df$variable <- as.character(df$variable)

df$appello <- unlist(lapply(str_split(df$variable, c("\\-")), function(x) x[[1]]))
df$domanda <- unlist(lapply(str_split(df$variable, c("\\-")), function(x) x[[2]]))

df$laurea.triennale[which(df$laurea.triennale == "")] <- "stat padova"

df$value <- NULL
df$variable <- NULL

str(df)

colnames(df) <- c('id_studente', 'tipo_laurea_triennale',
				  'voto_lt', 'voto_lm', 'numero_appelli', 
				  'score', 'appello', 'domanda' )

write.table(df, row.names = FALSE, file = here("data/voti_long.csv"))
##########################################
df <- read.table(here("data/voti_long.csv"), header = TRUE)

## punteggi studente per appello
punteggiSA <- df %>% 
    group_by(id_studente, appello, numero_appelli) %>% 
    summarise(tot_score = sum(score, na.rm = TRUE)) %>%
    group_by(id_studente, appello) %>% 
    filter(tot_score> 0) 

## rimuovere righe relative a combinazioni non osservate di studente/appello
## se il punteggio totale all'esame é zero, assumiamo manchi lo studente

data <-  df %>% 
	    group_by(id_studente, appello) %>% 
	    filter(!(sum(score, na.rm = TRUE) == 0)) %>% 
	    ungroup()

## ci sono ancora dei punteggi NA - Bruno non ha inserito uno zero 
data[which(is.na(data$score)),]  
data[which(is.na(data$score)),]$score <- 0

## controllo numero di appelli per studente 
## per controllo 
xx <- punteggiSA %>% group_by(id_studente) %>% 
	mutate(count = n())
plot(xx$numero_appelli, xx$count)

## non coincidono - il numero_appelli inserito da Bruno 
## conta anche esami ritirati?? la mia variabile count
## sembra corretta guardando il file excel originale

## C'é un punteggio totale maggiore di 30
## errore nel file - pratica = 21
punteggiSA[which(punteggiSA$tot_score > 30), ]
## studente 80 - stat BN =P

## Non ci importa al momento dato che consideriamo solo la
## parte teorica


## Rinomiamo l'ultima domanda come pratica per chiarezza

data$domanda <- as.character(data$domanda)
table(data$domanda)
data[data$domanda == "6",]$domanda <- "pratica"
data$domanda

## check number of question per exam
xx <- data %>% 
    group_by(appello) %>%
    summarise(count=length(unique(domanda))) 
print(xx, n = 30)

## appello aveva solo 5 domande 2018082
data[data$domanda == "6",]$domanda <- "pratica"
data[data$appello == 20180829 & data$domanda == "5", ]$domanda 



## arrotondo i punteggi brutalmente
data$score <- round(data$score)

## elimina prova pratica
data <- data[-which(data$domanda == "pratica"), ]

##########################################
library(lubridate)

## Ultimo appello per gli studenti con piú appelli
data <- data %>% 
  mutate(data_appello = ymd(appello)) 

data <- data %>%
  group_by(id_studente) %>%
  filter(data_appello == max(data_appello)) %>%
  ungroup()

# Numero di domande per appello

# data <- readRDS("dati/voti.rds")
data <-  data %>%
        group_by(appello) %>%
        mutate(numero_domande = n_distinct(domanda))  %>%
        ungroup()


saveRDS(data, file = here("data/voti.rds"))
################
## network data
################
netData <- read.csv(here("data/facebook.csv"))
netData$ID.studente

# cbind(netData$ID.studente, 
#     sapply(colnames(netData[, -1]), function(x)
#     strsplit(x, "X")[[1]][2]))

## set colnames equal to ID.students
colnames(netData)[-1]  <- netData$ID.studente

netMatrix <- as.matrix(netData[, -1])
lattice::levelplot(netMatrix)
## Check this is symmetric
isSymmetric.matrix(netMatrix, check.attributes = FALSE)
## 

## Dati rete, rimuovi studenti che non hanno fatto l'esame
## 84 studenti in tutto
toRemove <- setdiff(netData$ID.studente, data$id_studente)

indexToRemove <- which(netData$ID.studente %in% toRemove)


netData <- netData[-indexToRemove, -c(indexToRemove + 1) ]
colnames(netData)[-1] <- as.character(netData$ID.studente)

## Save processed data in rds file
saveRDS(netData, file = here("data/networkData.rds"))
#####################################################
#####################################################
## Update at 16/12/2022
## division between isolated students and connected students

## isolated students id
isolatedStudents <- c("13", "18", "20", "41", "74", "91", "103")

markData <- readRDS(here("data/voti.rds"))
netData <- readRDS(here("data/networkData.rds"))

markData$appello <- as.factor(markData$appello)

## Exams with most students
xx <- markData %>%
  group_by(appello) %>%
    summarise(count=length(unique(id_studente))) 

## remove exam with few students
markData <- droplevels(markData[markData$appello %in% xx$appello[which(xx$count > 2)], ])

## Update isolated students
isolatedStudents <- isolatedStudents[isolatedStudents%in% markData$id_studente]

markData$studentRelationship <- "yes"
markData$studentRelationship[which(markData$id_studente %in% isolatedStudents)] <- "no"

#################
allData <- list()
allData$isolatedStudents <- isolatedStudents

#################
## match network students with mark students
indexNet <- which(netData$ID.studente %in% markData$id_studente)
netData <- droplevels(netData[indexNet, c(1, indexNet + 1)])
## remove isolated students from network
net <- netData[, -1]

########
## remove isolated students from network
indexNet <- !(netData$ID.studente %in% isolatedStudents)
W <- as.matrix(net[indexNet, indexNet])

allData$studentMapping <- data.frame(cbind(studentId = colnames(W), index = 1:length(rownames(W))))
colnames(W) <- 1:length(rownames(W))
rownames(W) <- 1:length(rownames(W))

allData$networkMatrix <- W

#################
## maximum score per question
dd <- markData %>%
    group_by(appello, domanda) %>%
    slice(which.max(score)) %>%
    select(appello, domanda, score)

## convert exam and item in numeric
dd$numericExam <- as.numeric(as.factor(dd$appello))
dd$numericItem <- as.numeric(as.factor(dd$domanda))

dd$maxScore <- dd$score + 1
## dd[which(!dd$appello %in% appelli) , ]$maxScore <- 4


## reshape
allData$K <- as.matrix(reshape2::dcast(dd[, c("numericExam", "numericItem", "maxScore")], numericExam ~ numericItem, value.var = "maxScore"))
allData$K[6, 2] <- 4
allData$K[8, 4] <- 4

# apply(allData$K[, -1], 1, sum)

#################
## sort exam marks by student 

df <- markData[, c("id_studente", "score", "domanda", "appello")]
colnames(df) <- c("student","y", "item", "exam")

## reorder 
df <- rbind(df[!(df$student %in% isolatedStudents),],
    df[(df$student %in% isolatedStudents), ])

allData$nIsolated <- length(isolatedStudents)
allData$nConnected <- length(unique(df$student)) - allData$nIsolated

df$exam <- as.numeric(as.factor(df$exam))
df$item <- as.numeric(as.factor(df$item))

## convert student ids
allData$studentMapping <- rbind(allData$studentMapping , 
                            data.frame(studentId = isolatedStudents, index = (allData$nConnected + 1):(allData$nConnected + length(isolatedStudents))))

numericStudent <- match(df$student, allData$studentMapping$studentId)
df$student <- numericStudent

## convert scores
df$y <- df$y + 1

allData$marks <- df

###########
## Save also complete network 
## first all connected  students, last isolated

## Sort complete network according to data
indexOrder<- order(match(netData$ID.studente,
 allData$studentMapping$studentId))

net <- netData[, -1]
net <- as.matrix(net[indexOrder,  indexOrder ])
dimnames(net) <- list(allData$studentMapping$index, 
allData$studentMapping$index)

allData$networkMatrixAll <- net

save(allData, file = here("data/dataForIRTCAR.RData"))