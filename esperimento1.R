library(class)
library(gmodels)
library(rpart)
library(randomForest)
library(e1071)

# imposto un seed per il generatore di numeri casuali
set.seed(1234)

# Funzione di normalizzazione
normalize <- function(x) { 
  num <- x - min(x)
  denom <- max(x) - min(x)
  if( denom > 0) {
    return(num/denom)
  } else {
    return(0)
  }
}

#funzione di conversione string to number
string_to_number <- function(x){
  return(as.double(gsub(",", ".", x)))
}

boolean_to_number <- function(x){
  as.integer(as.logical(x))
}

features <- c('num_valutazioni', 'qta_min', #'valutazioni_positive', 
              'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'stelle', 'vend_amazon', 'delta_consegna', 'buy_box')

#funzione per estrazione dataset not buy box
extract_not_buy_box <- function(csv){
  # ciclo di creazione dataset, per ogni timestamp dell'esperimento prendo il primo elemento NON vincitore di buybox
  dates <- unique(csv$timestamp, incomparables = FALSE)
  data_not_buy_box <- NULL
  for (x in dates) {
    a <- csv[csv$buy_box==0 & csv$timestamp==x, features]
    randomRows <- sample(7, 3)
    data_not_buy_box <- rbind(data_not_buy_box, a[randomRows[1],])
    data_not_buy_box <- rbind(data_not_buy_box, a[randomRows[2],])
    data_not_buy_box <- rbind(data_not_buy_box, a[randomRows[3],])
  }
  return(data_not_buy_box)
}

setwd("C:/Users/Sergio/Desktop/uni/tesi/R/")

# carico il csv degli esperimenti
csv_caffe <- read.csv(file='Alimentari - Capsule caffè - Foglio1.csv')
csv_agenda <- read.csv(file='Ufficio - Agenda moleskine - Foglio1.csv')
csv_lampadine <- read.csv(file='Illuminazione - Lampadine Philips - Foglio1.csv')
csv_sport <- read.csv(file='Sport e tempo libero - Smartwatch - Xiaomi Mi Smart Band 6 - Foglio1.csv')

data_not_buy_box = extract_not_buy_box(csv_caffe)
data_not_buy_box <- rbind(data_not_buy_box, extract_not_buy_box(csv_agenda))
data_not_buy_box <- rbind(data_not_buy_box, extract_not_buy_box(csv_lampadine))
data_not_buy_box <- rbind(data_not_buy_box, extract_not_buy_box(csv_sport))

# prendo i vincitori di buybox 1 per timestamp
data_buy_box_caffe <- csv_caffe[csv_caffe$buy_box==1, features]
data_buy_box_agenda <- csv_caffe[csv_caffe$buy_box==1, features]
data_buy_box_lampadine <- csv_lampadine[csv_lampadine$buy_box==1, features]
data_buy_box_sport <- csv_sport[csv_sport$buy_box==1, features]

data_buy_box <- rbind(data_buy_box_caffe, data_buy_box_agenda, data_buy_box_lampadine,data_buy_box_sport)

# unisco  i 2 dataframe per creare il mio dataset da usare per il training e per il test
data <- rbind(data_buy_box, data_not_buy_box)

# converto im valori numerici le colonne che contengono numeri sotto forma di stringa
#data$dif_prezzo <- unlist(lapply(data[3], string_to_number))
data$dif_prezzo <- string_to_number(data$dif_prezzo)
data$dif_prezzo_sped <- string_to_number(data$dif_prezzo_sped)
data$dif_prezzo_tot <- string_to_number(data$dif_prezzo_tot)
data$stelle <-string_to_number(data$stelle)
#data$vend_amazon <- string_to_number(data$vend_amazon)
data$delta_consegna <- string_to_number(data$delta_consegna)

# normalizzo i dati
data$num_valutazioni <- normalize(data$num_valutazioni)
#data$valutazioni_positive <- unlist(lapply(data[3], normalize))
data$dif_prezzo <- normalize(data$dif_prezzo)
data$dif_prezzo_sped <-normalize(data$dif_prezzo_sped)
data$dif_prezzo_tot <- normalize(data$dif_prezzo_tot)
data$stelle <- normalize(data$stelle)
data$vend_amazon <-normalize(data$vend_amazon)
data$delta_consegna <- normalize(data$delta_consegna)



# divido i dati nel sotto insieme di training e di test
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.60, 0.40))

#data.training <- data[ind==1, 1:8]
#data.trainLabels <- data[ind==1,9]
#data.test <- data[ind==2, 1:8]
#data.testLabels <- data[ind==2, 9]

# elaboro il modello utilizzando knn k-Nearest Neighbour Classification
#data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=3)

#merge <- data.frame(data_pred, data.testLabels)

# Verifica accuratezza modello
#mean(data_pred == data.testLabels)

#names(merge) <- c("Predicted", "Observed")
#merge

#CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

# CART implementato in R da RPART 
data.training <- data[ind==1, 1:9]
data.test <- data[ind==2, 1:9]

cart_model <- rpart(buy_box ~., data = data.training, method = "class")

cart_model
plot(cart_model)
text(cart_model, digits = 3)

predicted.cart <- predict(cart_model, data.test, type = "class")

# Verifica accuratezza modello
mean(predicted.cart == data.test$buy_box)


#RANDOM FOREST
data.training <- data[ind==1, 1:9]
data.training$buy_box <- factor(data.training$buy_box)
data.test <- data[ind==2, 1:9]

randomforest_model <- randomForest(buy_box ~., data = data.training)
randomforest_model
predicted.randomforest <- predict(randomforest_model, data.test)

mean(predicted.randomforest == data.test$buy_box)

importance(randomforest_model)

varImpPlot(randomforest_model, sort=T, n.var= 8, main= "", pch=16)


#SVM
svm.model <- svm(buy_box ~ num_valutazioni+vend_amazon, data = data.training, kernel = "linear", gamma = 1, cost = 1)
summary(svm.model)


svm.pred <- predict(svm.model, data.test)
plot(svm.model , data.training[, c('num_valutazioni','vend_amazon','buy_box')])
plot( data.training$num_valutazioni, data.training$vend_amazon, lwd=3 )

mean(svm.pred ==data.test$buy_box)


