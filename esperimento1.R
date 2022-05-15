library(class)
library(gmodels)
library(rpart)
library(randomForest)

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

#funzione per estrazione dataset not buy box
extract_not_buy_box <- function(csv){
  # ciclo di creazione dataset, per ogni timestamp dell'esperimento prendo il primo elemento NON vincitore di buybox
  dates <- unique(csv$timestamp, incomparables = FALSE)
  data_not_buy_box <- NULL
  for (x in dates) {
    a <- csv[csv$buy_box==0 & csv$timestamp==x, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box', 'venduto_da')]
    data_not_buy_box <- rbind(data_not_buy_box, head(a,1))
  }
  return(data_not_buy_box)
}

# carico il csv degli esperimenti
csv_caffe <- read.csv(file='C:/Users/Sergio/Desktop/uni/tesi/R/Alimentari - Capsule caffè - Foglio1.csv')
csv_agenda <- read.csv(file='C:/Users/Sergio/Desktop/uni/tesi/R/Ufficio - Agenda moleskine - Foglio1.csv')
csv_lampadine <- read.csv(file='C:/Users/Sergio/Desktop/uni/tesi/R/Illuminazione - Lampadine Philips - Foglio1.csv')
csv_sport <- read.csv(file='C:/Users/Sergio/Desktop/uni/tesi/R/Sport e tempo libero - Smartwatch - Xiaomi Mi Smart Band 6 - Foglio1.csv')


data_not_buy_box = extract_not_buy_box(csv_caffe)
data_not_buy_box <- rbind(data_not_buy_box, extract_not_buy_box(csv_agenda))
data_not_buy_box <- rbind(data_not_buy_box, extract_not_buy_box(csv_lampadine))
data_not_buy_box <- rbind(data_not_buy_box, extract_not_buy_box(csv_sport))

# prendo i vincitori di buybox 1 per timestamp
data_buy_box_caffe <- csv_caffe[csv_caffe$buy_box==1, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box', 'venduto_da')]
data_buy_box_agenda <- csv_caffe[csv_caffe$buy_box==1, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box', 'venduto_da')]
data_buy_box_lampadine <- csv_lampadine[csv_lampadine$buy_box==1, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box', 'venduto_da')]
data_buy_box_sport <- csv_sport[csv_sport$buy_box==1, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box', 'venduto_da')]

data_buy_box <- rbind(data_buy_box_caffe, data_buy_box_agenda, data_buy_box_lampadine,data_buy_box_sport)

# unisco  i 2 dataframe per creare il mio dataset da usare per il training e per il test
data <- rbind(data_buy_box, data_not_buy_box)

# converto im valori numerici le colonne che contengono numeri sotto forma di stringa
data$dif_prezzo <- unlist(lapply(data[4], string_to_number))
data$dif_prezzo_sped <- unlist(lapply(data[5], string_to_number))
data$dif_prezzo_tot <- unlist(lapply(data[6], string_to_number))

# normalizzo i dati
data$num_valutazioni <- unlist(lapply(data[1], normalize))
data$valutazioni_positive <- unlist(lapply(data[3], normalize))
data$dif_prezzo <- unlist(lapply(data[4], normalize))
data$dif_prezzo_sped <- unlist(lapply(data[5], normalize))
data$dif_prezzo_tot <- unlist(lapply(data[6], normalize))

# imposto un seed per il generatore di numeri casuali
set.seed(1234)
# divido i dati ne sotto insieme di training e di test
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.50, 0.50))

data.training <- data[ind==1, 1:6]
data.trainLabels <- data[ind==1,7]
data.test <- data[ind==2, 1:6]
data.testLabels <- data[ind==2, 7]

# elaboro il modello utilizzando knn k-Nearest Neighbour Classification
data_pred <- knn(train = data.training, test = data.test, cl = data.trainLabels, k=3)

merge <- data.frame(data_pred, data.testLabels)

# Verifica accuratezza modello
mean(data_pred == data.testLabels)

#names(merge) <- c("Predicted", "Observed")
#merge

#CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

# CART implementato in R da RPART 
data.training <- data[ind==1, 1:7]
data.test <- data[ind==2, 1:7]

cart_model <- rpart(buy_box ~., data = data.training, method = "class")

plot(cart_model)
text(cart_model, digits = 3)

predicted.cart <- predict(cart_model, data.test, type = "class")

# Verifica accuratezza modello
mean(predicted.cart == data.test$buy_box)


#RANDOM FOREST
data.training <- data[ind==1, 1:7]
data.training$buy_box <- factor(data.training$buy_box)
data.test <- data[ind==2, 1:7]

randomforest_model <- randomForest(buy_box ~., data = data.training)
randomforest_model
predicted.randomforest <- predict(randomforest_model, data.test)

mean(predicted.randomforest == data.test$buy_box)

