library(class)
library(gmodels)
library(rpart)

# carico il csv degli esperimenti
csv <- read.csv(file='C:/Users/Sergio/Desktop/uni/tesi/Alimentari - Capsule caffè - Foglio1.csv')

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

# ciclo di creazione dataset, per ogni timestamp dell'esperimento prendo il primo elemento NON vincitore di buybox
dates <- unique(csv$timestamp, incomparables = FALSE)
data_not_buy_box <- NULL
for (x in dates) {
  a <- csv[csv$buy_box==0 & csv$timestamp==x, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box')]
  data_not_buy_box <- rbind(data_not_buy_box, head(a,1))
}

# prendo i vincitori di buybox 1 per timestamp
data_buy_box <- csv[csv$buy_box==1, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box')]

#data_not_buy_box <- csv[csv$buy_box==0, c('num_valutazioni', 'qta_min', 'valutazioni_positive', 'dif_prezzo', 'dif_prezzo_sped', 'dif_prezzo_tot', 'buy_box')]
#data_not_buy_box <- head(data_not_buy_box, nrow(data_buy_box));

# unisco  i 2 dataframe per creare il mio dataset da usar eper il training e per il test
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

names(merge) <- c("Predicted", "Observed")
merge

CrossTable(x = data.testLabels, y = data_pred, prop.chisq=FALSE)

# CART implementato in R da RPART 
data.training <- data[ind==1, 1:7]
data.test <- data[ind==2, 1:7]

model1 <- rpart(buy_box ~., data = data.training, method = "class")

plot(model1)
text(model1, digits = 3)

predicted.classes <- predict(model1, data.test, type = "class")

# Verifica accuratezza modello
mean(predicted.classes == data.test$buy_box)