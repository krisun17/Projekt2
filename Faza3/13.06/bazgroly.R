library(dplyr)
library(randomForest)
library(FSelector)

brca <- read.table("dane_BRCA.csv", h=T, sep=";", dec=",")

brca %>% 
  
  group_by(ICD10, GENDER, AGE_GROUP, TERYT4) %>%
  
  summarise(Stage1 = sum(Stage1), 
            
            Stage2 = sum(Stage2), 
            
            Stage3 = sum(Stage3), 
            
            Stage4 = sum(Stage4)) -> sum1

sum1$all <- rowSums(sum1[,c(5:8)])

data <- sum1[,c(2,3,9)]

bias <- function(pred, y) {
  return(mean((mean(pred)-y)^2))
}

mse <- function(pred, y) {
  return(mean((pred-y)^2))
}

rmse <- function(pred, y) {
  return(sqrt(mean((pred-y)^2)))
}

r.square <- function(pred, y) {
  tot <- sum((y-mean(y))^2)
  res <- sum((y-pred)^2)
  return(1 - res/tot)
}

set.seed(42)
biases <- c()
vars <- c()
for (nt in seq(5,300,5)) {
  train.ids <- sample(1:nrow(data), (2/3) * nrow(data))
  train <- data[train.ids,]
  test <- data[-train.ids,]
  
  model <- randomForest(all ~ ., train, ntree=nt)
  pred <- predict(model, test[,-3])
  biases <- c(biases, bias(pred, test[,3]))
  vars <- c(vars, var(pred))
}

plot(vars, col="red", type="l")
plot(biases, add=TRUE, col="green", type="l")


############################ eksperymenty #############################

library(randomForest)
library(caret)
library(FSelector)

bias <- function(pred, y) {
  return(mean((mean(pred)-y)^2))
}

mse <- function(pred, y) {
  return(mean((pred-y)^2))
}

r.square <- function(pred, y) {
  tot <- sum((y-mean(y))^2)
  res <- sum((y-pred)^2)
  return(1 - res/tot)
}

# przygotowanie danych 
clean.data <- year10.finito3[,c(1,2,3,8,9,10,11,12,13,14,18,19,21,22,23,24,25,26)]

clean.data$GENDER <- as.numeric(clean.data$GENDER)
colnames(clean.data)[4] <- "dec"
clean.data$dec <- as.numeric(clean.data$dec)

clean.data$Malkohol <- as.numeric(gsub(",", ".", as.vector(clean.data$Malkohol)))
clean.data$Falkohol <- as.numeric(gsub(",", ".", as.vector(clean.data$Falkohol)))
clean.data$Mtyton <- as.numeric(gsub(",", ".", as.vector(clean.data$Mtyton)))
clean.data$Ftyton <- as.numeric(gsub(",", ".", as.vector(clean.data$Ftyton)))

#wybieramy wyrzucamy Ostro³êkê
y10<- y10[y10$TERYT4!=1461,]
y11<- y11[y11$TERYT4!=1461,]
y12<- y12[y12$TERYT4!=1461,]
str(y11)

y10[is.na(y10)] <- 0
y11[is.na(y11)] <- 0
y12[is.na(y12)] <- 0

y10$GENDER <- as.numeric(y10$GENDER)
y11$GENDER <- as.numeric(y11$GENDER)
y12$GENDER <- as.numeric(y12$GENDER)
#y11 <- y11[,-1]
#y12 <- y12[,-1]
colnames(y10)[17] <- "dec"
colnames(y11)[17] <- "dec"
colnames(y12)[17] <- "dec"
y10$dec <- as.numeric(y10$dec)
y11$dec <- as.numeric(y11$dec)
y12$dec <- as.numeric(y12$dec)
y10 <- y10[,-10]
y11 <- y11[,-10]
y12 <- y12[,-10]

females10 <- y10[y10$GENDER == 1, -c(10,12)]
males10 <- y10[y10$GENDER == 2, -c(11,13)]
females11 <- y11[y11$GENDER == 1, -c(10,12)]
males11 <- y11[y11$GENDER == 2, -c(11,13)]
females12 <- y12[y12$GENDER == 1, -c(10,12)]
males12 <- y12[y12$GENDER == 2, -c(11,13)]

#inTraining <- sample(1:nrow(clean.data), (3/4) * nrow(clean.data))
#training <- clean.data[ inTraining,]
#testing  <- clean.data[-inTraining,]

##-- randomForest
rfGrid <-  expand.grid(mtry = seq(5,200,10))

fitControl <- trainControl(method = "cv", number = 10)

nrow(rfGrid)

set.seed(825)
rfFit <- train(dec ~ ., data = females,
               method = "rf",
               trControl = fitControl,
               verbose = TRUE,
               tuneGrid = rfGrid,
               metric="RMSE")
rfFit
## boosting

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = seq(5,200,10),
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit <- train(dec ~ ., data = y11[,-c(8:15)],
                                   method = "gbm",
                                   trControl = fitControl,
                                   verbose = FALSE,
                                   ## Now specify the exact models 
                                     ## to evaluate:
                                     tuneGrid = gbmGrid)
gbmFit

y10$dec<-as.numeric(y10$dec)
str(y10)

y12.ok <- y12[complete.cases(y12),]
preds <- predict(gbmFit, y12.ok[,-c(8:16)])

rmse(preds, y12.ok[,16])

errors <- abs(preds-y12.ok[,16])$dec

y12.ok$errors <- errors
y12.lin.ok <- y12[complete.cases(y12),]
y12.ok$errors_lin <- y12.lin.ok$b2
y12.ok$difs <- y12.ok$errors - y12.ok$errors_lin
library(ggplot2)
plot(errors)
ggplot(data=y12.ok,aes(x=URBANIZACJA,y=errors))+geom_point()
str(y12)
r.square(preds, y12$dec)
# analiza b³êdów

preds <- predict(gbmFit, testing[,-4])
erros <- abs(testing[,4] - preds)

ids <- sort(erros, decreasing=T, index.return=T, )
gbmFit2

# knn
cut.females11 <- data.frame(scale(females11[,-c(1,14)]))
cut.females11$dec <- females11$dec
weights <- rank.correlation(dec ~ ., cut.females11)$attr_importance
weights <- c(weights,1)
knn.females11 <- t( t(cut.females11) * weights)

cut.males11 <- data.frame(scale(males11[,-c(1,14)]))
cut.males11$dec <- males11$dec
weights <- rank.correlation(dec ~ ., cut.males11)$attr_importance
weights <- c(weights, 1)
knn.males11 <- t( t(cut.males11) * weights)

weights <- rank.correlation(dec ~ ., y11)$attr_importance
weights <- c(weights[1:15], 1, weights[16])

knn.y11 <- t( t(y11) * weights)

knnGrid <- expand.grid(k=c(1:20))

knnFit <- train(dec ~ ., data = knn.males11,
                method = "knn",
                trControl = fitControl,
                verbose = FALSE,
                ## Now specify the exact models 
                ## to evaluate:
                tuneGrid = knnGrid)

## svm

svmGrid <-  expand.grid(cost = c(1:10),
                        gamma = c(0.0001, 0.001, 0.01, 0.1, 1))

nrow(svmGrid)

set.seed(825)
svmFit <- train(dec ~ ., data = clean.data,
                method = "svmLinear2",
                trControl = fitControl,
                verbose = FALSE,
                ## Now specify the exact models 
                ## to evaluate:
                tuneGrid = svmGrid)

# enlps

enlpsGrid <-  expand.grid(maxcomp=c(10,12,14,16,18),
                          treshold=c(0.6,0.7,0.8,0.9))

nrow(enlpsGrid)

set.seed(825)
enplsFit <- train(dec ~ ., data = clean.data,
                  method = "enpls.fs",
                  trControl = fitControl,
                  verbose = FALSE,
                  ## Now specify the exact models 
                  ## to evaluate:
                  tuneGrid = enlpsGrid)

# TODO:
# sprawdziæ modele odzielnie dla kobiet i mê¿czyzn (tabele males i females)
# sprawdziæ czy s¹ outliery (tzn obiekty odstaj¹ce bardzo od œredniej) i  ewentualnie je wyrzuciæ
# zrobiæ analizê najwiêkszych b³êdów (czy obiekty daj¹ce najwiêkszy b³¹d tworz¹ jak¹œ specyficzn¹ grupê)