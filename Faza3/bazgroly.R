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

females <- clean.data[clean.data$GENDER == 1, -c(3,11,13,15,17)]
males <- clean.data[clean.data$GENDER == 2, -c(3,12,14,16,18)]

inTraining <- sample(1:nrow(clean.data), (3/4) * nrow(clean.data))
training <- clean.data[ inTraining,]
testing  <- clean.data[-inTraining,]

## randomForest
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
gbmFit <- train(dec ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)

# analiza błędów

preds <- predict(gbmFit, testing[,-4])
erros <- abs(testing[,4] - preds)

ids <- sort(erros, decreasing=T, index.return=T, )
gbmFit2

# knn
cut.females <- data.frame(scale(females[,-c(1,3)]))
cut.females$dec <- females$dec
weights <- rank.correlation(dec ~ ., cut.females)$attr_importance
weights <- c(weights, 1)
knn.females <- t( t(cut.females) * weights)

cut.males <- data.frame(scale(males[,-c(1,3)]))
cut.males$dec <- males$dec
weights <- rank.correlation(dec ~ ., cut.males)$attr_importance
weights <- c(weights, 1)
knn.males <- t( t(cut.males) * weights)

knnGrid <- expand.grid(k=c(1:20))

knnFit <- train(dec ~ ., data = knn.males,
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
# sprawdzić modele odzielnie dla kobiet i mężczyzn (tabele males i females)
# sprawdzić czy są outliery (tzn obiekty odstające bardzo od średniej) i  ewentualnie je wyrzucić
# zrobić analizę największych błędów (czy obiekty dające największy błąd tworzą jakąś specyficzną grupę)



