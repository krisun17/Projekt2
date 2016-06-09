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
  return(mean(sum((mean(pred)-y)^2)))
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


