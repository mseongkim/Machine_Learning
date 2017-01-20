library(rpart)  
library(rpart.plot) 
library(caret) 
library(gplots)
library(e1071)
library(ggplot2)
library(MASS)
rm(list = ls()) 
Housing.Data = Boston
Housing.Data$chas = factor(Housing.Data$chas)
Housing.Data$rad = factor(Housing.Data$rad)
summary(Housing.Data)

#Default Dataset Split
set.seed(1234)
k = sample(1:nrow(Housing.Data), round(nrow(Housing.Data)*0.9))
train.data.default = Housing.Data[k,]
test.data.default = Housing.Data[-k,]

control <- trainControl(method="repeatedcv", number = 10, repeats = 2)
model <-train(medv ~ ., data = train.data.default, method = "rpart", trControl = control)
importance <-varImp(model, scale = FALSE)
plot(importance)

#Conduct a cross Validation method and check mean squared errors
Cross_Validation_Method <- function(Housing.Data,k.fold,start.ratio,end.ratio,intvl){
      size <- (end.ratio)/intvl; index <- 1
      train.error.avg <- as.numeric(size)
      test.error.avg <- as.numeric(size)
  
      for (ratio in seq(start.ratio,end.ratio,intvl))
      {
           Houing.Data.trimmed <- Housing.Data[1:round(nrow(Housing.Data)*ratio),]
           Houing.Data.trimmed <- Houing.Data.trimmed[sample(nrow(Houing.Data.trimmed)),]
           Indicies <- cut(seq(1,nrow(Houing.Data.trimmed)),breaks=k.fold,labels=FALSE)
           train.error <- as.numeric(k.fold)
           test.error <- as.numeric(k.fold)
    
           all.error <- sapply(1:k.fold, function(i){ 
               test.Indicies <- which(Indicies==i,arr.ind=TRUE)
               test.data <- Houing.Data.trimmed[test.Indicies, ]
               train.data <- Houing.Data.trimmed[-test.Indicies, ]
               test.ind.vars <- test.data[,-14]
               train.ind.vars <- train.data[,-14]
      
               search.control <- rpart.control(cp = 0, minsplit = 2, maxdepth = 10)
               train.model <- rpart(medv ~ ., data = train.data, method = "anova", 
                                    control = search.control)
      
               train.medv.hat <- predict(train.model, newdata = train.ind.vars, type = "vector")
               test.medv.hat <- predict(train.model, newdata = test.ind.vars, type = "vector")
      
               #check MSE
               train.error[i] <- mean((train.data$medv-train.medv.hat)^2)
               test.error[i] <- mean((test.data$medv-test.medv.hat)^2)
               all.error <- rbind(train.error[i],test.error[i])
        })
    
       train.error.avg[index] <- mean(all.error[1,])
       test.error.avg[index] <- mean(all.error[2,])
       index <- index + 1
  }
  
       result <- rbind(train.error.avg,test.error.avg)
       result
}

#Choose 10 fold cross validation method. The 'k' can be any number. 
k.fold <- 10; start.ratio <- 0.02; end.ratio <- 0.98; intvl <- 0.02; 
output <- Cross_Validation_Method(Housing.Data, k.fold, start.ratio, end.ratio, intvl)
Training.data.size <- round(nrow(Housing.Data)*seq(start.ratio,end.ratio,intvl)*0.9)

dev.off() 
par(mfrow = c(2,1))
plot(Training.data.size,output[2,])
lines(Training.data.size,output[2,], col = "blue",lwd = 2)
title("Learning Curve")
plot(Training.data.size,output[1,])
lines(Training.data.size,output[1,], col = "green",lwd = 2)
