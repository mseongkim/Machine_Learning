library(rpart)  
library(ggplot2)
library(MASS)
library(grid)
library(gridExtra)
rm(list = ls()) 
Housing.Data = Boston
Housing.Data$chas = factor(Housing.Data$chas)
Housing.Data$rad = factor(Housing.Data$rad)
summary(Housing.Data)

Cross_Validation_Method <- function(Housing.Data,k.fold,start.ratio,end.ratio,intvl){
        size <- floor((end.ratio)/intvl); index <- 1
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
                      
                      tree.control <- rpart.control(cp = 0, minsplit = 2, maxdepth = 7)
                      train.model <- rpart(medv ~ ., data = train.data, method = "anova",control = tree.control)
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


k.fold <- 10; start.ratio <- 0.02; end.ratio <- 0.98; intvl <- 0.02; 
output <- Cross_Validation_Method(Housing.Data, k.fold, start.ratio, end.ratio, intvl)
Training.data.size <- round(nrow(Housing.Data)*seq(start.ratio,end.ratio,intvl)*0.9)
output<- rbind(Training.data.size,output)
output <- as.data.frame(t(output))
x_value <- output$Training.data.size
y1_value <- output$test.error.avg
y2_value <- output$train.error.avg 

dev.off() 
p1 <- ggplot() +   
      geom_line(mapping = aes(x_value,y1_value),color = "blue",size = 1,alpha = 0.8) +
      labs(title = "Learning Curve (Maxdepth = 7)", x = "Training Data Size", y = "PMSE (Test)") + theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot() +   
      geom_line(mapping = aes(x_value,y2_value),color = "dark green",size = 1,alpha = 0.8) +
      labs(x = "Training Data Size", y = "PMSE (Training)") 

grid.arrange(p1,p2, nrow = 2)












