#SMOTE 

library(DMwR)
SMOTE

# Split data into training and testing set
set.seed(2)
gtd.train.smote = sample(1:NROW(gtd) , NROW(gtd)/5) 
gtd.test.smote = - gtd.train.smote 

gtd_training_data = gtd[gtd.train.smote,]
gtd_test_data = gtd[gtd.test.smote,]
## Checking frequency of output 

#Distribution of data 
hist(gtd_training_data$attacktype1,xlab="Attack Type",ylab="Frequency in Dataset",  xlim = c(1,9),main = "Distribution of Attack Type " , col = "lightgreen" , freq = F)

library(gmodels)
freq_table = (CrossTable(gtd_training_data$attacktype1))
print(prop.table(table(gtd_training_data$attacktype1)))
#plot(freq_table$prop.col[,attr],freq_table$prop.row)
#summary(freq_table)
tapply(gtd_training_data$crit1  , gtd_training_data$attacktype1 , sum)



#smote training set 

library(DMwR)
gtd_training_data$attacktype1 = factor(gtd_training_data$attacktype1)
str(gtd_training_data$attacktype1)

gtd_training_data$attacktype1 <- as.factor(gtd_training_data$attacktype1) 
gtd_training_data <- SMOTE(gtd_training_data$attacktype1 ~ iyear+country+region, gtd_training_data, perc.over = 200, perc.under= 100)
gtd_training_data$attacktype1 <- as.numeric(gtd_training_data$attacktype1)
print(prop.table(table(gtd_training_data$attacktype1)))













# model using treebag
# ctrl <- trainControl(method = "cv", number = 5)
# tbmodel <- train(attacktype1 ~ ., data = gtd_training_data, method = "treebag", trControl = ctrl)
# 
# # predict
# predictors <- names(trainSplit)[names(trainSplit) != 'target']
# pred <- predict(tbmodel$finalModel, testSplit[,predictors])
# 
# # score prediction using AUC
# library(pROC)
# auc <- roc(testSplit$target, pred)
# print(auc)



