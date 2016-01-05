library(caret)
library(fields)
library(randomForest)
library(e1071)

# read the raw input
cols <- c("crit1","crit2","crit3" ,"targtype1","targsubtype1","weaptype1","weapsubtype1","country", "attacktype1","attacktype2" , "attacktype3")
gtd_rf<- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv', select = cols)
attach(gtd_rf)

# Split data into training and testing set
set.seed(4)
gtd_rf_train = sample(1:NROW(gtd_rf) , NROW(gtd_rf)/5) 
gtd_rf_test = - gtd_rf_train 

rf_training_data = gtd_rf[gtd_rf_train,]
rf_test_data = gtd_rf[gtd_rf_test,]


# Cross table of categorical variable 
table(rf_training_data[,c("attacktype1", "crit1")]) # attack type might need to be changed to factors 
# Comparing Survival Rate and Fare
bplot.xy(rf_training_data$attacktype1 , rf_training_data$crit1)

#attack Level
attack_level <- levels(as.factor(gtd_rf$attacktype1))


# Convert Survived to Factor
rf_training_data$attacktype1 <- factor(rf_training_data$attacktype1)
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
rf_model <- train(attacktype1 ~ crit1, # taking all  variables
               data = rf_training_data, # Use the training dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

rf_test_data$predicted <- predict(rf_model, newdata = rf_test_data)


### Random Forest 

library(randomForest)
set.seed(1)
rand_forest_tr_model <- randomForest(attacktype1~ crit1+crit2+crit3, data = training_data,type = 'classification',importance= TRUE , proximity= TRUE,ntree=30)
summary(rand_forest_tr_model)
importance(rand_forest_tr_model)
print(rand_forest_tr_model)

rand_predict <- predict(rand_forest_tr_model,test_data)
partialPlot(rand_forest_tr_model,training_data,plot = TRUE , x.var = "crit1")

table(test_data$attacktype1, predict(rand_forest_tr_model,test_data))

