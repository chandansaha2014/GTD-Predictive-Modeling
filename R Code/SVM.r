library(klaR)
library(data.table)
library(e1071)

# read the raw input
cols <- c("crit1","crit2","crit3" ,"targtype1","targsubtype1","weaptype1","weapsubtype1","country", "attacktype1","attacktype2" , "attacktype3","attacktype1_txt")
gtd_svm<- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv', select = cols)
attach(gtd_svm)

## Sub set algo through greedy wilks 
gtd_obj <- greedy.wilks(attacktype1 ~ crit1+crit2+crit3+country,data = training_data,niveau=0.2)
gtd_obj
gtd_obj$results
lda(gtd_obj$formula , data = training_data)
# Split data into training and testing set
set.seed(2)
gtd_svm_train = sample(1:NROW(gtd_svm) , NROW(gtd_svm)/5) 
gtd_svm_test = - gtd_svm_train 

training_data = gtd_svm[gtd_svm_train,]
test_data = gtd_svm[gtd_svm_test,]
## Checking frequency of output 
prop.table(table(training_data$attacktype1_txt))

## remove data with NA values 
sub <- subset(rbind(training_data), !is.na(attacktype1))
rm(sub)


svm_fit <- svm(as.factor(attacktype1) ~ country+crit3+crit1+crit2, data = training_data,na.action = na.omit , type = "C")
summary(svm_fit)
plot(svm_fit,training_data,formula = training_data$attacktype1 ~ country+crit3+crit1+crit2)
svm_predict <- predict(svm_fit , test_data)
summary(svm_predict)
table(svm_predict , test_data$attacktype1)


fitted=attributes(predict(svm_fit,training_data,decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,training_data,main="Training Data")
