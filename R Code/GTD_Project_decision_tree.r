library(tree)
library(data.table)
library(RWeka)
library(bit64)

# Read all data 
gtd_all_attribute <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv')
names(gtd_all_attribute)
attach(gtd_all_attribute)

# read the raw input
cols <- c("eventid","crit1","crit2","crit3" ,"targtype1","targsubtype1","weaptype1","weapsubtype1","country", "attacktype1","attacktype2" , "attacktype3","attacktype1_txt")
gtd_decision_tree <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv', select = cols)
gtd_decision_tree$eventid = as.factor(gtd_decision_tree$eventid)
class(gtd_decision_tree$eventid)
# Split data into training and testing set
set.seed(3)
gtd_tree_train = sample(1:NROW(gtd_decision_tree) , NROW(gtd_decision_tree)/2) 
gtd_tree_test = - gtd_tree_train 

training_data = gtd_decision_tree[gtd_tree_train,]
test_data = gtd_decision_tree[gtd_tree_test,]
## Checking frequency of output 
prop.table(table(training_data$attacktype1_txt))


## Creating model on J48

j48_fit <- J48(as.factor(attacktype1)~ crit1+crit2+crit3+country+targtype1+targsubtype1+weapsubtype1+weaptype1  , data = training_data , na.action=NULL)

summary(j48_fit)
j48_fit

# j48_fit <- J48(as.factor(attacktype1)~ .,data = gtd_all_attribute, na.action=NULL)
# 
# summary(j48_fit)

# Prediction on test_data
j48_predictions <- predict(j48_fit,test_data )
summary(j48_predictions)
# showing results in matrix format 
table(j48_predictions , test_data$attacktype1)
# displaying tree
if(require("party", quietly = TRUE)) plot(j48_fit)






# Creating Model  - RPART
dec_tree_control = rpart.control(cp = 0 , minsplit = 200)
rpart_filter_tree = rpart(attacktype1~ crit1+crit2+crit3+country+targtype1+targsubtype1+weapsubtype1+weaptype1 , method = "class" , data = training_data , control = dec_tree_control)

fancyRpartPlot(rpart_filter_tree) # shows tree 
sum(predict(object = rpart_filter_tree,newdata = training_data,type = "class")!=training_data$attacktype1)

## Prediction 
rpart_predicted <- predict(object = rpart_filter_tree,newdata = test_data)
summary(rpart_predicted)

# final <- data.frame(Id = as.data.frame(test_data$eventid) , predicted)
# colnames(final)  <- c("Id",levels(training_data$attacktype1))



