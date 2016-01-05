
library(tree)
library(data.table)

# Read all data 
# gtd_all_attribute <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv')
# names(gtd_all_attribute)
# # read the raw input
#  cols <- c("eventid","crit1","crit2","crit3" , "attacktype1","attacktype2" , "attacktype3","attacktype1_txt")
#  gtd_decision_tree <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/globalterrorismdb_0615dist.csv', select = cols)
#  #political  = ifelse(crit1 == 1 , "Yes" , "No")
#  gtd_decision_tree$eventid = as.factor(gtd_decision_tree$eventid)
#  #gtd_decision_tree = data.frame(gtd_decision_tree,political)
 
 # Split data into training and testing set
 set.seed(2)
 gtd_tree_train = sample(1:NROW(gtd.small) , NROW(gtd.small)/2) 
 gtd_tree_test = - gtd_tree_train  
 
 training_data = gtd.small[gtd_tree_train,]
 #levels(training_data$attacktype1_txt) = as.factor(training_data$attacktype1_txt) 
 test_data = gtd.small[gtd_tree_test,]
 #test_data$eventid = as.numeric(test_data$eventid)
 
 # Create vector for class type 
 #attach(training_data)
 
 
 # fit model using training data 
#  tree_model = tree(political~. , training_data)
#  plot(tree_model)
 # rpart 
 library(rpart)
 library(rpart.plot)
 library(rattle)
 library(RColorBrewer)
 
 # Creating Model 
 dec_tree_control = rpart.control(cp = 0 , minsplit = 200)
 filter_tree = rpart(attacktype1~ . , method = "class" , data = training_data , control = dec_tree_control)
#  filter_tree$cptable
#  
#  cptarg = sqrt(filter_tree$cptable[6,1]*filter_tree$cptable[7,1])
#  prunedtree = prune(filter_tree,cp=cptarg)
#  
#  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
#  plot(prunedtree, uniform = T, compress = T, 
#       margin = 0.1, branch = 0.3)
#  text(prunedtree, use.n = T, digits = 3, cex = 0.6)
 
 

 fancyRpartPlot(filter_tree) # shows tree 
 sum(predict(object = filter_tree,newdata = training_data,type = "class")!=training_data$attacktype1)
 
 predicted <- predict(object = filter_tree,newdata = test_data)
 final <- data.frame(Id = as.data.frame(test_data$iyear) , predicted)
 colnames(final)  <- c("Id",levels(training_data$attacktype1))
 
 
 