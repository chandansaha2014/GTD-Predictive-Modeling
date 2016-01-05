##################################################
## MODELING ##
##################################################

######### splitting into train/test sets #########
set.seed(1010)
ndx <- sample(nrow(gtd.small), floor(nrow(gtd.small) * 0.9))
train <- gtd.small[ndx,]
test <- gtd.small[-ndx,]

# taking numerical cols 
filter_features <- c("iyear","imonth","iday","extended","country","region","vicinity","crit1","crit2",
                     "crit3","doubtterr","multiple","success","suicide","attacktype1","targtype1","natlty1",
                     "guncertain1","claimed","weaptype1","weapsubtype1","ncasualty","property","ishostkid",
                     "INT_MISC","gname.index")  
                   

train <- train[, (names(test) %in% filter_features)]
test <- test[, (names(test) %in% filter_features)]
trainX <- train[!names(test) %in% c("ncasualty")]#,"property" , "ishostkid","claimed","vicinity")]
#trainX <- trainX[, -c("property" , "ihostkid","claimed","vicinity")]
#trainX <- train[, (names(test) %in% filter_features)]
trainY <- train[names(test) %in% ("ncasualty")]
testX <- test[,!names(test) %in% c("ncasualty")]#,"property" , "ishostkid","claimed","vicinity")]
#testX <- test[, (names(test) %in% filter_features)]
testY <- test[,names(test) %in% ("ncasualty")]
rm(train)
rm(test)


######## Lasso Regression ######## - 
cvob1=cv.glmnet(as.matrix(trainX),as.matrix(trainY),alpha=1,family="gaussian")
plot(cvob1)
grid()
#saving image 

setwd("~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/results/")
filename = "lassoLogLambda.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

# best lambda and corresponding coefficients
best_lambda <- cvob1$lambda.min
coef(cvob1)

# glmnet without cv
cvob3=glmnet(as.matrix(trainX),as.matrix(trainY))
plot(cvob3,label=TRUE)
grid()
setwd("~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/results/")
filename = "lassoL1Norm.png"
dev.copy(device = png, filename = filename) # save png
dev.off()


########## Ridge regression ###########
cvob2=cv.glmnet(as.matrix(trainX),as.matrix(trainY),alpha=0)
plot(cvob2)
grid()
filename = "ridgeLogLambda.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

cvob2b=glmnet(as.matrix(trainX),as.matrix(trainY),alpha=0)
plot(cvob2b)
grid()
filename = "ridgeL2Norm.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

# best lambda and corresponding coefficients
best_lambda <- cvob2$lambda.min
coef(cvob2)


########## Linear regression ###########
cvob05=glmnet(as.matrix(trainX),as.matrix(trainY),alpha=0, lambda=0)
coef(cvob05)


#@@@ Lasso Regression on test set
RMSE.test.Lasso <- sqrt(sum((testY-(predict(cvob1,as.matrix(testX))))^2)/length(testY))
RMSE.train.Lasso <- sqrt(sum((trainY-(predict(cvob1,as.matrix(trainX))))^2)/nrow(trainY))
RMSE.test.Lasso
RMSE.train.Lasso

#@@@ Ridge Regression on test set
RMSE.test.RR <- sqrt(sum((testY-(predict(cvob2,as.matrix(testX))))^2)/length(testY))
RMSE.train.RR <- sqrt(sum((trainY-(predict(cvob2,as.matrix(trainX))))^2)/nrow(trainY))
RMSE.test.RR
RMSE.train.RR

#@@@ Linear Regression on test set
RMSE.test.LR <- sqrt(sum((testY-(predict(cvob05,as.matrix(testX))))^2)/length(testY))
RMSE.train.LR <- sqrt(sum((trainY-(predict(cvob05,as.matrix(trainX))))^2)/nrow(trainY))
RMSE.test.LR
RMSE.train.LR


############ Plotting confidence intervals ##############
## Not possible because underlying distribution is not a symmetric normal ##

qplot(x=trainY$ncasualty, geom="histogram", binwidth=0.01) +
  geom_vline(xintercept=mean(trainY$ncasualty), linetype=2, color="red")
n=nrow(testX)
grid()
filename = "SamplingDistribution.png"
dev.copy(device = png, filename = filename) # save png
dev.off()
#LCL <- testY - 1.96*sqrt(RMSE.test/n)
#UCL <- testY + 1.96*sqrt(RMSE.test/n)
#mean(mean(trainY$ncasualty) >= LCL & mean(trainY$ncasualty) <= UCL)


####### Logistic regression on "success" column ##########

#Check whether variables are factor or not 
#l<-sapply(gtd.small,function(x)is.factor(x))

#Factorize all variables 

# To do it for all names
#col_names <- names(gtd.small)
# col_names <- c("iyear","imonth","iday","extended","country","region","vicinity","crit1","crit2",
#                "crit3","doubtterr","multiple","success","suicide","attacktype1","targtype1","natlty1",
#                "guncertain1","claimed","weaptype1","weapsubtype1","property","ishostkid") 
# gtd.small[,col_names] <- lapply(gtd.small[,col_names] , factor)

gtd.small <- gtd.small[, (names(gtd.small) %in% filter_features)]

model <- glm(success ~ ., data=gtd.small[ndx, ], family="binomial") #" not working for all "
table(predict(model, gtd.small[-ndx, ]) > 0, gtd.small[-ndx, "success"])

pred <- prediction(predict(model, gtd.small[-ndx, ]), gtd.small[-ndx, "success"])
perf_lr <- performance(pred, measure='tpr', x.measure='fpr')
plot(perf_lr)
grid()
filename = "ROCcurve.png"
dev.copy(device = png, filename = filename) # save png
dev.off()
aucValue <- performance(pred, 'auc')
aucValue@y.values

########## Subset selection ##########

reg.model <- regsubsets(ncasualty ~ ., data = gtd.small, nvmax = 26) # not running for all , need to do more data cleaning 
reg.summary <- summary(reg.model)

# point which maximizes adjusted rsquared, Cp and BIC - Best Subset Selection
max.adjR <- which.max(reg.summary$adjr2) 
min.cp <- which.min(reg.summary$cp) 
min.bic <- which.min(reg.summary$bic) 
min.rss <- which.min(reg.summary$rss)

# selecting best model based on least rss
coef(reg.model, min.rss)
reg.model.coefs <- coef(reg.model, min.rss)
x.test <- model.matrix(testY~., data = testX)
reg.model.pred <- x.test[, names(coef(reg.model, min.rss))] %*% coef(reg.model, min.rss)
RMSE.test.SS <- sqrt(sum((testY-(reg.model.pred))^2)/length(testY))

## plot adjusted r2
plot(reg.summary$adjr2, xlab = "Size of Subset", ylab = "Adjusted RSq", type ="l", col="blue", main="Best subset selection")
# add point with max RSq
points(max.adjR, reg.summary$adjr2[max.adjR], col="red", cex=2, pch=20) 
grid()

## plot Cp
plot(reg.summary$cp, xlab = "Size of Subset", ylab = "Cp", type ="l", col="blue", main="Best subset selection")
# add point with min Cp
points(max.adjR, reg.summary$cp[min.cp], col="red", cex=2, pch=20) 
grid()

## plot BIC
plot(reg.summary$bic, xlab = "Size of Subset", ylab = "BIC", type ="l", col="blue", main="Best subset selection")
# add point with min BIC
points(min.bic, reg.summary$bic[min.bic], col="red", cex=2, pch=20) 
grid()

# plots - which subset to chose?
# based on adjusted R2
plot(reg.model, scale = "adjr2", main="Adjr2 - subsets",col="pink")
grid()
filename = "adjr2_subsets.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

# based on CP
plot(reg.model, scale = "Cp", main ="Cp - subsets", col="blue")
grid()
filename = "cp_subsets.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

# based on BIC 
plot(reg.model, scale = "bic", main="BIC - subsets", col="snow3")
grid()
filename = "bic_subsets.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

#@@@ plot the RSS for different models 
## plot rss
plot(reg.summary$rss, xlab = "Size of Subset", ylab = "RSS", type ="l", col="blue", main="Best subset selection")
# add point with min RSS
points(min.rss, reg.summary$rss[min.rss], col="red", cex=2, pch=20) 
grid()
# best model according to RSS
print("Best subset based on RSS")



#@@@ VISAL/GABY - Really tried doing this for sometime but it's just so much easier in excel :P
#@@@ Comparing ranks of the coeffs of the two models
SScoef <- coef(reg.model, min.rss)
SScoef <- sort(SScoef,decreasing=TRUE)
Lascoef <- data.frame()
Lascoef <- rbind(names(gtd.small))
Lascoef <- rbind(Lascoef,temp[,1])
Lascoef <- sort(Lascoef[2,], decreasing=TRUE)
rank(SScoef)
rank(Lascoef[2,])




##################################################
## PLOTTING ##
##################################################

# look up countries, attacks
countries.index <- data.frame(country = gtd.top6$country_txt, country.id = gtd.top6$country, stringsAsFactors = F)
attacks.index <- data.frame(attack = gtd.top6$attacktype1_txt, attack.id = gtd.top6$attacktype1, stringsAsFactors = F)

countries.plot <- unique(countries.index[order(countries.index$country.id),])
attacks.plot <- unique(attacks.index[order(attacks.index$attack.id),])

#1) plot of casualties by country
plot.countries <- ggplot(gtd.small, aes(x=country, y = ncasualty)) +
  geom_point(aes(color = country, group = country), size = 2.2) +
  scale_x_discrete(breaks=c(8,86,95,140,160,200),
                   labels=c("Afghanistan", "India", "Iraq", "Pakistan", "Philippines","Thailand"),
                   name = "") +
  scale_y_discrete(name = "Casualties") +
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_text(angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  scale_color_continuous(guide="none") +
  ggtitle('Number of Casualties by Country')
plot.countries
ggsave(filename = "casualties_countries.png", plot = plot.countries, path =".")


#2) plot of casualties by attacktype
plot.attacks <- ggplot(gtd.small, aes(x=attacktype1, y = ncasualty)) +  
  geom_point(aes(color="firebrick", group = attacktype1), size =2.2) +
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9),
                   labels= c("Assassination","Armed\nAssault","Bombing/\nExpolsion","Hijacking",
                             "Hostage \ntaking","Kidnapping","Infra-\nstructure \nAttack",
                             "Unarmed \nAssault","Unknown"), name ="") +
  scale_y_discrete(name = "Casualties") +
  scale_color_discrete(guide="none") +
  # hide tickmarks and rotate labels
  theme(axis.ticks = element_blank()) + 
  theme(axis.title.y = element_text(angle = 90)) +
  theme(axis.title.y = element_text(size = rel(1), angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  ggtitle('Number of Casualties by Attack Type') 
plot.attacks
ggsave(filename = "attacks_casualties.png", plot = plot.attacks, path =".")


#3) plot of casualties by multiple incidents
plot.multiple <- ggplot(gtd.small, aes(x=multiple, y = ncasualty)) +
  geom_point(aes(color = factor(multiple), group = multiple), size =2.3) +
  scale_x_discrete(name = "Multiple Incidents") +
  scale_y_discrete(name = "Casualties") +
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_text(angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  scale_color_discrete(guide="none") +
  ggtitle('Number of Casualties by Multiple Incidents')
plot.multiple
ggsave(filename = "casualties_multiple.png", plot = plot.multiple, path =".")


#4) plot of casualties by day
plot.day <- ggplot(gtd.small, aes(x=iday, y = ncasualty)) +
  geom_point(aes(color = factor(iday), group = iday), size =2) +
  scale_x_discrete(name = "Day of Month") +
  scale_y_discrete(name = "Casualties") + 
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_text(angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  scale_color_discrete(guide="none") + coord_equal() +
  ggtitle('Number of Casualties by Day')
plot.day
ggsave(filename = "casualties_day.png", plot = plot.day, path =".")
