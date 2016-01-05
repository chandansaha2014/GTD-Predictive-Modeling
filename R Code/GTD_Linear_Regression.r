
########## Linear Regression for weapsubtype1 ###########

plot(lowess(x=gtd.small$weaptype1,y=gtd.small$weapsubtype1))

fit <- data.frame()
removecols <- c()
data.lm <- gtd.small
k = which(names(data.lm)%in%c("weapsubtype1"))

for (j in 1:length(data.lm$weapsubtype1))
  {
  if (is.na(data.lm[j,k])) 
    { 
      removecols <- rbind(removecols, j) 
    } 
  }
data.lm <- data.lm[-c(removecols),]
data.lm.labels <- data.lm[,"weapsubtype1"]
#data.lm$weapsubtype1 <- NULL

num.train <- floor(nrow(data.lm)*0.5)
train.ndx <- sample(1:nrow(data.lm), num.train, replace=F)

data.lm.train <- data.lm[train.ndx, ]
data.lm.train.labels <- data.lm.labels[train.ndx]
data.lm.test <- data.lm[-train.ndx, ]
data.lm.test.labels <- data.lm.labels[-train.ndx]

form <- as.formula(sprintf('weapsubtype1 ~ suicide + weaptype1 + guncertain1 + property + attacktype1 + natlty1 + targtype1')) # + ncasualty + property ))
model <- lm(form, data=data.lm.train)
summary(model)
data.lm.train.labels <- predict(model, data.lm.train)

regplot = function(x,y,...) {
  fit= lm(y~x)
  plot(x,y,...)
  abline(fit , col="red")
  
}
regplot(weapsubtype1,attacktype1,xlab="Weapon Subtype",ylab="AttackType",col="blue",pch=20)

RMSE.test <- sqrt(sum((data.lm.test.labels-(predict(model,data.lm.test)))^2)/length(data.lm.test))
RMSE.train <- sqrt(sum((data.lm.train.labels-(predict(model,data.lm.train)))^2)/nrow(data.lm.train))
RMSE.test
RMSE.train
rm(RMSE.train,RMSE.test , data.lm , data.lm.test , data.lm.train , train.ndx)
# note : model is gathering information of 55% of data . 
