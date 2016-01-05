
# Read Data  - 1970 -1991

library(ggplot2)
library(dplyr)

cols <- c("iyear","crit1","crit2","crit3" , "attacktype1","targettype1",
          "subtype1","nkill", "nwound") # "attacktype1_txt",,"motive","gname"
gtd_70to91_0615 <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/GTD Files/CSV Dataset/gtd_70to91_0615dist.csv', select = cols)

gtd_with_attacktype <- subset(rbind(gtd_70to91_0615), !is.na(gtd_70to91_0615$attacktype1))
sub <- subset(gtd_70to91_0615 , crit1 == "1")
attach(gtd_70to91_0615)

gtd_70to91_0615$crit1 <- factor(gtd_70to91_0615$crit1)
#levels(gtd_70to91_0615$crit1) <-c("non-political","Political")
#levels(gtd_70to91_0615$crit1)

gtd_70to91_0615$attacktype1_txt <- factor(gtd_70to91_0615$attacktype1_txt)
#levels(gtd_70to91_0615$att) <-c("non-political","Political")
#levels(gtd_70to91_0615$crit1)


prop.table(table(sub$attacktype1_txt))

# ggplot based on attacktype
ggplot(sub , aes(iyear , group = attacktype1_txt)) + 
  geom_bar(badwidth = 1, aes(color = attacktype1_txt , fill = attacktype1_txt ) , alpha = 0.3)+
  xlab("year")+
  ylab("Type of Attack")+
  ggtitle("Type of attack ")

# Checking distribution of the attack type 
plot(density(sub$attacktype1))

## Creating model on J48
data(gtd_70to91_0615)
library(RWeka)
j48_fit <- J48(as.factor(attacktype1)~., data = gtd_70to91_0615 , na.action=NULL)
summary(j48_fit)


predictions <- predict(j48_fit,globalterrorismdb_0615 )
table(predictions , globalterrorismdb_0615$attacktype1)
as.factor(globalterrorismdb_0615$attacktype)
levels(globalterrorismdb_0615$attacktype1) <- factor(globalterrorismdb_0615$attacktype1)
if(require("party", quietly = TRUE)) plot(j48_fit)

## random forest 

library(randomForest)
set.seed(1)
rand_forest_model <- randomForest(as.factor(attacktype1), data = gtd_70to91_0615 , na.rm= TRUE)
summary(rand_forest_model)


                      