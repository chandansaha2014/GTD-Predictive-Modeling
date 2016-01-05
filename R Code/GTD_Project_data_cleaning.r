library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(leaps) # best subset
library(glmnet) # lasso
library(gplots)
library(ROCR)
library(data.table)
library(leaps)

# read the raw input



gtd <- fread('~/Rutgers/Data Analytics 16.137.550.01/Project/Terrrorism Analysis/Regression/GlobalTerrorism-master/gtd_06_to_13.csv')
attach(gtd)
######### filter data for top 6 countries = 70% of full dataset (datafull) ###########
gtd.top6 <- filter(gtd, country_txt=="Iraq" | country_txt=="Pakistan" | country_txt=="Afghanistan" | 
                      country_txt=="India" | country_txt=="Philippines" | country_txt=="Thailand")



rm(gtd)

######### cleaning columns ###########

# loop through columns and add to cols.exclude any column with less than n/2 rows
# where n/2 is total number of rows in a complete column
# cols.exclude = vector for columns to remove - have less than 1/2 the total number of rows
reduce_cols <- function(dat){
  cols.exclude <- c()
  data.cols <- names(dat)
  n <- nrow(dat)
  for (i in 1:length(data.cols)){
    empty.rows <- sum(is.na(dat[[data.cols[i]]]))
    if(n - empty.rows < n/2){
      cols.exclude <- rbind(cols.exclude, data.cols[i])
    }
  }
  return(cols.exclude)
}

# columns to be removed -  based on where there are less than n/2 rows
cols.remove = reduce_cols(gtd.top6)

# columns names in data
gtd.columns = names(gtd.top6)

gtd.top6 = as.data.frame(gtd.top6)

# reduce data based on whether there are more than n/2 rows in column

gtd.small = gtd.top6[, !(gtd.columns %in% (cols.remove))]

#gtd.small <- gtd.top6[,!(names(gtd.top6) %in% cols.remove)]



#rm(gtd.data.small)

#gtd.small = subset(gtd.top6 , select = cols.remove)

# remove any columns with "_txt" - have numerical values already
txt.cols.remove <- names(select(.data = gtd.small , contains("_txt"))) # gtd.small

# remove "_txt" columns
gtd.small <- gtd.small[,!(names(gtd.small) %in% txt.cols.remove)]# gtd.small



# columns with text or other info that may not be needed
other.cols <- c("eventid", "provstate", "city","latitude","longitude","specificity",
                "location","summary","targsubtype1","motive","weapdetail","propcomment","scite1","scite2",
                "dbsource", "target1", "corp1", "nkillter", "nkillus", "nwoundus","nwoundte")
# remove other columns with text or other information thats not needed
gtd.small <- gtd.small[, !(names(gtd.small) %in% other.cols)]

# last 4 columns contain internation info - check that there is sufficient data filled
# -9 = unknown - check that there are at least n/2 knowns in these columns
internat.cols <- names(select(.data = gtd.small, contains("INT_")))



n.rows <- nrow(gtd.small)
int.exclude <- c()
for (i in 1:length(internat.cols) ){
  unknown.rows <- sum(gtd.small[,internat.cols[i]]==-9)
  if(n.rows - unknown.rows < n.rows/2){
    int.exclude <- rbind(int.exclude, internat.cols[i])
  }
}

int.exclude <-c("INT_MISC")

# remove international columns with too many unknowns (-9) rows
gtd.small <- gtd.small[, !(names(gtd.small) %in% int.exclude)]

rm(other.cols,txt.cols.remove , cols.remove)
rm(internat.cols,int.exclude)
######### vectorizing text columns ###########

# vectorize gname column 
gname <- gtd.small$gname
# unique names
unique.gname <- unique(gtd.small$gname)
# match index 
gname.index <- match(gname, unique.gname)
# add to data.small
gtd.small$gname.index <- gname.index
# remove gname, target1 and corp columns (because of characters) 
gtd.small <- gtd.small[,!(names(gtd.small) %in% c("gname"))]
rm(gname.index)
rm(gname)
rm(unique.gname)
rm(unknown.rows)

#@@@ Combining the variables: nkill + nwound = ncasualty
ncasualty <- gtd.small$nkill + gtd.small$nwound
gtd.small$ncasualty <- c(log2(ncasualty))
gtd.small$nkill <- NULL
gtd.small$nwound <- NULL
rm(ncasualty)




# Note: since the test error is so large, the model is better off without these imputed rows

#######################################################################
## Working around NAs and Inf - since glmnet cannot model with them ##
#######################################################################

# Check coloumns have missing values 

#gtd.small[complete.cases(gtd.small),]

na.test <-  function (x) {
  w <- sapply(x, function(x)all(is.na(x)))
  if (any(w)) {
    stop(paste("All NA in columns", paste(which(w), collapse=", ")))
  }
}
na.test (gtd.small)

######### Function to clean NAs ##############

cleaningNAs <- function(dat, i){
  #"natlty" - assuming country of incidence based on source (GTD) documentation
  if (names(dat)[i] == "natlty1"){
    print('here1')
    column <- which(names(dat)%in%c("country"))
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {dat[j,i] = dat[j, column]}}
    print('natlty1 Worked!')
  }
  #"guncertain1" - deleting the rows since not a significant number
  if (names(dat)[i] == "guncertain1"){
    removecols <- c()
    print("here2")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j) }}
    dat <- dat[-c(removecols),]
    print('guncertain1 Worked!')
  }
  #"ishostkid" - deleting the rows since not a significant number
  if (names(dat)[i] == "ishostkid"){
    print("here3")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {dat <- dat[-c(j),]} }
    print('ishostkid Worked!')
  }
  #"ncasualty" - deleting the rows since this is the response variable
  if (names(dat)[i] == "ncasualty"){
    removecols <- c()
    print("here4")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i]) || is.infinite(dat[j,i])) {removecols <- rbind(removecols, j) }}
    dat <- dat[-c(removecols),]
    print('ncasualty Worked!')
  }
  #"weapsubtype1" - deleting the rows since not a significant number, Linear Regression did not give good results
  if (names(dat)[i] == "weapsubtype1"){
    removecols <- c()
    print("here5")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
    dat <- dat[-c(removecols),]
    print('weapsubtype1 Worked!')
  }
  #"nperps" and "nperpcap" columns --- ignoring these cols since metrics prove these are not significant
  if (names(dat)[i] == "nperps"){
    removecols <- c()
    print("here6")
    column <- which(names(dat)%in%c("nperps"))
    dat[,column] <- NULL
    column <- which(names(dat)%in%c("nperpcap"))
    dat[,column] <- NULL
    print('nperps Worked!')
  }
#   #"claimed" - deleting the rows since this is the response variable
#   if (names(dat)[i] == "claimed"){
#     removecols <- c()
#     print("here7")
#     for (j in 1:length(dat[,i])){if (is.na(dat[j,i]) || is.infinite(dat[j,i])) {removecols <- rbind(removecols, j) }}
#     dat <- dat[-c(removecols),]
#     print('Claimed Worked!')
#   }
  return(dat)
}


#@@@ Removing the NAs by calling the function defined above for each column in the data set
NAVector <- c()
flag <- c()
for (i in 1:length(names(gtd.small)))
{
  ratiobefore = 0
  rationow = 0
  countbefore = 0
  countnow = 0
  if (anyNA(gtd.small[,i])) {
    countbefore = sum(is.na(gtd.small[,i]))
    ratiobefore = countbefore/length(gtd.small[,i])
    gtd.small <- cleaningNAs(gtd.small, i)  #calling the function
    countnow = sum(is.na(gtd.small[,i]))
    rationow = countnow/length(gtd.small[,i])
  }
  flag <- c(names(gtd.small)[i], anyNA(gtd.small[,i]), ratiobefore, countbefore, rationow, countnow) 
  NAVector <- rbind(NAVector,flag)
}
colnames(NAVector) <- c("feature","anyNA","ratiobefore","countbefore","rationow","countnow")

rm(countbefore,countnow,ratiobefore,rationow)






















