library(caret)
library(RCurl)
library(psych)
library(xts)
library(PerformanceAnalytics)

# dmy <- dummyVars(" ~ .", data = gtd.small , fullRank = F)
# gtdTrsf <- data.frame(predict(dmy,gtd.small))

str(gtd.small)

gtd.feature <- gtd.small[,c("gname.index","target3","country","region","ncasualty","attacktype1")]
# ,"targtype1","natlty1","corp2", "crit1","crit2","crit3",
dmy <- dummyVars(" ~ .", data = gtd.feature , fullRank = F)
gtdTrsf <- data.frame(predict(dmy,gtd.feature))

## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

# Use this to dump the cor.prob output to a 4 column matrix
# with row/column indices, correlation, and p-value.
# See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}


corMasterList <- flattenSquareMatrix (cor.prob(gtdTrsf))
print(head(corMasterList,10))
corList <- corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,10))

