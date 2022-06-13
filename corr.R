corr <- function(directory=NULL,threshold=0){
  if(is.null(complete)){
    source("complete.R")
  }
  if(!is.null(directory)){
    setwd(directory)
  }
  
  complete <- complete()
  idlist <- complete$id[complete$nobs>threshold]
  corr <- numeric()
  for(i in idlist){
    if(i<10){
      filename <- paste("00",i,".csv",sep="")
    } else if(i>=10 & i<100){
      filename <- paste("0",i,".csv",sep="")
    } else {
      filename <- paste(i,".csv",sep="")
    }
    data <- read.csv(filename,header=TRUE)
    regr <- lm(nitrate~sulfate,data=data)
    regrsum <- summary(regr)
    if(regrsum$coefficients[2,1]<0){
      s <- -1
    } else {
      s <- 1
    }
    corr <- c(corr,sqrt(regrsum$r.squared)*s)
  }
  corr
}