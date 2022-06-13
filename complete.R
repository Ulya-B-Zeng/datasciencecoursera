complete <- function(directory,id=1:332){
  setwd(directory)
  comp <- data.frame()
  for(i in id){
    if(i<10){
      filename <- paste("00",i,".csv",sep="")
    } else if(i>=10 & i<100){
      filename <- paste("0",i,".csv",sep="")
    } else {
      filename <- paste(i,".csv",sep="")
    }
    data <- read.csv(filename,header=TRUE)
    x <- length(data[[1]][!is.na(data$sulfate) & !is.na(data$nitrate) & !is.na(data$Date) & !is.na(data$ID)])
    comp <- rbind.data.frame(comp,c(i,x))
  }
  colnames(comp) <- c("id","nobs")
  print(comp)
}