pollutantmean <- function(directory,pollutant,id=1:332){
  setwd(directory)
  aggre <- data.frame()
  for(i in id){
    if(i<10){
      filename <- paste("00",i,".csv",sep="")
    } else if(i>=10 & i<100){
      filename <- paste("0",i,".csv",sep="")
    } else {
      filename <- paste(i,".csv",sep="")
    }
    data <- read.csv(filename,header=TRUE)
    aggre <- rbind.data.frame(aggre,data)
  }
  if(pollutant == "sulfate"){
    x=2
  } else if(pollutant == "nitrate") {
    x=3
  } else {
    stop("Wrong pollutant")
  }
  mean2 <- mean(aggre[[x]][!is.na(aggre[[x]])])
  print(mean2)
}