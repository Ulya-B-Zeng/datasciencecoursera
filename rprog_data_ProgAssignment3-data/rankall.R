## rankall funtion: to find the best "num" (eg. best 15,best 20) hospitals in the
## whole USA.
## You can use `num="best"`to find the best 1 hospital, and `num="worst"` to find
## the worst hospital.
### Warning: This function throws the rows with NA in specified outcomes to the last
## of the rank!!!!!

rankall <- function(outcome=c("heart attack","heart failure","pneumonia"),num=1){
  ##0. Read the file
  data_pr <- read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")
  
  ##1. Waring the null:
  if(is.null(outcome) | is.null(num)){
    stop("Please enter the specified outcome and your desired ranking number!")
  }
  
  ##2. Check the validity:
  vec_outcome <- c("heart attack","heart failure","pneumonia")
  match_outc <- match(outcome, vec_outcome,nomatch=NA)
  if(is.na(match_outc)){
    stop("invalid outcome")
  } ## validation of the outcome.
  
  ##3. defining the colnames
  data <- data_pr[,c(2,7,11,17,23)] ## a subset included only that will use
  options(warn = -1) ##hide the warning of coercion NA to NA
  i <- 3
  while(i <= length(data)){
    data[,i] <- as.numeric(data[,i])
    i <- i+1
  } ## trans. to num.
  options(warn = 1)
  names(data)[3:5] <- c("heart attack","heart failure","pneumonia") ## rename
  
  ##4. ordering
  data_order <- data[order(data[[outcome]],
                           data[["Hospital.Name"]],
                           decreasing = FALSE,na.last = TRUE),]
  ## here, "data[["Hospital.Name"]]" in "order" function is 
  ## performed as the second ordering column to ensure the hospital names 
  ## being sorted in alphabetical order.
  
  #test if num is "best"/"worst"
  if(num=="best"){
    nump <- 1
  } else if(num=="worst"){
    nump <- length(data_order[,1])
  } else if(num > length(data_order[,1]) | num < 0){
    return(NA)## if the "num" argument is larger than the number of the hospitals,
    ## return NA.
  } else {
    nump <- num
  }
  
  rank <- 1:length(data_order[,1]) ## give ordered data a rank column
  data_for_print <- data.frame(hospital=data_order[,1],
                               state=data_order[,2],
                               Rank=rank)
  result <- data_for_print[1:nump,1:2]
  ##Find the hospital name
  result
}