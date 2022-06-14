## rankhospital function: to find the specified ranked hospital with the least 30-day mortality RATE
## for the specified outcome.
## You can use `num="best"`to find the best 1 hospital, and `num="worst"` to find
## the worst hospital.
## Warning: This function drops the rows with NA in specified outcomes!!!!!

rankhospital <- function(state=NULL,outcome=c("heart attack","heart failure","pneumonia"),num=1){
  ###### please note that the code below is just similar to that of function "best"
  ###### but the outcome will be different.
  ##0. Read the file
  data_pr <- read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")
  
  ##1. Waring the null:
  if(is.null(state) | is.null(outcome) | is.null(num)){
    stop("Please enter the state and the specified outcome!")
  }
  
  ##2. Check the validity:
  state_check <- unique(data_pr$State)
  match_state <- match(state, state_check, nomatch=NA)
  if(is.na(match_state)){
    stop("invalid state")
  }  ## validation of the state
  
  vec_outcome <- c("heart attack","heart failure","pneumonia")
  match_outc <- match(outcome, vec_outcome,nomatch=NA)
  if(is.na(match_outc)){
    stop("invalid outcome")
  } ## validation of the outcome, here state can be a vector of several outcomes.
  
  ##3. defining the colnames
  data <- data_pr[,c(2,7,11,17,23)] ## a subset included only that will use
  data[,2] <- as.factor(data[,2]) ## state as a factor var.
  options(warn = -1) ##hide the warning of coercion NA to NA
  i <- 3
  while(i <= length(data)){
    data[,i] <- as.numeric(data[,i])
    i <- i+1
  } ## trans. to num.
  options(warn = 1)
  names(data)[3:5] <- c("heart attack","heart failure","pneumonia") ## rename
  
  ##4. split by state and ordering
  data_split <- split(data,data$State)
  data_order <- data_split[[state]][order(data_split[[state]][[outcome]],
                                          data_split[[state]][["Hospital.Name"]],
                                          decreasing = FALSE,na.last = NA),]
  ## here, "data_split[[state]][["Hospital.Name"]]" in "order" function is 
  ## performed as the second ordering column to ensure the hospital names 
  ## being sorted in alphabetical order.
  ###### please note that the code above is just similar to that of function best
  ###### but the outcome will be different.
  
  #test if num is "best"/"worst"
  if(num=="best"){
    nump <- 1
  } else if(num=="worst"){
    nump <- length(data_order[,1])
  } else if(num > length(data_order[,1]) | num < 0){
      return(NA)## if the "num" argument is larger than the number of the hospitals in a state,
      ## return NA.
  } else {
      nump <- num
  }
  
  rank <- 1:length(data_order[,1]) ## give ordered data a rank column
  data_for_print <- data.frame(Hospital.Name=data_order[,1],
                                     Rate=data_order[[outcome]],
                                     Rank=rank)
  result <- data_for_print[data_for_print$Rank==nump,][["Hospital.Name"]]
  ##Find the hospital name
  result
}