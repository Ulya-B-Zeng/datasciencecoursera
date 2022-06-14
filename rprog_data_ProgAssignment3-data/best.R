## best function: to find the best hospital with the least 30-day mortality RATE
## for the specified outcome.

best <- function(state=state.abb,outcome=c("heart attack","heart failure","pneumonia")){
  ##0. Read the file
  data_pr <- read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")
  
  ##1. Waring the null:
  if(is.null(state) | is.null(outcome)){
    stop("Please enter the states and the specified outcomes!")
  }
  
  ##2. Check the validity:
  state_check <- unique(data_pr$State)
  match_state <- match(state, state_check, nomatch=NA)
  for(i in match_state){
    if(is.na(i)){
    stop("invalid state")
    }
  }  ## validation of the state, here state can be a vector of several states.
  
  vec_outcome <- c("heart attack","heart failure","pneumonia")
  match_outc <- match(outcome, vec_outcome,nomatch=NA)
  for(i in match_outc){
    if(is.na(i)){
      stop("invalid outcome")
    }
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
  colnames(data[,3:5]) <- c("heart attack","heart failure","pneumonia")
  
  ##4. split by state
  data_split <- split(data,data$State)
}