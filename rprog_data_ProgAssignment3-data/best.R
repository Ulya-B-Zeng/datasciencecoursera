## best function: to find the best hospital with the least 30-day mortality RATE
## for the specified outcome.
## Warning: This function drops the rows with NA in specified outcomes!!!!!

best <- function(state=NULL,outcome=c("heart attack","heart failure","pneumonia")){
  ##0. Read the file
  data_pr <- read.csv("outcome-of-care-measures.csv",header=TRUE,colClasses="character")
  
  ##1. Waring the null:
  if(is.null(state) | is.null(outcome)){
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
  result <- head(data_order,1,header=FALSE)
  print(result[,1])
}