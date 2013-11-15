rankhospital <- function(state, out, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome = subset(outcome, outcome$State==state)
  if(out == "heart attack")
    out = "Heart Attack"
  if(out == "heart failure")
    out = "Heart Failure"
  if(out == "pneumonia")
    out = "Pneumonia"
  if(num=="best")
    num = 1
  
  
  valid_outcomes = c("Heart Attack", "Heart Failure", "Pneumonia")
  result = out %in% valid_outcomes
  
  if(!result)
    stop("invalid outcome")
  if(nrow(outcome) == 0)
    stop("invalid state")
  
  index  = match(out, valid_outcomes) - 1  
  outcome[, 11+index*6] = as.numeric(outcome[, 11+index*6])
  outcome = subset(outcome, outcome[,11+index*6]!='NA')
  #print(nrow(outcome))
  result = cbind(as.numeric(outcome[,11+index*6]), outcome$Hospital.Name)
  result = result[do.call(order, lapply(1:2, function(i) if (i==1) as.numeric(result[, i]) else result[,i])),]
  #result = result[do.call(order, lapply(1:NCOL(result), function(i) result[, i])),]
  
  if(num == "worst")
    num = nrow(outcome)
  
  if(num <= nrow(outcome))
    result[num, 2]
  else
    NA
}

rankhospital("GU", "worst")