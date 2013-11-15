best <- function(state, out) {
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
  valid_outcomes = c("Heart Attack", "Heart Failure", "Pneumonia")
  result = out %in% valid_outcomes

  if(!result)
    stop("invalid outcome")
  if(nrow(outcome) == 0)
    stop("invalid state")
  
  index  = match(out, valid_outcomes) - 1
  
  outcome[, 11+index*6] = as.numeric(outcome[, 11+index*6])
  outcome = subset(outcome, outcome[,11+index*6]!='NA')
  result=cbind(outcome$Hospital.Name, outcome[,11+index*6])
  
  result = result[do.call(order, lapply(1:NCOL(result), function(i) result[, i])), ]
  result[which.min(result[,2])]
  
  
  
  #outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
  #death <- as.numeric(outcome.hospital[, 11])
  #death
}
