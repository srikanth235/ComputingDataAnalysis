outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[,11])
outcome2=subset(outcome, outcome$State>10)
death = outcome2[, 11]
state = outcome2$State
boxplot(death~state)
hospital <- read.csv("hospital-data.csv", colClasses = "character")
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
death <- as.numeric(outcome.hospital[, 11])

names(outcome)
best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome = subset(outcome, outcome$State==state)
  valid_outcomes = c("Heart Attack", "Heart Failure", "Pneumonia")
  result = outcome %in% valid_outcomes
  if(nrow(outcome) == 0 | !result)
    stop
  
  index  = match(valid_outcomes, outcome) - 1
  outcome.hospital[, 11+index*6] = as.numeric(outcome.hospital[, 11+index*6])
  table(outcome$Hospital.name)
  
  #outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
  #death <- as.numeric(outcome.hospital[, 11])
  #death
}

best("TX", "hear failure")