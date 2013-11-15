getmonitor = function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  if(id<10)
    id = paste("00", as.character(id), sep="")
  else if(id<100)
    id = paste("0", as.character(id), sep="")
  filename = paste(directory,"/",id, ".csv", sep="")
  data = read.csv(filename)
 
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  if(summarize == TRUE)
    data
  data
}

complete = function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs = numeric()
  for(i in id)
    nobs = c(nobs, nrow(getmonitor(i, directory))-1)
  data.frame(id, nobs)
}

getmonitor(3, "specdata", TRUE)

complete("specdata")