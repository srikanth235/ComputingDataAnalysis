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
  for(i in id) {
    data = getmonitor(i, directory)
    data = subset(data, !is.na(data$sulfate) & !is.na(data$nitrate))
    nobs = c(nobs, nrow(data))
  }
  data.frame(id, nobs)
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  dat = complete(directory)
  ids = subset(dat, dat$nobs>=threshold)
  output = numeric()
  if(nrow(ids) > 0 ) {
  
 
  for(i in 1:nrow(ids)) {
    row = ids[i,]
    data = getmonitor(row[1], directory);
    data = subset(data, !is.na(data$sulfate) & !is.na(data$nitrate))
    output = c(output, cor(data[,2], data[,3]))
   }
}
  output
}

head(corr("specdata",150))