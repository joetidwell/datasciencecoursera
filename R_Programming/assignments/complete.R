complete <- function(directory, id = 1:332) {
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

  path.work <- file.path(directory)
  id.source <- list.files(file.path(path.work))
  id.select <- file.names[as.numeric(gsub(".csv", "", id.source)) %in% id]
  data.select <- read.csv(file.path(path.work, id.select[1]), header=TRUE)
  data.select <- rbind(data.select,
                       do.call(rbind, 
                               lapply(id.select[-1], function(f) {
                                 read.csv(file.path(path.work,f))
                               })))
  data.select$is.complete <- rowSums(is.na(data.select))==0
  names(data.select)[4:5] <- c("id","n.obs")
  aggregate(n.obs~id, sum, data=data.select)
}

complete("~/Downloads/specdata", 3)