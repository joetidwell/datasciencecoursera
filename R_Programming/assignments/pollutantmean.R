pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files


  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  path.work <- file.path(directory)
  id.source <- list.files(file.path(path.work))
  id.select <- file.names[as.numeric(gsub(".csv", "", id.source)) %in% id]

  data.select <- read.csv(file.path(path.work, id.select[1]), header=TRUE)
  data.select <- rbind(data.select,
                       do.call(rbind, 
                               lapply(id.select[-1], function(f) {
                                 read.csv(file.path(path.work,f))
                               })))

  mean(data.select[data.select$ID %in% id, pollutant], na.rm=TRUE)
}

pollutantmean("~/Downloads/specdata", "nitrate", 23)