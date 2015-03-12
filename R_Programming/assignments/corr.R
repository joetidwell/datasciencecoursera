corr <- function(directory, threshold = 1000) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations

  path.work <- file.path(directory)
  id.source <- list.files(file.path(path.work))
  data.select <- read.csv(file.path(path.work, id.source[1]), header=TRUE)
  data.select <- rbind(data.select,
                       do.call(rbind, 
                               lapply(id.source[-1], function(f) {
                                 read.csv(file.path(path.work,f))
                               })))
  data.select <- data.select[!(is.na(data.select$nitrate) |
                               is.na(data.select$sulfate)),]
  out <- lapply(unique(data.select$ID), function(x) {   
    if(nrow(data.select[data.select$ID==x,])>threshold) {
      cor(data.select[data.select$ID==x,]$sulfate, data.select[data.select$ID==x,]$nitrate)
    }
  })
  as.numeric(do.call(c,out))
}
