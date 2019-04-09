ro_comp_analysis <- function(roCmp) {

  # roPct is the list of basins with percentage of each component of total (hr)
  roPct <- roCmp
  
  # roAll is a mean of percentage of each component of total flows for all basns
  roAll <- roCmp[[1]]
  
  roAll[, 2 : 4] <- 0
  
  for (i in 1 : length(roPct)) {

    tmp <- rowSums(roCmp[[i]][, 2 : 4])
    
    for (j in 2 : 4) {

      roPct[[i]][, j] <- roCmp[[i]][, j] / tmp
      
      roPct[[i]][which(is.nan(roPct[[i]][, j])), j] <- 0
        
      roAll[, j] <- roAll[, j] + roPct[[i]][, j]
      
    }
  }
  
  for (j in 2 : 4) {roAll[, j] <- roAll[, j] / length(roPct)}
  
  # compress to daily means
  roAll$Date2 <- as.Date(roAll$Date)
  
  roAll <- aggregate(roAll[, 2 : 4], by = list(roAll$Date2), FUN = 'mean')
  
  names(roAll)[1] <- 'Date'
  
  return(roAll)

}
