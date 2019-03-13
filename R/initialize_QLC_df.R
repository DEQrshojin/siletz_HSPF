initialize_QLC_df <- function(nOrd, modDF, zero = FALSE) {

  # Pass a vector of the basins in model order and the DF to model on
  nOrd <- data.frame(BAS = as.numeric(nOrd[-1]))
  
  nOrd$ord <- as.numeric(row.names(nOrd)) + 1
  
  nOrd <- nOrd[order(nOrd$BAS), ]
  
  modDF <- modDF[, c(1, nOrd$ord)]
  
  names(modDF) <- c('Date', paste0('Bas', 1 : (length(modDF) - 1)))
  
  retDF <- modDF
  
  if (zero) {retDF[, 2 : length(modDF)] <- 0}

  # Return a DF with same dimensions and correct column order
  return(retDF)

}

